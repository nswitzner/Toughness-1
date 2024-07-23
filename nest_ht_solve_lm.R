## Nested solving of HT

library(readxl)
library(tidyverse)
library(tidymodels)
library(minpack.lm)
library(here)

# Read data in----------------------------------------------------
tim_cvn <-
  read_excel(here(
    "timp_met_charpy_clean_rev02.xlsx"
  ),
  sheet = "Charpy") %>%
  janitor::clean_names() %>%
  rename(
    location = specimen_location,
    temp_f = test_temperature_f,
    charpy_thickness_mm = specimen_thickness_mm,
    cvn_ss_ft_lbs = measured_energy_absorbed_ft_lbs,
    sa_ss = shear_area_percent
  ) %>%
  mutate(
    across(temp_f:sa_ss, as.numeric),
    sa_ss = as.numeric(str_remove(string = sa_ss, pattern = "[<>]")),
    specimen_id_notes = ifelse(id == "1161", "XX", specimen_id_notes),
    specimen_id_notes = ifelse(id == "238", "XX", specimen_id_notes),
    id_notes = glue::glue("{id}{specimen_id_notes}"),
    cvn_fs = cvn_ss_ft_lbs * 10 / charpy_thickness_mm,
    sa_ss = case_when(
      sa_ss == 100 ~ 99,
      sa_ss == 2 ~ 3,
      TRUE ~ sa_ss
    )
  ) %>%
  filter(
    str_detect(location, "Base"),
    # removing weld tests
    # str_detect(single_or_multiple_temperature_test, "Multiple"),
    # remove single temp tests
    sa_ss <= 100,!is.na(temp_f),
    # removing shear areas greater than 100
    bar_orientation == "Transverse (T-L)"
    # limited to cracking in the longitudinal direction
    # that simulates cracking due to hoop stress
  ) %>%
  select(
    id,
    specimen_id_notes,
    id_notes,
    bar_orientation,
    charpy_thickness_mm,
    temp_f,
    cvn_ss_ft_lbs,
    sa_ss,
    cvn_fs
  ) 

## Count how many unique temps
tim_n2plus <- tim_cvn %>%
  select(id_notes, temp_f) %>%
  unique() %>%
  group_by(id_notes) %>%
  count() %>% 
  filter(n > 1)

# id count before filtering
tim_cvn %>%
  count(id_notes) %>%
  arrange(-n) %>%
  left_join(tim_n2plus, by = "id_notes")


# Initial values and function ---------------------------------------------
## Set A = 51 and B = 49 for shear area solution

Ai_sa <- 51 
Bi_sa <- 49 

# initial values for each ID
init_vals <- tim_cvn %>%
  filter(id_notes %in% tim_n2plus$id_notes) %>%
  group_by(id_notes) %>%
  summarise(
    use = max(cvn_fs),
    lse = min(cvn_fs),
    Ai_ae = (use + lse) / 2,
    # mid point b/n us and ls
    Bi_ae = (use + lse) / 2.2,
    # B is slightly smaller than A
    Ci = IQR(temp_f) / 2,
    # 1/2 * IQR
    Di = temp_f[which.min(abs(50 - sa_ss))]
  ) # temp closest to 50% SA

# HT function -------------------------------------------------------------

ht_func <- function(temp_c, A, B, C, D) {
  A + B * tanh((temp_c - D) / C)
}

# k1 function for LM -----------------------------------------------------
k1_func <- function(sa) {
  atanh((sa - Ai_sa) / Bi_sa)
}

# create nested dataframe ------------------------------------
mod_data <- tim_cvn %>%
  filter(id_notes %in% tim_n2plus$id_notes) %>%
  # left_join(init_vals, by = "id_notes") %>% # join initial values
  nest(data = -id_notes) # nest data with initial values

# Shear area models --------------------------------------------
# solve for SATT with error

mod_sa <- mod_data %>%
  mutate(k1 = map(data, ~ k1_func(.$sa_ss))) %>%
  unnest(everything()) %>%
  nest(data = -id_notes) %>%
  mutate(
    mod_salm = map(data, possibly( ~ lm(.$temp_f ~ .$k1), otherwise = NA)),
    tidied_sa = map(mod_salm, possibly(tidy, otherwise = NA)),
    mt = map_vec(tidied_sa, is_empty)
  ) %>%
  filter(mt == FALSE, !is.na(tidied_sa)) %>%
  select(-mt) %>%
  unnest(tidied_sa) %>%
  pivot_wider(
    id_cols = id_notes,
    names_from = term,
    values_from = c(estimate, std.error)
  ) %>%
  left_join(mod_data, by = "id_notes") %>%
  rename(
    D_sa = "estimate_(Intercept)",
    C_sa = "estimate_.$k1",
    d_error = "std.error_(Intercept)",
    c_error = "std.error_.$k1"
  ) %>% 
  filter(!is.na(C_sa))

no_solve <- mod_sa %>% 
  filter(mt==TRUE) %>% 
  unnest(data) %>% 
  select(-c(k1:mt))

no_solve[[2]][[14]] %>% ggplot(aes(temp_f, sa_ss))+geom_point()

# absorbed energy models joined with SA models ---------------------

mod_all <- mod_sa %>%
  unnest(everything()) %>%
  nest(data = -id_notes) %>%
  mutate(k2 = map(data, possibly(~ tanh((.$temp_f - unique(.$D_sa)) /
                                          unique(.$C_sa)
  ), otherwise = NA))) %>%
  unnest(everything()) %>%
  nest(data = -id_notes) %>%
  mutate(mod_aelm = map(data, possibly(~ lm(.$cvn_fs ~ .$k2), otherwise = NA)),
         tidied_ae = map(mod_aelm, tidy)) %>%
  unnest(tidied_ae) %>%
  pivot_wider(
    id_cols = id_notes,
    names_from = term,
    values_from = c(estimate, std.error)
  ) %>%
  left_join(mod_sa, by = "id_notes") %>%
  rename(
    A_ae = "estimate_(Intercept)",
    B_ae = "estimate_.$k2",
    b_error = "std.error_.$k2",
    a_error = "std.error_(Intercept)"
  ) %>%
  select(-data) %>%
  mutate(
    use = A_ae + B_ae,
    satt = C_sa * atanh((85 - 51) / 49) + D_sa,
    use_error = a_error + b_error,
    satt_error = c_error * atanh((85 - 51) / 49) + d_error,
    across(A_ae:satt_error, ~ na_if(.x, NaN))
  )
# use this line if you want to include the original data
# left_join(mod_data, by = "id_notes")


# write results to csv

# filter checks -----------------------------------------------------------

mod_all %>%
  filter(use_error < use / 2 | satt_error < satt / 2) %>%
  write_csv("two_param_solutions.csv")

# Single parameter model --------------------------------------------------


