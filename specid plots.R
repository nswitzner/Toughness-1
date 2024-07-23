library(readxl)
library(tidyverse)
library(here)
library(tidymodels)

## tim cvn ----------------------------------------------------------------

tim_cvn <- read_csv(here("data","timp_met.csv"),
                      na =c("-----","UNK","N/A","NA")
) %>%
  janitor::clean_names() %>%
  dplyr::rename(
    location = charpy_specimen_location,
    temp_f = charpy_test_temperature_f,
    ct_mm = specimen_thickness_mm,
    ae_ss = measured_energy_absorbed_ft_lbs,
    sa_ss = shear_area_percent
  ) %>%
  mutate(
    sa_ss = str_remove(string = sa_ss, pattern = "[<>]"),
    across(temp_f:sa_ss, as.numeric),
    year = as.integer(installation_year),
    #glue together the report id with the specimen id notes to distinguish between different pipe features from one report.
    bar_orientation = str_split_i(charpy_bar_orientation, " ", i = 1),
    # splitting the string (Ex: Transverse (T-L)) and taking only the first part to make consistent with eca
    seam_type = if_else(
      str_detect(seam_type, regex("unk|N/A", ignore_case = TRUE)), 
      NA, 
      seam_type
    ),
    identified_seam_type = case_when(
      identified_seam_type %in% c(     #turns the unknowns from seam table to NA so they can be replaced with data
        #from the description table in the coalesce function
        "Unknown > 4 inch",
        "Unknown 4 inch or less",
        "NA",
        "N/A - Valve/Filter/Other"
      ) ~ NA,
      TRUE ~ identified_seam_type
    ),
    seam = coalesce(identified_seam_type, seam_type),
    seam = case_when(
      seam == "Furnace Butt Weld" ~ "FBW",
      seam == "Lap Weld" ~ "Lap",
      seam == "Seamless" ~ "SMLS",
      seam == "AO Smith SMAW" ~ "SMAW",
      seam == "AO Smith Arc Weld" ~ "SMAW",
      seam == "AO Smith Flash Weld" ~ "Flash",
      seam == "Electric Fusion Weld" ~ "ERW",
      seam == "ELectric Resistance Weld" ~ "ERW",
      seam == "Electric Resistance Weld" ~ "ERW",
      seam == "High Frequency Weld /Electric Resistance Weld" ~ "ERW",
      seam == "electric Resistance Weld" ~ "ERW",
      seam == "Electric Resistance Weld High Freq-HFW" ~ "ERW",
      seam == "Single Submerged Arc Weld" ~ "SSAW",
      seam == "Double Submerged Arc Weld" ~ "DSAW",
      seam == "Submerged Arc Weld Long Seam-SAWL" ~ "SSAW",
      seam == "Spiral Weld" ~ "Spiral",
      seam == "A.O. Smith" & between(year, 1927, 1930) ~ "SMAW",
      seam == "A.O. Smith" & between(year, 1931, 1964) ~ "Flash",
      seam == "A.O. Smith" & between(year, 1970, 1973) ~ "DSAW",
      seam %in% c("A.O. Smith", 
                  "Electric Resistance Weld/Seamless",
                  "Double Submerged Arc Weld / AO Smith SAW / SMLS",
                  "Double Submerged Arc Weld / Seamless")
      ~ NA,
      TRUE ~ seam
    ),
    seam = if_else(seam == "SSAW" & year > 1983, "DSAW", seam),
    seam = if_else(seam == "SAW" & year > 1983, "DSAW", seam),
    seam = if_else(seam == "DSAW" & year < 1940, "SSAW", seam),
    seam = if_else(seam == "SAW" & year < 1940, "SSAW", seam),
    # seam = if_else(id == c("85B",
    #                                    "923",
    #                                    "1431"), "ERW", seam), 
    #Pipe 85B, 923, 1431 identified almost entirely as ERW
    # seam = if_else(id == "1285", "SMLS", seam), 
    #Pipe 1285 listed as SMLS on desc sheet
    seam = if_else(str_detect(id,"DSAW"), "DSAW", seam),
    seam = if_else(str_detect(route,"DSAW"), "DSAW", seam),
    location = case_when(
      str_detect(location, "Base") ~ "Base",
      str_detect(location, "Girth") ~ "Girth",
      str_detect(location, "Seam") ~ "Seam",
      TRUE ~ location
    ),
    spec_id = glue::glue('{id}_{route}_{year}_{seam}_{location}_{bar_orientation}'),
    od_in = as.numeric(nominal_outside_diameter_in),
    wt_in = as.numeric(nominal_wall_thickness_in),
    vintage = year %/%10 *10
  ) %>% 
  filter(sa_ss <= 100,   #removing shear areas greater than 100 like id 13524: 8,054% sa and id 13514: 334% sa
         !is.na(vintage),     #13,492 remain new 9,355
         !is.na(seam),         #12,341 remain new 9,355
         vintage > 1900,      #12,339 remain new 9,353
  ) %>%   
  dplyr::select(
    spec_id,
    id,
    seam,
    location,
    bar_orientation,
    year,
    vintage,
    ct_mm,
    temp_f,
    ae_ss,
    sa_ss,
    smys_psi,
    od_in,
    wt_in
  )

## Count how many unique temps
tim_n2plus <- tim_cvn %>%
  select(spec_id, temp_f) %>%
  unique() %>%
  group_by(spec_id) %>%
  count() %>% 
  filter(n > 1)

# id count before filtering
tim_cvn <- tim_cvn %>%
  right_join(tim_n2plus, by = "spec_id")

## eca description ----------------------------------------------------------------

ecadat <- here("data","MasterDB-SQL-2021-07-28 NS.xlsx")

eca_desc <-  read_xlsx(ecadat,sheet = "Features") %>%
  janitor::clean_names() %>% 
  rename(
    seam = seam_type
  ) %>%
  mutate(
    group_feature = glue::glue('{group}_{feature_id}'),
    od_in = as.numeric(od_inch),
    year = ifelse(
      is.na(pg_e_install_year) |
        pg_e_install_year == "Unknown",
      mfr_year,
      pg_e_install_year
    ),
    year = na_if(year, "Unknown"),
    year = na_if(year, "N/A"),
    year = ifelse(str_detect(year, "/"), 
                  year(as.Date(year, format = "%m/%d/%Y")), 
                  year),
    year = as.integer(str_sub(year, 1, 4)),
    grade = case_when(
      str_detect(grade, "35") ~ "Grade B",
      str_detect(grade, "B") ~ "Grade B",
      str_detect(grade, "42") ~ "X42",
      str_detect(grade, "X6065") ~ "X60",
      str_detect(grade, "-") ~ str_remove(grade, "-"),
      str_detect(grade, "X") ~ str_sub(grade, -3),
      grade == "40000" ~ "Grade B",
      grade == "30000" ~ "LTB",
      grade == "30 ksi (min.)" ~ "LTB",
      TRUE ~ grade
    ),
    grade = na_if(grade, "Unknown"),
    seam = if_else(str_detect(seam, "ERW"), "ERW", seam),
    seam = if_else(seam == "SAWL", "SAW", seam),
    seam = if_else(seam == "SAW" & year > 1983, "DSAW", seam),
    seam = if_else(seam == "DSAW" & year < 1940, "SSAW", seam),
    seam = if_else(seam == "SAW" & year < 1940, "SSAW", seam)
  ) %>% 
  dplyr::select(group_feature,
                grade,
                seam,
                year,
                feature_type,
                od_in)

## eca cvn ----------------------------------------------------------------

eca_cvn <- read_xlsx(ecadat, sheet = "Charpy") %>%
  janitor::clean_names()  %>%
  rename(
    ct_mm = charpy_size_mm,
    ae_ss = absorbed_energy_ft_lbs,
    bar_orientation = orientation,
    temp_f = temperature_f,
    sa_ss = shear_area_percent
  ) %>%
  mutate(group_feature = glue::glue('{group}_{feature}'),
         location = case_when(
           str_detect(location, "Base") ~ "Base",
           str_detect(location, "Girth") ~ "Girth",
           str_detect(location, "Seam") ~ "Seam",
           TRUE ~ location
         ))%>% 
  dplyr::select(
    group_feature,
    ct_mm,
    ae_ss,
    sa_ss,
    location,
    temp_f,
    bar_orientation
  )

## eca all ----------------------------------------------------------------

eca_join <- eca_cvn  %>% 
  left_join(eca_desc, by = "group_feature") %>%
  mutate(spec_id = glue::glue('{group_feature}_{year}_{seam}_{location}_{bar_orientation}')) %>%
  # filter(feature_type == "Pipe") %>% 
  dplyr::select(
    spec_id,
    seam,
    year,
    location,
    od_in,
    bar_orientation,
    ct_mm,
    temp_f,
    ae_ss,
    sa_ss
  )


## tim & eca join ----------------------------------------------

tim_eca_join <- bind_rows(tim_cvn, eca_join) %>%
  mutate(
    vintage = floor(year / 10) * 10,
    ae_fs = ae_ss * 10 / ct_mm
  ) %>%
  filter(
    location == "Base",
    seam != "Lap",
    seam != "FBW",
    seam != "SSAW",
    seam != "Flash",
    seam != "SMAW",
    seam != "Spiral",
    # seam != "DSAW"
  )

## subsize sa fitting ------------------------------------------------------------

Ai_sa = 51
Bi_sa = 49

#create a dataframe with the temperature penalty that you get on SATT85 due to small size samples. These are the C_A values in API 579 the table before equation 9F.6
D_shift <- data.frame(
  Charpy_size = c(10, 6.7, 5.0, 3.3, 2.5),
  Temperature_shift = c(0, 9, 14, 22, 28)
)

C_shift <- data.frame(
  Charpy_size = c(10, 6.7, 5.0, 3.3, 2.5),
  Temperature_shift = c(0, 1, 3, 9, 14)
)

# hyper tan model ---------------------------------------------------------

hyptan_model <- tim_eca_join %>%
  nest(data = -spec_id) %>% 
  mutate(k1 = map(data, ~ atanh((.$sa_ss - Ai_sa) / Bi_sa))) %>%
  unnest(everything()) %>%
  nest(data = -spec_id) %>%
  mutate(
    hyptan_modellm = map(data, possibly( ~ lm(.$temp_f ~ .$k1))),
    tidied_sa = map(hyptan_modellm, possibly(tidy)),
    empty = map_vec(tidied_sa, is_empty)
  ) %>% 
  filter(
    empty == FALSE
  ) %>%
  mutate(
    D_sass = map_vec(tidied_sa,~nth(.$estimate,1)),
    C_sass = map_vec(tidied_sa,~nth(.$estimate,2)),
    D_error = map_vec(tidied_sa,~nth(.$std.error,1)),
    C_error = map_vec(tidied_sa,~nth(.$std.error,2)),
    ct_mm =   map_vec(data,     ~nth(.$ct_mm,1)),
    ct_mm = if_else(between(ct_mm, 2.4, 2.5), 2.5, ct_mm)
  ) %>% 
  
  mutate(
    # For this two-parameter model, we can use the trend gained from the one-parameter model to 
    # increase the 50pct transition temperature of the transition region up to that expected for full-size
    # Because per Switzner, 2023, Metals (MDPI), D = DBTT85 - Sigmoidal parameter A
    D_sa_fs = D_sass + approx(D_shift$Charpy_size, D_shift$Temperature_shift, xout = ct_mm)$y,  
    # For this two-parameter model, we can use the trend gained from the one-parameter model to 
    # increase the half-width of the transition region up to that expected for full-size
    # Because per Switzner, 2023, Metals (MDPI), One-parameter model, C = Sigmoidal parameter B
    C_sa_fs = C_sass + 2 * approx(C_shift$Charpy_size, C_shift$Temperature_shift, xout = ct_mm)$y, # approx(data$Charpy_size, data$Temperature_shift, xout = charpy_size)$y
    C_sa_fs = if_else(C_sa_fs<26 | C_sa_fs>106, 66, C_sa_fs),
    #Calc the new temperature at which the SA value would have hypothetically occurred if the Charpy was fullsize
    temp_f_fs = pmap(list(data, C_sa_fs, D_sa_fs), function(data, C_sa_fs, D_sa_fs) (C_sa_fs * data$k1 + D_sa_fs))
    # k2 = map(data, ~ tanh((.$temp_f - D_sa_fs) / C_sa_fs)),
    # mod_aelm = map2(list(k2, data), ~ lm(data$ae_fs ~ k2))
  ) %>% 
  dplyr::select(-ct_mm) %>% 
  unnest(c(data, temp_f_fs)) %>%
  mutate(
    ae_fs = ae_ss * 10 / ct_mm
  ) %>% 
  nest(data = -c(spec_id, hyptan_modellm, tidied_sa, empty, D_sass, C_sass, D_sa_fs, C_sa_fs, D_error, C_error)) %>% 
  mutate(
    k2 = pmap(list(data, C_sa_fs, D_sa_fs), function(data, C_sa_fs, D_sa_fs) tanh((data$temp_f_fs - D_sa_fs)/C_sa_fs)),
    aelm = map2(data, k2, possibly( ~ lm(.x$ae_fs ~ .y))),
    tidied_ae = map(aelm, tidy),
    empty = map_vec(tidied_ae, is_empty)
  ) %>% 
  filter(
    empty == FALSE
  ) %>% 
  mutate(
    seam  =     map_vec(data,     ~nth(.$seam,1)),
    vintage =   map_vec(data,     ~nth(.$vintage,1)),
    A_ae_fs = map_vec(tidied_ae,~nth(.$estimate,1)),
    A_ae_fs = abs(A_ae_fs),
    B_ae_fs = map_vec(tidied_ae,~nth(.$estimate,2)),
    B_ae_fs = if_else(B_ae_fs < 0.5 * A_ae_fs |
                        B_ae_fs > A_ae_fs, 0.818 * A_ae_fs, B_ae_fs),
    A_error = map_vec(tidied_ae,~nth(.$std.error,1)),
    B_error = map_vec(tidied_ae,~nth(.$std.error,2)),
    ct_mm =   map_vec(data,     ~nth(.$ct_mm,1)),
    ct_mm =   if_else(between(ct_mm, 2.4, 2.5), 2.5, ct_mm),
    use_fs = A_ae_fs + B_ae_fs,
    use_fs_lo = use_fs - A_error - B_error,
    use_fs_hi = use_fs + A_error + B_error,
    t_20 = D_sa_fs +  C_sa_fs / B_ae_fs * (20 - A_ae_fs),
    t_20 = if_else(t_20 < 50 & seam == "ERW", 50 , t_20), #There's an dip in ERW T20 in 1970 that doesn't make sense and could be non-conservative
    t_20_error_dist = if_else(D_error < 35, 35 , D_error), #introducing a standard minimum error assumption to eliminate non-conservative values from little datasets like seam/vintage combinations with only one pipe
    t_20_lo = t_20 - D_error,
    t_20_hi = t_20 + D_error,
    t_20_lo_dist = t_20 - t_20_error_dist, #There's a dip in ERW T20 in 1970 that doesn't make sense and could be non-conservative
    t_20_hi_dist = t_20 + t_20_error_dist,
    n = map_vec(data, nrow),
  ) 


# multi_plot --------------------------------------------------------------
func_sa <- function(temp_f, C_sass, D_sass) {
  51 + 49 * tanh((temp_f - D_sass) / C_sass)
}

sa_plots <- hyptan_model %>%
  select(data, spec_id, C_sa_fs, D_sa_fs) %>%
  unnest(data) %>%
  select(spec_id, sa_ss, temp_f, C_sa_fs, D_sa_fs) %>%
  nest(data = c(sa_ss, temp_f)) %>%
  mutate(plt = pmap(list(data, spec_id, C_sa_fs, D_sa_fs), function(w, x, y, z) {
    w %>%
      ggplot(aes(x = temp_f, y = sa_ss)) +
      geom_point() +
      stat_function(fun = func_sa, args = list(y, z)) +
      ylim(0,100)+
      xlim(-100,100)+
      ggtitle(paste0(x))
  }))


# AE plots ----------------------------------------------------------------
# hyperbolic tangent function for absorbed energy
func_ae <- function(temp_f, A, B, C, D) {
  A + B * tanh((temp_f - D) / C)
}

ae_plots <- hyptan_model %>%
  mutate(use = A_ae_fs + B_ae_fs) %>%
  select(data, spec_id, A_ae_fs, B_ae_fs, C_sa_fs, D_sa_fs, use) %>%
  unnest(data) %>%
  select(spec_id, ae_fs, temp_f, A_ae_fs, B_ae_fs, C_sa_fs, D_sa_fs, use) %>%
  nest(data = c(ae_fs, temp_f)) %>%
  mutate(plt = pmap(list(data, spec_id, A_ae_fs, B_ae_fs, C_sa_fs, D_sa_fs, use), function(u, v, w, x, y, z, zz) {
    u %>%
      ggplot(aes(x = temp_f, y = ae_fs)) +
      geom_point() +
      xlim(-100, 100) +
      ylim(0, max(c(u$ae_fs, zz))) +
      stat_function(fun = func_ae, args = list(w, x, y, z)) +
      ggtitle(paste0(v))
  }))


suspects <- hyptan_model %>%
  filter(abs(D_sass) < 2 * D_error |
           C_sass < 2 * C_error |
           A_ae_fs < 2 * A_error |
           B_ae_fs < 2 * B_error|
           C_sass < 0)
# write_csv(suspects, here("suspects.csv"))


# Example plots -----------------------------------------------------------

sa_plots$plt[1:20]
sid <- 200:220
sa_plots %>%  filter(spec_id%in% suspects$spec_id[sid]) %>% pull(plt)
ae_plots %>% filter(spec_id %in% suspects$spec_id[sid]) %>% pull(plt)


# t-test ------------------------------------------------------------------

nodif_3250 <- hyptan_model %>%
  select(spec_id, data) %>%
  unnest(data) %>%
  select(spec_id, temp_f, sa_ss, ae_fs) %>%
  # filter(temp_f == 32 | temp_f == 50) %>%
  nest(data = -spec_id) %>%
  mutate(ttest = map(data, possibly(~ (
    t.test(.$sa_ss[.$temp_f == 32], .$sa_ss[.$temp_f == 50])
  ))),
  tdy = map(ttest, possibly( ~ tidy(.)))) %>%
  unnest(tdy) %>%
  filter(p.value > 0.05) %>% 
  arrange(-p.value)

sa_plots %>%  filter(spec_id%in% nodif_3250$spec_id[51:70]) %>% pull(plt)



# aggregating -------------------------------------------------------------

hyptan_model %>% 
  filter(seam=="ERW"|seam=="DSAW"|seam=="SMLS") %>% 
  group_by(seam, vintage) %>% 
  summarise(use_05 = mean(use_fs_lo, na.rm = TRUE),
            t_20= mean(t_20, na.rm = TRUE),
            use_50 = mean(use_fs, na.rm = TRUE))
