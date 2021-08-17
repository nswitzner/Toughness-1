library(tidyverse)
library(tidymodels)
library(ggpmisc)
library(patchwork)

set.seed(777)

maxey <- read_csv("maxey_data.csv",
                  skip = 5,
                  col_names = TRUE) %>%
  janitor::clean_names()

maxey %>%
  ggplot(aes(x, y)) +
  geom_point() +
  theme_minimal(14) +
  geom_smooth(method = "lm", se = TRUE) +
  annotate(
    "table",
    x = 0.15,
    y = 0.95,
    label = list(maxtidy),
    size = 5
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  labs(title = "Regression Analysis of Maxey Shear Area Data",
       x = "Energy/Plateau Energy",
       y = expression(S[c] ~ "," ~ S[F]))

  

lm_mod <- lm(y ~ x, maxey)

glance(lm_mod)
maxey_pred_int <- augment(lm_mod,newdata = maxey,interval = "prediction")
write_csv(maxey_pred_int, "maxey_pred_int.csv")


maxtidy <- tidy(lm_mod,conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high) %>% 
  mutate(across(.cols = estimate:conf.high,.fns =  ~round(.x, 3)))

boots <- bootstraps(maxey,
                    times = 2e3,
                    apparent = TRUE)

fit_lm_boot <- function(split){
  lm(y ~ x, analysis(split)) 
}

boot_models <- boots %>% 
  mutate(model = map(splits, fit_lm_boot),
         coef_info = map(model, tidy))

boot_coefs <- boot_models %>% 
  unnest(coef_info)

percentile_intervals <- int_pctl(boot_models, coef_info)

histo <- ggplot(boot_coefs, aes(estimate)) +
  geom_histogram(bins = 30,
                 aes(fill = term),
                 col = 'black',
                 show.legend = F) +
  facet_wrap(~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower),
             data = percentile_intervals,
             col = "blue",
             lty = 2,
             lwd=0.72) +
  geom_vline(aes(xintercept = .upper),
             data = percentile_intervals,
             col = "blue",
             lty = 2,
             lwd=0.72) +
  ggsci::scale_fill_simpsons() +
  theme_bw()+
  labs(title = "Uncertainty of Coefficents to Maxey Toughness Data",
       x="Coefficent Estimates")

boot_aug <- 
  boot_models %>% 
  slice_sample(n = 50) %>% 
  mutate(augmented = map(model, augment)) %>% 
  unnest(augmented)

regress <- boot_aug %>% 
  ggplot(aes(x, y)) +
  geom_line(aes(y = .fitted, group = id), 
            alpha = .1, 
            col = "blue") +
  geom_point(alpha=0.5, 
             col='orangered', size=1)+
  theme_minimal()+
  labs(title = "Regression Uncertainty")



histo/regress

MASS::rlm(y ~ x, maxey)

boot_aug %>% 
  ggplot(aes(.resid))+
  geom_histogram(fill='royalblue2', 
                 col='black',
                 bins=50)+
  theme_minimal()

performance::check_model(lm_mod)

library(rstanarm)

max_stan <- stan_glm(y ~ x, data = maxey,family = "gaussian")

summary(max_stan)

plot(max_stan, prob=0.95)


