## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("insurancerating")

## ----gh-installation, eval = FALSE--------------------------------------------
#  # install.packages("remotes")
#  remotes::install_github("MHaringa/insurancerating")

## ----example, eval = TRUE, message = FALSE, warning = FALSE-------------------
library(insurancerating)


# Claim frequency 
age_policyholder_frequency <- fit_gam(data = MTPL, 
                                      nclaims = nclaims, 
                                      x = age_policyholder, 
                                      exposure = exposure)

# Claim severity 
age_policyholder_severity <- fit_gam(data = MTPL, 
                                     nclaims = nclaims, 
                                     x = age_policyholder, 
                                     exposure = exposure, 
                                     amount = amount, 
                                     model = "severity")

## ----plotgam, eval = TRUE, message = FALSE, warning = FALSE-------------------
library(ggplot2)
autoplot(age_policyholder_frequency, show_observations = TRUE)


## ----figfreq, eval = TRUE-----------------------------------------------------

clusters_freq <- construct_tariff_classes(age_policyholder_frequency)
clusters_sev <- construct_tariff_classes(age_policyholder_severity)

## ----plotclustersfreq, eval = TRUE--------------------------------------------
autoplot(clusters_freq, show_observations = TRUE)

## ----figsev, eval = TRUE, message = FALSE, warning = FALSE--------------------
library(dplyr)
age_policyholder_severity %>%
  construct_tariff_classes() %>%
  autoplot(., show_observations = TRUE, remove_outliers = 100000)


## ----example2, eval = TRUE, message = FALSE, warning = FALSE------------------

dat <- MTPL %>%
  mutate(age_policyholder_freq_cat = clusters_freq$tariff_classes) %>%
  mutate(age_policyholder_sev_cat = clusters_sev$tariff_classes) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, list(~biggest_reference(., exposure)))

glimpse(dat)



## ----example3, eval = TRUE, echo = TRUE---------------------------------------
model_freq <- glm(nclaims ~ age_policyholder_freq_cat, offset = log(exposure), 
                  family = "poisson", data = dat)

model_sev <- glm(amount ~ age_policyholder_freq_cat, weights = nclaims, 
                  family = Gamma(link = "log"), data = dat[dat$amount > 0, ])

rating_factors(model_freq, model_sev, model_data = dat, exposure = exposure) %>%
  autoplot()

