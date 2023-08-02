# Purpose: To model coho growh as a fxn of time and to select the best model according
# to a variety of metrics
# Creator: Matthew LH. Cheng UAF CFOS
# Date 2/4/23

# Set up ------------------------------------------------------------------

library(here)
library(tidyverse)
library(mgcv)
library(visreg)
library(gratia)
library(insol)
library(mgcViz)

# Source function for making gam fxns
source(here("R", "Analysis", "gam_analysis_fxns.R"))

# load in data
# contains temp and flow data now
mt_all <- read.csv(here("output", "MT_Age_Phys_Dat.csv")) 

# Bind these two dataframes together
mt_all <- mt_all %>%  
  filter(age_cluster %in% c("age-0", "age-1")) %>% # filter for only these ages
  mutate(ci_fact = (100 * Weight) / TL^3, # Create metric for condition factor
         PIT_tag = ifelse(is.na(PIT_tag), "Not_tagged", PIT_tag),
         mean_Flow_cms = ifelse(is.na(mean_Flow_cms), 
                                median(mt_all$mean_Flow_cms[mt_all$year == 2022], na.rm =  TRUE),
                                mean_Flow_cms))  # NOTE: We are filling in the missing flow w/ the mean

# Separate out the ages here by dataframe
age_0 <- mt_all %>% filter(age_cluster == "age-0") %>% 
  filter(mean_Flow_cms < 40)
age_1 <- mt_all %>% filter(age_cluster == "age-1") %>% 
  filter(mean_Flow_cms < 20)

# Set up model selection
possible_variables <- c(
  "Habitat", # habitat
  "s(mean_Flow_cms, by = factor(year), k = 4, bs = 'cr')", # mean flow response by year
  "s(mean_Temp_C, by = factor(year), k = 4, bs = 'cr')", # mean temp response by year
  "s(mean_Flow_cms, k = 4, bs = 'cr')",
  "s(doy, k = 4, bs = 'cr')",
  "s(mean_Temp_C, k = 4, bs = 'cr')"
)

# Conduct Model Selection (Age-0s) Length ----------------------------------------

# Control variables (specify response formula and variables we want to keep in all models)
control_variables_len <- "TL ~ s(doy, by = factor(year), k = 4, bs = 'cr') + "


# Run models
age_0_TL_models <- model_selex(possible_variables = possible_variables, 
                               control_variables =  control_variables_len,
                               df = age_0, error_dist = Gamma(link = "log"))

# Save model outputs
save(age_0_TL_models, file = here("output", "model_selection", "age_0_TL_models.RData"))

# Conduct Model Selection (Age-1s) Length ----------------------------------------

# Run models
age_1_TL_models <- model_selex(possible_variables = possible_variables, 
                               control_variables =  control_variables_len,
                               df = age_1, error_dist = Gamma(link = "log"))

# Save model outputs
save(age_1_TL_models, file = here("output", "model_selection", "age_1_TL_models.RData"))


# Conduct Model Selection (Age-0s) Weight ----------------------------------------

# Control variables (specify response formula and variables we want to keep in all models)
control_variables_wt <- "Weight ~ s(doy, by = factor(year), k = 4, bs = 'cr') + "

# Run models
age_0_wt_models <- model_selex(possible_variables = possible_variables, 
                               control_variables =  control_variables_wt,
                               df = age_0, error_dist = Gamma(link = "log"))

# Save model outputs
save(age_0_wt_models, file = here("output", "model_selection", "age_0_wt_models.RData"))

# Conduct Model Selection (Age-1s) Weight ----------------------------------------

# Run models
age_1_wt_models <- model_selex(possible_variables = possible_variables, 
                               control_variables =  control_variables_wt,
                               df = age_1, error_dist = Gamma(link = "log"))

# Save model outputs
save(age_1_wt_models, file = here("output", "model_selection", "age_1_wt_models.RData"))



