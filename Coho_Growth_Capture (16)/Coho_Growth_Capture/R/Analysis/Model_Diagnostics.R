# Purpose: To conduct diagnostics on candidate models
# 3/1/23
# Creator: Matthew LH. Cheng (UAF - CFOS)


# Set up ------------------------------------------------------------------

library(here)
library(tidyverse)
library(mgcv)
library(visreg)
library(gratia)
library(insol)
library(mgcViz)

# Load in models
# Weight models
load(here("output", "model_selection", "age_0_wt_models.RData"))
load(here("output", "model_selection", "age_1_wt_models.RData"))

# Length models
load(here("output", "model_selection", "age_0_TL_models.RData"))
load(here("output", "model_selection", "age_1_TL_models.RData"))


# Select candidate models -------------------------------------------------

# Weight Models
# Look at best model formulation
mod_age0_wt_comp <- age_0_wt_models[[2]]
# Extract the best model (lowest AIC value)
age0_cand_wt_mod <- age_0_wt_models[[1]]$`model 15`
# Look at best model formulation
mod_age1_wt_comp <- age_1_wt_models[[2]]
# Extract the best model (lowest AIC value)
age1_cand_wt_mod <- age_1_wt_models[[1]]$`model 3`

# Length Models
# Look at best model formulation
mod_age0_comp <- age_0_TL_models[[2]]
# Extract the best model (lowest AIC value)
age0_cand_mod <- age_0_TL_models[[1]]$`model 15`
# Look at best model formulation
mod_age1_comp <- age_1_TL_models[[2]]
# Extract the best model (lowest AIC value)
age1_cand_mod <- age_1_TL_models[[1]]$`model 3`


# Diagnostics -------------------------------------------------------------

# Weight Models
# Age 0s
par(mfrow = c(2,2))
gam.check(age0_cand_wt_mod)
abline(0, 1, col = "red")
dev.off()

# Age 1s
par(mfrow = c(2,2))
gam.check(age1_cand_mod)
abline(0, 1, col = "red")
dev.off()

# Length Models
# Age 0s
par(mfrow = c(2,2))
gam.check(age0_cand_mod)
abline(0, 1, col = "red")
dev.off()

# Age 1s
par(mfrow = c(2,2))
gam.check(age1_cand_mod)
abline(0, 1, col = "red")
dev.off()


