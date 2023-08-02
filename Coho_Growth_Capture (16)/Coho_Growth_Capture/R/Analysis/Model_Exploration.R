# Purpose: To model coho growh as a fxn of time
# Creator: Matthew LH. Cheng UAF CFOS
# Date 10/17/22

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
                                mean(mt_all$mean_Flow_cms, na.rm =  TRUE),
                                mean_Flow_cms))  # NOTE: We are filling in the missing flow w/ the mean

# Separate out the ages here by dataframe
age_0 <- mt_all %>% filter(age_cluster == "age-0") %>% 
  filter(mean_Flow_cms < 40)
age_1 <- mt_all %>% filter(age_cluster == "age-1") %>% 
  filter(mean_Flow_cms < 20)
  

# Start modelling using GAMs ----------------------------------------------

# Constraining to 4 basis functions such that we don't overfit to noise
# This formulation of s(doy) + s(doy, by = year) makes it so that
# we are penalizing the smooths to be deviating from the global term.
# This way, we can help share information among groups
mod_age0 <- bam(TL ~ s(doy, by = factor(year), k = 4, bs = "tp") + year +
                  s(doy, k = 4) + s(mean_Flow_cms, k = 4) + s(mean_Temp_C, k = 4) +
                  s(mean_Flow_cms, by = factor(year),k = 4, bs = "tp") +
                  s(mean_Temp_C, by = factor(year), k = 4, bs = "tp"),
                  data = age_0, family = Gamma(link = "log"))

mod_age1 <- bam(TL ~ s(doy, by = factor(year), k = 4, bs = "cr")  +
                   s(mean_Flow_cms, by = factor(year), k = 4, bs = "cr") +
                   s(mean_Temp_C, by = factor(year), k = 4, bs = "cr"),
                   data = age_1, family = Gamma(link = "log"))

print(plot(getViz(mod_age1), allTerms = T) +
        theme(axis.text = element_text(angle = 90)), pages = 1)


# Look at model summary
summary(mod_age0)
summary(mod_age1)

# Look at autocorrelation in residuals
acf(residuals(mod_age0))
acf(residuals(mod_age1))

# Look at model daignostics
par(mfrow = c(2,2))
gam.check(mod_age0)
abline(0,1, col = "red")

gam.check(mod_age1)
abline(0,1, col = "red")
dev.off()

# quick plotting for model
par(mfrow = c(1,2))
plot(mod_age0)
plot(mod_age1)


# make predictions here

# Age 0 2021
age0_2021 <- pred_gam(df = with(age_0,
                                data.frame(doy = unique(doy),
                                           age_cluster = "age-0",
                                           year = "2021",
                                           Habitat = "Main_Side",
                                           mean_Flow_cms = mean(mean_Flow_cms, na.rm = TRUE),
                                           mean_Temp_C = mean(mean_Temp_C, na.rm = TRUE))),  mod = mod_age0) 

# Age 0 2022
age0_2022 <- pred_gam(df = with(age_0,
                                data.frame(doy = unique(doy),
                                           age_cluster = "age-0",
                                           year = "2022",
                                           Habitat = "Main_Side",
                                           mean_Flow_cms = mean(mean_Flow_cms, na.rm = TRUE),
                                           mean_Temp_C = mean(mean_Temp_C, na.rm = TRUE))),  mod = mod_age0) 

# Age 1 2021
age1_2021 <- pred_gam(df = with(age_1,
                                data.frame(doy = unique(doy),
                                           age_cluster = "age-1",
                                           year = "2021",
                                           Habitat = "Main_Side",
                                           mean_Flow_cms = mean(mean_Flow_cms, na.rm = TRUE),
                                           mean_Temp_C = mean(mean_Temp_C, na.rm = TRUE))), mod = mod_age1) 

# Age 1 2022
age1_2022 <- pred_gam(df = with(age_1,
                                data.frame(doy = unique(doy),
                                           age_cluster = "age-1",
                                           year = "2022",
                                           Habitat = "Main_Side",
                                           mean_Flow_cms = mean(mean_Flow_cms, na.rm = TRUE),
                                           mean_Temp_C = mean(mean_Temp_C, na.rm = TRUE))),  mod = mod_age1) 

# Now, bound these all together
all_preds <- rbind(age0_2021, age0_2022, age1_2021, age1_2022)


# Plots -------------------------------------------------------------------

# Doy and TL
ggplot() +
  geom_ribbon(all_preds, mapping = aes(x = doy, y = fitted, ymin = lower, ymax = upper,
                                       fill = year), alpha = 0.35) +
  geom_line(all_preds, mapping = aes(x = doy, y = fitted, color = year), size = 1.5) +
  geom_jitter(mt_all, mapping = aes(x = doy, y = TL, color = factor(year),
                                    shape = factor(year)), alpha = 0.45) +
  # ggthemes::scale_color_colorblind() +
  # ggthemes::scale_fill_colorblind() +
  facet_wrap(~age_cluster, scales = "free_x") +
  theme_bw() +
  labs(x = "Day of Year", y = "TL", color = "Year", shape = "Year", fill = "Year") +
  theme(legend.position = c(0.075, 0.9))


