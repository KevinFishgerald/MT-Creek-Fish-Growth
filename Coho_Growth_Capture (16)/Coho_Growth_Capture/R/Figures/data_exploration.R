# Purpose: To explore coho data
# Creator: Matthew LH. Cheng
# Date 10/27/22


# Set up ------------------------------------------------------------------

library(here)
library(tidyverse)
library(mgcv)
library(visreg)
library(gratia)
library(insol)

# Source function for making gam predictions
source(here("R", "Analysis", "gam_analysis_fxns.R"))

# load in data
# contains temp and flow data now
mt_all <- read.csv(here("output", "MT_Age_Phys_Dat.csv")) 

# Bind these two dataframes together
mt_all <- mt_all %>%  
  filter(age_cluster %in% c("age-0", "age-1")) %>% # filter for only these ages
  mutate(ci_fact = (100 * Weight) / TL^3, # Create metric for condition factor
         PIT_tag = ifelse(is.na(PIT_tag), "Not_tagged", PIT_tag),
         mean_Flow_cms = ifelse(is.na(mean_Flow_cms), mean(mt_all$mean_Flow_cms, na.rm =  TRUE),
                                mean_Flow_cms))  # NOTE: We are filling in the missing flow w/ the mean

# Separate out the ages here by dataframe
age_0 <- mt_all %>% filter(age_cluster == "age-0") %>% 
  filter(mean_Flow_cms < 40)
age_1 <- mt_all %>% filter(age_cluster == "age-1") %>% 
  filter(mean_Flow_cms < 20)


# Look at age distributions -----------------------------------------------

# Figure out for each month, which individuals were the smallest for the
# age 1s, and largest for the age 0s
mt_minage1 <- mt_all %>%  # Min for age 1
  mutate(month = lubridate::month(date)) %>% 
  filter(age_cluster == "age-1") %>% 
  group_by(year, month) %>% 
  summarize(min_age1 = min(TL, na.rm = T))

# Max for age0
mt_maxage0 <- mt_all %>% 
  mutate(month = lubridate::month(date)) %>% 
  filter(age_cluster == "age-0") %>% 
  group_by(year, month) %>% 
  summarize(max_age0 = max(TL, na.rm = T))

png(here("figs", "age_clusters.png"), width = 1000, height = 800)

print(
  mt_all %>% 
    mutate(month = lubridate::month(date)) %>% 
    left_join(mt_minage1, by = c("year", "month")) %>% 
    left_join(mt_maxage0, by = c("year", "month")) %>% 
    ggplot(aes(x = TL, fill = age_cluster)) +
    geom_bar(alpha = 0.8) +
    # Color lines here
    geom_vline(aes(xintercept = min_age1), color = "#00BFC4", lty = 2, size = 1.1) +
    geom_vline(aes(xintercept = max_age0), color = "#F8766D", lty = 2, size = 1.1) +
    facet_grid(month~year, scales = "free_y") +
    theme_bw() + 
    theme(legend.position = "top",
          axis.text = element_text(size = 13, color = "black"),
          axis.title = element_text(size = 15),
          strip.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13)) +
    labs(x = "Total Length (mm)", y = "Count",
         fill = "Age Cluster", color = "Age Cluster") 
)
dev.off()


# Start plotting ----------------------------------------------------------

# Total length as af fxn of year
ggplot(mt_all, aes(x = factor(year), y = TL)) +
  geom_boxplot()

# Total length as af fxn of year - break it down by age class
ggplot(mt_all, aes(x = factor(year), y = TL, fill = age_cluster, color = age_cluster)) +
  geom_jitter(alpha = 0.5, width = 0.05) +
  geom_violin(alpha = 0.3) +
  facet_wrap(~age_cluster) +
  ylim(0,130)

# Weight as af fxn of year
ggplot(mt_all, aes(x = factor(year), y = Weight, fill = age_cluster, color = age_cluster)) +
  geom_jitter(alpha = 0.5, width = 0.05) +
  geom_violin(alpha = 0.3) +
  facet_wrap(~age_cluster) 

# Weight as af fxn of year - break it down by age class
ggplot(mt_all, aes(x = factor(year), y = Weight, fill = age_cluster)) +
  geom_boxplot() 
