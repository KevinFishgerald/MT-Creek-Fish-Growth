# Purpose: To make figures for AK AFS Poster Presentation
# Date 2/2/23
# Creator: Matthew LH. Cheng (UAF-CFOS)


# Set up ------------------------------------------------------------------

library(here)
library(tidyverse)
library(lubridate)
library(cowplot)
library(mgcv)

# Source function for making gam fxns
source(here("R", "Analysis", "gam_analysis_fxns.R"))

# load in length weight data, with mean flow and temp 
mt_all <- read.csv(here("output", "MT_Age_Phys_Dat.csv")) 

# load in physical data
temp <- read.csv(here("output", "mt_temp_all.csv"))
flow <- read.csv(here("output", "mt_flow_all.csv"))


# Quick data cleaning -----------------------------------------------------

# Bind these two dataframes together
mt_all <- mt_all %>%  
  filter(age_cluster %in% c("age-0", "age-1")) %>% # filter for only these ages
  mutate(ci_fact = 100 * (Weight / TL^3), # Create metric for condition factor
         PIT_tag = ifelse(is.na(PIT_tag), "Not_tagged", PIT_tag),
         # NOTE: We are filling in the missing flow w/ the mean for now!***
         mean_Flow_cms = ifelse(is.na(mean_Flow_cms), 
                                mean(mt_all$mean_Flow_cms, na.rm =  TRUE),
                                mean_Flow_cms),
         date = ymd(date))  

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


# Length Weight Plots -----------------------------------------------------

# Get mean lengths for age clusters
mean_sizes <- mt_all %>% 
  group_by(age_cluster, year) %>% 
  summarize(mean_length = mean(TL, na.rm = TRUE),
            mean_weight = mean(Weight, na.rm = TRUE))

# Length frequency plot
png(here("figs", "Len_Freq.png"), width = 800, height = 600)
ggplot(mt_all, aes(x = TL, fill = age_cluster)) +
  geom_histogram(alpha = 0.35, color = "black") + 
  geom_vline(mean_sizes, mapping = aes(xintercept = mean_length, color = age_cluster),
             size = 1.5, lty = 2) +
  ggsci::scale_fill_nejm() +
  ggsci::scale_color_nejm() +
  facet_wrap(~year, ncol = 1) +
  guides(color = "none") +
  theme_bw() + 
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.position = c(0.9, 0.9),
        strip.text = element_text(size = 17)) +
  labs(x = "Total Length (mm)", y = "Frequency",
       fill = "Age Cluster", color = "Age Cluster")
dev.off()

# Weight frequency plot
png(here("figs", "Wt_Freq.png"), width = 800, height = 600)
ggplot(mt_all, aes(x = Weight, fill = age_cluster)) +
  geom_histogram(alpha = 0.35, color = "black") + 
  geom_vline(mean_sizes, mapping = aes(xintercept = mean_weight, 
                                       color = age_cluster),
             size = 1.5, lty = 2) +
  ggsci::scale_fill_nejm() +
  ggsci::scale_color_nejm() +
  facet_wrap(~year, ncol = 1) +
  guides(color = "none") +
  theme_bw() + 
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.position = c(0.9, 0.9),
        strip.text = element_text(size = 17)) +
  labs(x = "Total Weight (g)", y = "Frequency",
       fill = "Age Cluster", color = "Age Cluster")
dev.off()

# Physical Data -----------------------------------------------------------
# Quick data summary
temp_sum <- temp %>% 
  mutate(Date = ymd_hms(Date), 
         Year = year(Date)) %>% 
  group_by(Year, doy) %>% 
  summarize(mean_Temp = mean(Temp_C, na.rm = TRUE))

# Temperature plots
png(here("figs", "mean_daily_temp.png"), width = 800, height = 600)
ggplot(temp_sum, aes(x = doy, y = mean_Temp, color = factor(Year))) +
  geom_line(size = 1, alpha = 0.9) +
  scale_color_manual(values = c("#0073C299", "#A7303099")) +
  theme_bw() +
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.position = c(0.1, 0.85),
        legend.key.width = unit(1, "cm")) +
  labs(x = "Day of Year", y = "Mean Daily Temperature (°C)", 
       color = "Year")
dev.off()

# Flow Plots
# Quick data summary for flow
flow_sum <- flow %>% 
  mutate(Date = ymd(Date), 
         Year = year(Date)) %>% 
  group_by(Year, doy) %>% 
  summarize(mean_Flow_cms = mean(Flow_cms, na.rm = TRUE)) %>% 
  filter(Year %in% c(2021, 2022))

png(here("figs", "mean_daily_flow.png"), width = 800, height = 600)
ggplot(flow_sum, aes(x = doy, y = mean_Flow_cms, color = factor(Year))) +
  geom_line(size = 1, alpha = 0.9) +
  scale_color_manual(values = c("#0073C299", "#A7303099")) +
  scale_linetype_manual(values = c(1, 2)) +
  theme_bw() +
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.position = c(0.1, 0.85),
        legend.key.width = unit(1, "cm")) +
  labs(x = "Day of Year", y = "Mean Daily Flow (cms)", 
       color = "Year", linetype = "Year")
dev.off()

# Bioenergetic Plots ------------------------------------------------------

# load in bioenergetic simulation outputs
bioe_df_len <- readxl::read_excel(here("data", "pink_pres_abs_bioenergetics.xlsx"), sheet = "Length") %>% 
  mutate(Type = "Length", age_cluster = "age-1")
bioe_df_wt <- readxl::read_excel(here("data", "pink_pres_abs_bioenergetics.xlsx"), sheet = "Mass") %>% 
  mutate(Type = "Weight", age_cluster = "age-1")

# Pivot longer
bioe_df_all <- rbind(bioe_df_len, bioe_df_wt)
bioe_df_long <- bioe_df_all %>% 
  pivot_longer(cols = c("2021", "2022"), names_to = "year", values_to = "growth") %>% 
  rename(doy = `Julian Day`)

# Plot -- weight
png(here("figs", "bioe_wt.png"), width = 600, height = 400)
ggplot(bioe_df_long %>% 
         filter(Type == "Weight"), aes(x = doy, y = growth, color = year)) +
  geom_line(size = 2, alpha = 0.9) +
  geom_vline(xintercept = 210, lty = 2) +
  scale_color_manual(values = c("#0073C299", "#A7303099")) +
  theme_bw() + 
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.position = c(0.075, 0.85)) +
  labs(x = "Day of Year", y = "Weight (g)", color = "Year")
dev.off()

# Plot -- length
png(here("figs", "bioe_len.png"), width = 600, height = 400)
ggplot(bioe_df_long %>% 
         filter(Type == "Length"), aes(x = doy, y = growth, color = year)) +
  geom_line(size = 2, alpha = 0.9) +
  geom_vline(xintercept = 210, lty = 2) +
  scale_color_manual(values = c("#0073C299", "#A7303099")) +
  theme_bw() + 
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.position = c(0.075, 0.85)) +
  labs(x = "Day of Year", y = "Length (mm)", color = "Year")
dev.off()

# GAM Results (Length) -------------------------------------------------------------

# Load in models
load(here("output", "model_selection", "age_0_TL_models.RData"))
load(here("output", "model_selection", "age_1_TL_models.RData"))

# Look at best model formulation
mod_age0_comp <- age_0_TL_models[[2]]
# Extract the best model (lowest AIC value)
age0_cand_mod <- age_0_TL_models[[1]]$`model 10`
age0_cand_mod$formula

# Look at best model formulation
mod_age1_comp <- age_1_TL_models[[2]]
# Extract the best model (lowest AIC value)
age1_cand_mod <- age_1_TL_models[[1]]$`model 5`
age1_cand_mod$formula

# Age 0 2021
age0_2021 <- pred_gam(df = with(age_0,
             data.frame(doy = unique(doy),
             age_cluster = "age-0",
             year = "2021",
             Habitat = "Main_Side",
             mean_Flow_cms = mean(mean_Flow_cms, na.rm = TRUE),
             mean_Temp_C = mean(mean_Temp_C, na.rm = TRUE))),  
             mod = age0_cand_mod) 

# Age 0 2022
age0_2022 <- pred_gam(df = with(age_0,
             data.frame(doy = unique(doy),
             age_cluster = "age-0",
             year = "2022",
             Habitat = "Main_Side",
             mean_Flow_cms = mean(mean_Flow_cms, na.rm = TRUE),
             mean_Temp_C = mean(mean_Temp_C, na.rm = TRUE))),  
             mod = age0_cand_mod)


# Age 1 2021
age1_2021 <- pred_gam(df = with(age_1,
             data.frame(doy = unique(doy),
             age_cluster = "age-1",
             year = "2021",
             Habitat = "Main_Side",
             mean_Flow_cms = mean(mean_Flow_cms, na.rm = TRUE),
             mean_Temp_C = mean(mean_Temp_C, na.rm = TRUE))), 
             mod = age1_cand_mod) 

# Age 1 2022
age1_2022 <- pred_gam(df = with(age_1,
             data.frame(doy = unique(doy),
             age_cluster = "age-1",
             year = "2022",
             Habitat = "Main_Side",
             mean_Flow_cms = mean(mean_Flow_cms, na.rm = TRUE),
             mean_Temp_C = mean(mean_Temp_C, na.rm = TRUE))), 
             mod = age1_cand_mod) 


all_preds <- rbind(age0_2022, age0_2021, age1_2022, age1_2021)

# Day of Year and TL
(len_plot <- ggplot() +
    # Need to have a dummy for this to work
    annotate(geom = "rect", xmin = 210, xmax = 303, ymin = -Inf, ymax = Inf,
             fill = 'grey', alpha = 0.5, lty = 2, size = 0.85)+
    
    # Points
    geom_jitter(mt_all, mapping = aes(x = doy, y = TL, color = factor(year),
                                      shape = factor(year)), alpha = 0.2, size = 3) +
    # Bioenergetics
    geom_line(bioe_df_long %>% filter(Type == "Length"),
              mapping = aes(x = doy, y = growth, color = year), alpha = 1, lty = 5,
              size = 1.5) +
    # GAM predictions
    geom_line(all_preds, mapping = aes(x = doy, y = fitted, color = year), size = 1.5) +
    geom_ribbon(all_preds, mapping = aes(x = doy, y = fitted, ymin = lower, ymax = upper,
                                         fill = year), alpha = 0.45) +
  # Aesthetics
  scale_color_manual(values = c("#0073C299", "#A7303099")) +
  scale_fill_manual(values = c("#0073C299", "#A7303099")) +
  facet_wrap(~age_cluster, scales = "free_x") +
  theme_bw() +
  labs(x = "", y = "Total Length (mm)", color = "Year", shape = "Year", fill = "Year") +
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.position = c(0.055,0.75),
        strip.text = element_text(size = 17)) )

# GAM Results (Weight) -------------------------------------------------------------

# Load in models
load(here("output", "model_selection", "age_0_wt_models.RData"))
load(here("output", "model_selection", "age_1_wt_models.RData"))

# Look at best model formulation
mod_age0_wt_comp <- age_0_wt_models[[2]]
# Extract the best model (lowest AIC value)
age0_cand_wt_mod <- age_0_wt_models[[1]]$`model 10`
age0_cand_wt_mod$formula

# Look at best model formulation
mod_age1_wt_comp <- age_1_wt_models[[2]]
# Extract the best model (lowest AIC value)
age1_cand_wt_mod <- age_1_wt_models[[1]]$`model 17`
age1_cand_wt_mod$formula

# Age 0 2021
age0_2021_wt <- pred_gam(df = with(age_0,
                                data.frame(doy = unique(doy),
                                           age_cluster = "age-0",
                                           year = "2021",
                                           Habitat = "Main_Side",
                                           mean_Flow_cms = mean(mean_Flow_cms, na.rm = TRUE),
                                           mean_Temp_C = mean(mean_Temp_C, na.rm = TRUE))),  
                      mod = age0_cand_wt_mod) 

# Age 0 2022
age0_2022_wt <- pred_gam(df = with(age_0,
                                data.frame(doy = unique(doy),
                                           age_cluster = "age-0",
                                           year = "2022",
                                           Habitat = "Main_Side",
                                           mean_Flow_cms = mean(mean_Flow_cms, na.rm = TRUE),
                                           mean_Temp_C = mean(mean_Temp_C, na.rm = TRUE))),  
                      mod = age0_cand_wt_mod)


# Age 1 2021
age1_2021_wt <- pred_gam(df = with(age_1,
                                data.frame(doy = unique(doy),
                                           age_cluster = "age-1",
                                           year = "2021",
                                           Habitat = "Main_Side",
                                           mean_Flow_cms = mean(mean_Flow_cms, na.rm = TRUE),
                                           mean_Temp_C = mean(mean_Temp_C, na.rm = TRUE))), 
                      mod = age1_cand_wt_mod) 

# Age 1 2022
age1_2022_wt <- pred_gam(df = with(age_1,
                                data.frame(doy = unique(doy),
                                           age_cluster = "age-1",
                                           year = "2022",
                                           Habitat = "Main_Side",
                                           mean_Flow_cms = mean(mean_Flow_cms, na.rm = TRUE),
                                           mean_Temp_C = mean(mean_Temp_C, na.rm = TRUE))), 
                      mod = age1_cand_wt_mod) 


all_preds_wt <- rbind(age0_2022_wt, age0_2021_wt, age1_2022_wt, age1_2021_wt)

# Day of Year and TL
(wt_plot <- ggplot() +
    
    # Need to have a dummy for this to work
    annotate(geom = "rect", xmin = 210, xmax = 303, ymin = -Inf, ymax = Inf,
              fill = 'grey', alpha = 0.5, lty = 2, size = 0.85)+
    
    # Points
    geom_jitter(mt_all, mapping = aes(x = doy, y = Weight, color = factor(year),
                                      shape = factor(year)), alpha = 0.2, size = 3) +
    # Bioenergetics
    geom_line(bioe_df_long %>% filter(Type == "Mass"),
              mapping = aes(x = doy, y = growth, color = year), alpha = 1, lty = 5,
              size = 1.5) +
    # GAM predictions
    geom_line(all_preds_wt, mapping = aes(x = doy, y = fitted, color = year), size = 1.5) +
    geom_ribbon(all_preds_wt, mapping = aes(x = doy, y = fitted, ymin = lower, ymax = upper,
                                         fill = year), alpha = 0.45) +
    
    # Bioenergetic Simulations
  geom_line(bioe_df_long %>% filter(Type == "Weight"),
              mapping = aes(x = doy, y = growth, color = year), alpha = 0.8, lty = 2,
              size = 1.5) +
    
    # Plot aesthetics
  scale_color_manual(values = c("#0073C299", "#A7303099")) +
  scale_fill_manual(values = c("#0073C299", "#A7303099")) +
  facet_wrap(~age_cluster, scales = "free_x") +
  theme_bw() +
  labs(x = "Day of Year", y =  "Weight (g)", color = "Year", shape = "Year", fill = "Year") +
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(size = 17)) )


# Plot both length and weight
png(filename = here("figs", "LW_GAM.png"), width = 1000, height = 800)
cowplot::plot_grid(len_plot,  wt_plot, ncol = 1, rel_heights = c(1,0.9), 
          align = "hv", axis = "bl")
dev.off()

