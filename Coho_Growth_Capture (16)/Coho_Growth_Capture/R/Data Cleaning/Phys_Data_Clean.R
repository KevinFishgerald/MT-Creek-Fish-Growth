# Purpose: To clean physical chemical data and join to coho dataframes
# Creator: Matthew LH. Cheng
# Date: 12/5/22


# Set up ------------------------------------------------------------------

library(here)
library(tidyverse)
library(readxl)
library(lubridate)

# Load in physical data 
# Flow 2021
mt_flow <- read_xlsx(here("data", "phys_dat", "Montana_flow_2013_2022.xlsx"), skip = 2,
                     sheet = "Montana Bridge")

# load in modelled flow from 2022 via overlord fellman
mt_flow_2022 <- read_xlsx(here("data", "phys_dat", "Montana_Flow_May-Oct_2022.xlsx"))

# Temp 2021
mt_2021_temp <- read_xlsx(here("data", "phys_dat", "Montana_bridge_temp_2021.xlsx"))

# Temp 2022
mt_2022_temp <- read_xlsx(here("data", "phys_dat", "MontanaCr_2022_Temp.xlsx"))

# Load in coho capture data
# 2021
mt_age_21 <- read.csv(here("output", "MT_age_21.csv")) %>% 
  mutate(year = 2021) %>% 
  filter(TL <= 150) %>% 
  select(year, date, doy, PIT_tag, Habitat, age_cluster, FL, TL, Weight)

# 2022
mt_age_22 <- read.csv(here("output", "MT_age_22.csv")) %>% 
  mutate(year = 2022) %>%  
  filter(TL <= 150) %>% 
  select(year, date, doy, PIT_tag, Habitat, age_cluster, FL, TL, Weight)

# Bind these together
mt_age_all <- rbind(mt_age_21, mt_age_22)


# Data Cleaning -----------------------------------------------------------

# Convert mt_flow 2022 to daily mean
mt_flow_2022 <- mt_flow_2022 %>% 
  rename(Flow_cms = `Q cms`) %>% 
  mutate(Date = substr(Date, 1, 10),
         Date = ymd(Date), # Change to date format
         doy = yday(Date)) %>% 
  group_by(Date, doy) %>% 
  summarize(Flow_cms = mean(Flow_cms)) %>% 
  mutate(year = year(Date))

# First, make sure the dates, times, and col names are all in the same format
mt_flow <- mt_flow %>% 
  rename(Date = ...1,
         Flow_cms = 'cms') %>% 
  mutate(Date = substr(Date, 1, 10),
    Date = ymd(Date), # Change to date format
         doy = yday(Date),
    year = year(Date)) %>% 
  filter(year != 2022) %>% 
  rbind(mt_flow_2022) %>% 
  dplyr::select(-year) # remove year from this for consistency with other dataframes

# Write this csv out
write.csv(mt_flow, here("output", "mt_flow_all.csv"))

# MT Creek 2021 Temp
mt_2021_temp <- mt_2021_temp %>% 
  mutate(Time = substr(Time, 12, 16), # extract out the times
         Date = paste(Date, Time), # paste date and time together
         Date = ymd_hm(Date), # coerce to date format
         doy = yday(Date)) %>%  # day of year
  dplyr::select(Date, 'Temp C', doy) %>% 
  rename(Temp_C = 'Temp C')

# MT Creek 2022 Temp
mt_2022_temp <- mt_2022_temp %>% 
  mutate(Time = substr(Time, 12, 16), # extract out the times
         Date = paste(Date, Time), # paste date and time together
         Date = ymd_hm(Date), # coerce to date format
         doy = yday(Date)) %>%   # day of year
  dplyr::select(Date, 'Temperature C', doy) %>% 
  rename(Temp_C = 'Temperature C')

# Bind flow and temp together
temp <- rbind(mt_2021_temp, mt_2022_temp)

# Write this csv out
write.csv(temp, here("output", "mt_temp_all.csv"))

# Summarize mean flow and temp by day and year and left join to the datasets
flow_mean <- mt_flow %>% 
  group_by(doy, year(Date)) %>% 
  summarize(mean_Flow_cms = mean(Flow_cms, na.rm = TRUE)) %>% 
  rename(year = `year(Date)`) %>% 
  filter(year %in% c(2021:2022))
  
# Tempearture
temp_mean <- temp %>% 
  group_by(doy, year(Date)) %>% 
  summarize(mean_Temp_C = mean(Temp_C, na.rm = TRUE)) %>% 
  rename(year = `year(Date)`)

# Now, left join these to our age cluster dataframe and
# output it as a csv
mt_age_all <- mt_age_all %>% 
  left_join(temp_mean, by = c("year", "doy")) %>% 
  left_join(flow_mean, by = c("year", "doy"))

# mt_age_all %>% 
#   group_by(year, doy) %>% 
#   summarize(is.na(mean_Temp_C)) %>% 
#   view()

# Output this csv
write.csv(mt_age_all, here("output", "MT_Age_Phys_Dat.csv"))

# Quick visualizations ----------------------------------------------------

# Flow plot
ggplot(mt_flow, aes(x = Date, y = Flow_cms)) +
  geom_line() +
  theme_bw()  

# Do mean flow to smooth out
flow_mean %>% 
  filter(doy >= 200, doy <= 280) %>% 
  ggplot(aes(x = doy, y = mean_Flow_cms, color = factor(year))) +
  geom_line() +
  theme_bw() +
  facet_wrap(~year, scales = "free_x")

# Temp plot
ggplot(temp, aes(x = Date, y = Temp_C)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~year(Date), scales = "free_x")

# Do mean temp to smooth out
temp_mean %>% 
  ggplot(aes(x = doy, y = mean_Temp_C, color = factor(year))) +
  geom_line(alpha = 0.5) +
  geom_smooth() +
  theme_bw() 
