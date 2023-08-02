# Creator: Matthew LH. Cheng
# Date: 12.21.22
# Purpose: To plot pinks presence and absence bioenergetics


# Set Up ------------------------------------------------------------------

library(here)
library(tidyverse)
library(readxl)

# load in data
bio_df <- readxl::read_xlsx(here("data", "pink_pres_abs_bioenergetics.xlsx"))

# Pivot longer
bio_df_long <- bio_df %>% 
  pivot_longer(cols = c("2021", "2022"), names_to = "year", values_to = "growth") %>% 
  rename(doy = `Julian Day`)


# Visualize ---------------------------------------------------------------

ggplot(bio_df_long, aes(x = doy, y = growth, color = year)) +
  geom_line(size = 2, alpha = 0.9) +
  theme_bw() +
  labs(x = "Day of Year", y = "TL (mm)", color = "Year")
