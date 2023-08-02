
# 2021-2022 Coho length-frequency analysis -------------------------------------
# Kevin Fitzgerald
# Fall 2022

#upload packages

library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)
library(here)
library(readxl)
library(lubridate)

# Read in Data -----------------------------------------------------------------

cap.dat.21 <- read.csv(here("data", "2021 Coho Capture Data.csv"))
cap.dat.22 <- read_xlsx(here("data", '2022 Coho Capture Data.xlsx'))

# Clean The Data ---------------------------------------------------------------

# filter the data to only cohos
cohos.21 <- cap.dat.21 %>% 
  filter(Species == "Coho",
         !is.na(TL),
         TL > 28) %>% 
  mutate(date = mdy(Date),
         doy = yday(date),
         TL = as.numeric(ifelse(TL == "N/A",NA,TL)),
         age_cluster = NA) # Creating variable to fill in using loop

cohos.22 <- cap.dat.22 %>% 
  filter(Species == "Coho",
         !is.na(TL),
         TL > 28) %>% 
  mutate(date = ymd(Date),
         doy = yday(date),
         TL = as.numeric(ifelse(TL == "N/A",NA,TL)),
         age_cluster = NA) # Creating variable to fill in using loop

# take a look at the length distributiins of each 

# ggplot(data = cohos.21, aes(x = Date, y = TL))+
#   geom_point()
# 
# ggplot(data = cohos.22, aes(x = doy, y = TL))+
#   geom_point()

# Now assign age classes

# First, create a vector of the unique doy, that we will use to index and loop through
uniq_doy <- unique(cohos.21$doy)

# Create an empty dataframe to bind rows to
Mt.age.id.21 <- data.frame(matrix(ncol = ncol(cohos.21))) %>% 
  drop_na()
colnames(Mt.age.id.21) <- names(cohos.21) # Changing the names so they match up
# with the dataframe when we bind it later.


# Now run the loop to add ages for 2021

for (i in 1:length(uniq_doy)){
  # Testing
  # i <- 2
  
  # Next, incrementally index the unique doy
  un_doy <- uniq_doy[i]
  
  Mt_sub_day <- cohos.21 %>% filter(doy == un_doy) 
  # subsetting the dataframe here to the unique day
  
  if (un_doy == 125) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 60, "age-1", "age-2") 
    
  } # if un_doy == 125
  
  if (un_doy %in% c(131:146) & un_doy != 141) {
    
    # Now, let's set up the k-means alogrithim here to separate out age-1 and age-2
    age_clusters <- as.factor(kmeans(Mt_sub_day$TL, centers = 2,iter.max = 100, nstart = 100)$centers)
    
    centers <- sort(age_clusters)
    
    age_clusters <- kmeans(Mt_sub_day$TL, centers = centers,iter.max = 100, nstart = 100)$cluster
    
    age_clusters <- ifelse(age_clusters == "1", "age-1", "age-2")
    
    # Now, put these identified age clusters correctly into the corresponding TL rows
    Mt_sub_day$age_cluster <- age_clusters
    
  } # if the indexed day == 131:146 - differentiate age 1s and age 2s
  
  if (un_doy %in% c(141, 147:161)) {
    
    Mt_sub_day$age_cluster <- "age-1"
    
  } # These are classified during days where there are only age-1 captures
  
  if (un_doy %in% c(162:221, 230, 245:253, 274)) {
    
    # Now, let's set up the k-means alogrithim here to separate out age-1 and age-2
    age_clusters <- as.factor(kmeans(Mt_sub_day$TL, centers = 2,iter.max = 100, nstart = 100)$centers)
    
    centers <- sort(age_clusters)
    
    age_clusters <- kmeans(Mt_sub_day$TL, centers = centers,iter.max = 100, nstart = 100)$cluster
    
    age_clusters <- ifelse(age_clusters == "1", "age-0", "age-1")
    
    # Now, put these identified age clusters correctly into the corresponding TL rows
    Mt_sub_day$age_cluster <- age_clusters
    
  } # These are classified during days where there are both age-0s, and age-1s.
  
  if (un_doy == 224) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 80, "age-0", "age-1")
    
  }
  
  if (un_doy == 226) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 80, "age-0", "age-1")
    
  }
  
  if (un_doy == 237) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 85, "age-0", "age-1")
    
  }
  
  if (un_doy == 240) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 85, "age-0", "age-1")
    
  }
  
  if (un_doy == 257) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 90, "age-0", "age-1")
    
  }
  
  if (un_doy == 264) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 90, "age-0", "age-1")
    
  }
  
  if (un_doy == 266) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 90, "age-0", "age-1")
    
  }
  
  if (un_doy == 267) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 92, "age-0", "age-1")
    
  }
  
  if (un_doy == 275) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 92, "age-0", "age-1")
    
  }
  
  if (un_doy == 280) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 92, "age-0", "age-1")
    
  }
  
  if (un_doy == 285) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 92, "age-0", "age-1")
    
  }
  
  if (un_doy == 291) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 92, "age-0", "age-1")
    
  }
  
  if (un_doy == 301) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 92, "age-0", "age-1")
    
  }
  
  Mt.age.id.21<- rbind(Mt.age.id.21, Mt_sub_day)
  
} # next i


# Now for 2022

# First, create a vector of the unique doy, that we will use to index and loop through
uniq_doy <- unique(cohos.22$doy)

# Create an empty dataframe to bind rows to
Mt.age.id.22 <- data.frame(matrix(ncol = ncol(cohos.22))) %>% 
  drop_na()
colnames(Mt.age.id.22) <- names(cohos.22)
# Now run the loop to add ages for 2022


for (i in 1:length(uniq_doy)){
  # Testing
  # i <- 2
  
  # Next, incrementally index the unique doy
  un_doy <- uniq_doy[i]
  
  Mt_sub_day <- cohos.22 %>% filter(doy == un_doy) 
  # subsetting the dataframe here to the unique day
  if (un_doy == 127) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 85, "age-1", "age-2") 
    
  } # if the indexed day == 127:171 - differentiate age 1s and age 2s
  
  if (un_doy %in% c(149:171)) {
    
    Mt_sub_day$age_cluster <- "age-1"
    
  } # if un_doy == 177
  
  if (un_doy %in% c(171:203)) { 
    
    # We need to do this b/c there are only two obs, and we suspect we can't use kmeans
    Mt_sub_day$age_cluster <- ifelse(Mt_sub_day$TL < 58, "age-0", "age-1")
    
  }
  
  if (un_doy %in% c(203:277)) {
    
    # Now, let's set up the k-means alogrithim here to separate out age-1 and age-2
    age_clusters <- as.factor(kmeans(Mt_sub_day$TL, centers = 2,iter.max = 100, nstart = 100)$centers)
    
    centers <- sort(age_clusters)
    
    age_clusters <- kmeans(Mt_sub_day$TL, centers = centers,iter.max = 100, nstart = 100)$cluster
    
    age_clusters <- ifelse(age_clusters == "1", "age-0", "age-1")
    
    # Now, put these identified age clusters correctly into the corresponding TL rows
    Mt_sub_day$age_cluster <- age_clusters

  }
  
  if (un_doy %in% c(278:365)) {
    
    # Now, let's set up the k-means alogrithim here to separate out age-1 and age-2
    age_clusters <- as.factor(kmeans(Mt_sub_day$TL, centers = 2,iter.max = 100, nstart = 100)$centers)
    
    centers <- sort(age_clusters)
    
    age_clusters <- kmeans(Mt_sub_day$TL, centers = centers,iter.max = 100, nstart = 100)$cluster
    
    age_clusters <- ifelse(age_clusters == "1", "age-0", "age-1")
    
    # Now, put these identified age clusters correctly into the corresponding TL rows
    Mt_sub_day$age_cluster <- age_clusters
    
  }

  Mt.age.id.22 <- rbind(Mt.age.id.22, Mt_sub_day)
  
} # next i

# Making sure that these are age 0s
Mt.age.id.22$age_cluster[Mt.age.id.22$doy %in% c(293:300) & Mt.age.id.22$TL <= 75] <- "age-0"

# Output these files as csvs
write.csv(Mt.age.id.21, here("output", "MT_age_21.csv"), row.names = FALSE)
write.csv(Mt.age.id.22, here("output", "MT_age_22.csv"), row.names = FALSE)

# 
# # Visualize --------------------------------------------------------------------
# 
# 
# lf.21 <- ggplot(Mt.age.id.21 %>% filter(!is.na(TL)) %>% 
#                   mutate(age_cluster = factor(age_cluster, 
#                                               levels = c("age-2", "age-1","age-0"))), 
#                 mapping = aes(x = mdy(Date), y = TL, color = age_cluster, 
#                               shape = age_cluster))+
#   geom_smooth(Mt.age.id.21 %>%
#                 filter(!is.na(TL)) %>%
#                 mutate(age_cluster = factor(age_cluster,
#                                             levels = c("age-1","age-0"))),
#               mapping = aes(x = mdy(Date), y = TL), method = "loess", se = FALSE)+
#   geom_point(alpha = .5, size = 2.5)+
#   labs(x  = "Sample Date", y = "Total Length (mm)", color = "Cohort")+
#   scale_x_date(date_labels="%b",date_breaks  ="1 month")+
#   ggtitle("2021 Coho Length Scatter Plot")+
#   labs(color = 1, shape = 1)+
#   labs(color = "Cohort", 
#        shape = "Cohort")+
#   scale_shape_manual(values = c(19, 17, 15))+
#   theme_light()+
#   theme(legend.position = "none", 
#         legend.key = element_rect(colour = "white"),
#         plot.title = element_text(hjust = 0.5), 
#         text = element_text(size=12))
# 
# lf.22 <- ggplot(Mt.age.id.22 %>% filter(!is.na(TL)) %>% 
#                   mutate(age_cluster = factor(age_cluster, 
#                                               levels = c("age-2", "age-1","age-0"))), 
#                 mapping = aes(x = Date, y = TL, color = age_cluster, 
#                               shape = age_cluster))+
#   geom_smooth(Mt.age.id.22 %>% 
#                 filter(!is.na(TL)) %>% 
#                 mutate(age_cluster = factor(age_cluster,
#                                             levels = c("age-1","age-0"))),
#               mapping = aes(x = Date, y = TL), method = "loess", se = FALSE)+
#   geom_point(alpha = .5, size = 2.5)+
#   labs(x  = "Sample Date", y = "Total Length (mm)", color = "Cohort")+
#   # scale_x_date(date_labels="%b",date_breaks  ="1 month")+
#   ggtitle("2022 Coho Length Scatter Plot")+
#   labs(color = 1, shape = 1)+
#   labs(color = "Cohort", 
#        shape = "Cohort")+
#   scale_shape_manual(values = c(19, 17, 15))+
#   theme_light()+
#   theme(legend.position = "top", 
#         legend.key = element_rect(colour = "white"),
#         plot.title = element_text(hjust = 0.5), 
#         text = element_text(size=12))
# 
# 
# gc.viz <- plot_grid(lf.21, lf.22, ncol = 2, nrow = 1, align = 'hv', 
#                     axis = 'l', rel_heights = c(0.5, 0.5))



