# Load libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

# Water pot data
data1 <- read.csv("/Users/emmarigatti/Desktop/GitHub/soil_moisture_pd/Field Data 10312024 - Pressure Bomb.csv")
data1$datetime <- as.POSIXct(
  paste("2024/10/31", data1$Time),
  format = "%Y/%m/%d %H:%M",
  tz = "UTC"
)
data1 <- data1[,-2]

# For October
data1 <- data1 %>%
  filter(datetime <= as.POSIXct("2024/10/31 08:00:00", tz = "UTC"))

# For January
data1 <- data1 %>%
  filter(Sunlit.Shaded == "Pre-dawn")


# Soil moisture data

nf_mid <- read.csv("/Users/emmarigatti/Desktop/GitHub/soil_moisture_pd/nf_mid.csv")
sf_bottom <- read.csv("/Users/emmarigatti/Desktop/GitHub/soil_moisture_pd/sf_bottom.csv")
sf_top <- read.csv("/Users/emmarigatti/Desktop/GitHub/soil_moisture_pd/sf_top.csv")
power_box <- read.csv("/Users/emmarigatti/Desktop/GitHub/soil_moisture_pd/power_box.csv")

soil_list <- list(nf_mid, sf_bottom, sf_top, power_box)
names(soil_list) <- c("nf_mid", "sf_bottom", "sf_top", "power_box")

for (i in seq_along(soil_list)) {
   
  data <- soil_list[[i]]
  data <- data[,-c(14:20)]
  colnames(data)[1:13] <- c("datetime", paste0("Moisture_", 1:6), paste0("Temp_", 1:6))
  data$datetime <- as.POSIXct(data$datetime, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")

  start_date <- as.POSIXct("2024/10/31 05:00:00", tz = "UTC")
  end_date   <- as.POSIXct("2024/10/31 08:00:00", tz = "UTC")
  data <- data %>%
    filter(datetime >= start_date & datetime <= end_date)
  
  soil_list[[i]] <- data
  
  
}

list2env(soil_list, envir = .GlobalEnv)

# Separating by location

nf_mid_pot <- data1 %>%
  filter(Location == "North facing, mid slope")
nf_top_pot <- data1 %>%
  filter(Location == "North facing, top of slope")
sf_bottom_pot <- data1 %>%
  filter(Location == "South facing, bottom of slope")
sf_top_pot <- data1 %>%
  filter(Location == "South facing, top of slope")


# Change this for each location
soil_times  <- sf_bottom$datetime
pot_times <- sf_bottom_pot$datetime

nearest_idx <- vapply(
  pot_times,
  function(t) which.min(abs(as.numeric(soil_times - t))),
  integer(1)
)

sf_top_pot$soil_moisture <- sf_top[nearest_idx, 2]

# Clean up df

sf_combined_2 <- rbind(sf_bottom_pot, sf_top_pot)
sf_combined_2 <- sf_combined_1[,-c(3:5)]

# Plot 

ggplot(sf_combined_2, aes(x = factor(soil_moisture), 
                          y = Water.Potential..mPa., 
                          fill = factor(Tree.Number))) +
  geom_boxplot() +
  labs(
    x = "Soil moisture (%)",
    y = "Water potential (MPa)",
    fill = "Tree Number",
    title = "Water potential vs soil moisture",
    subtitle = "October 31, 2024"
  ) +
  theme_minimal()
