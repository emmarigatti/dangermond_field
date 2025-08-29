# Load libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(patchwork)

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

nf_mid <- read.csv("/Users/emmarigatti/Desktop/GitHub/dangermond_field/soil_moisture/nf_mid.csv")
sf_bottom <- read.csv("/Users/emmarigatti/Desktop/GitHub/dangermond_field/soil_moisture/sf_bottom.csv")
sf_top <- read.csv("/Users/emmarigatti/Desktop/GitHub/dangermond_field/soil_moisture/sf_top.csv")

soil_list <- list(nf_mid, sf_bottom, sf_top)
names(soil_list) <- c("nf_mid", "sf_bottom", "sf_top")

for (i in seq_along(soil_list)) {
   
  data <- soil_list[[i]]
  data <- data[,-c(14:20)]
  colnames(data)[1:13] <- c("datetime", paste0("Moisture_", 1:6), paste0("Temp_", 1:6))
  data$datetime <- as.POSIXct(data$datetime, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")

  start_date <- as.POSIXct("2025/08/12 04:45:00", tz = "UTC")
  end_date   <- as.POSIXct("2025/08/312 13:00:00", tz = "UTC")
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

#----------------------------------------------------

nf_mid_pd <- nf_mid[34,3] - nf_mid[1,3]
sf_top_pd <- sf_top[34,3] - sf_top[1,3]
sf_bottom_pd <- sf_bottom[34,3] - sf_bottom[1,3]

delta_psi_time <- delta_psi_time %>%
  mutate(
    Group = case_when(
      N.S == "SF" & Location == "Top"    ~ "SF Top",
      N.S == "SF" & Location == "Bottom" ~ "SF Bottom",
      N.S == "NF" & Location == "Top"    ~ "NF Top",
      N.S == "NF" & Location == "Mid"    ~ "NF Mid",
      TRUE ~ NA_character_
    ),
    RefValue = case_when(
      Group == "NF Mid"    ~ nf_mid_pd,
      Group == "SF Top"    ~ sf_top_pd,
      Group == "SF Bottom" ~ sf_bottom_pd,
      TRUE ~ NA_real_
    )
  )

ggplot(
  delta_psi_time %>% 
    filter(Organ == "Leaf"),
  aes(x = RefValue, y = delta_psi, fill = Group)
) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "Soil Moisture (%)",
    y = "Leaf Water Potential (MPa)",
    fill = "Time",
    title = "Change in Water Potential vs Change in Soil Moisture at 20cm",
    subtitle = "Leaf measurements"
  ) +
  theme_minimal()
ggplot(
  delta_psi_time %>% 
    filter(Organ == "Stem"),
  aes(x = RefValue, y = delta_psi, fill = Group)
) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "Soil Moisture (%)",
    y = "Stem Water Potential (MPa)",
    fill = "Time",
    title = "Change in Water Potential vs Change in Soil Moisture at 20cm",
    subtitle = "Stem measurements"
  ) +
  theme_minimal()


