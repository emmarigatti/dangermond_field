# Load libraries

library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

# Set working directory
setwd("~/Desktop/GitHub/dangermond_field/pressure_bomb")

# Read in data
psi_data <- read.csv("20250812_psi.csv")

# Group tree by location and split into leaf and stem

psi_data <- psi_data %>%
  mutate(Group = case_when(
    N.S == "SF" & Location == "Top"    ~ "SF Top",
    N.S == "SF" & Location == "Bottom" ~ "SF Bottom",
    N.S == "NF" & Location == "Top"    ~ "NF Top",
    N.S == "NF" & Location == "Mid"    ~ "NF Mid",
    TRUE ~ NA_character_
  ))

summary <- psi_data %>%
  group_by(Group, Time) %>%
  summarise(
    mean_psi = mean(Water.pot, na.rm = TRUE),
    sd_psi   = sd(Water.pot, na.rm = TRUE),
    se_psi   = sd_psi / sqrt(n()),
    n        = n()
  )

# Box plot

ggplot(
  psi_data %>% filter(Organ == "Leaf"),
  aes(x = factor(Group), y = Water.pot, fill = Time)
) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
 # scale_fill_manual(
  #  values = c("Leaf" = "#1b9e77", "Stem" = "#d95f02")  
  #) +
  labs(
    x = "Group",
    y = "Water Potential (MPa)",
    fill = "Time",
    title = "Predawn and Midday Leaf Water Potentials",
    subtitle = "By location across a hillslope gradient"
  ) +
  theme_minimal()

# Difference between pd and md

delta_psi_time <- psi_data %>%
  pivot_wider(names_from = Time, values_from = Water.pot) %>%
  mutate(delta_psi = md - pd)

delta_psi_organ <- psi_data %>%
  pivot_wider(names_from = Organ, values_from = Water.pot) %>%
  mutate(delta_psi = Stem - Leaf)

ggplot(delta_psi_time, aes(x = factor(Group), y = delta_psi, fill = Organ)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = c("Leaf" = "#1b9e77", "Stem" = "#d95f02")  
  ) +
  labs(
    x = "Group",
    y = expression(Delta*psi),
    fill = "Organ",
    title = expression(Delta*psi~"Water Potential"),,
    subtitle = "Midday - Predawn water potential"
  ) +
  theme_minimal()
  

psi_wide <- psi_data %>%
  mutate(Time = factor(Time, levels = c("pd", "md"))) %>%
  pivot_wider(
    names_from = Organ,
    values_from = Water.pot
  )

ggplot(psi_wide, aes(x = Stem, y = Leaf, color = Group)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Time) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    x = expression(psi[stem]~"(MPa)"),
    y = expression(psi[leaf]~"(MPa)"),
    title = "Stem–Leaf Coordination Across Hillslope Gradient"
  ) +
  theme_minimal()

