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
  psi_data %>% filter(Time == "pd"),
  aes(x = factor(Group), y = Water.pot, fill = Organ)
) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = c("Leaf" = "#1b9e77", "Stem" = "#d95f02")  
  ) +
  geom_point()
  labs(
    x = "Group",
    y = "Water Potential (MPa)",
    fill = "Time",
    title = "Predawn Stem and Leaf Water Potentials",
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
  mutate(Organ = factor(Organ, levels = c("Leaf", "Stem"))) %>%
  pivot_wider(
    names_from = Time,
    values_from = Water.pot
  )

ggplot(psi_wide, aes(x = md, y = pd, color = Group)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Organ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    x = "Pre-dawn",
    y = "Midday",
    title = "Stem–Leaf Coordination Across Hillslope Gradient"
  ) +
  theme_minimal()

# TLP stuff

nm  <- ls(pattern = "^tlp_(leaf|stem)_\\d+$")   
val <- mget(nm, inherits = TRUE)                

tlp_lookup <- tibble(name = nm, TLP = unlist(val)) %>%
  mutate(
    Organ_raw = stringr::str_match(name, "^tlp_(leaf|stem)_\\d+$")[,2],
    Tree_chr  = stringr::str_match(name, "^tlp_(?:leaf|stem)_(\\d+)$")[,2],
    Organ     = stringr::str_to_title(Organ_raw),  
    Tree      = as.integer(Tree_chr)                
  ) %>%
  dplyr::select(Tree, Organ, TLP)

psi_data <- psi_data %>%
  mutate(
    Tree  = as.integer(as.character(Tree)), 
    Organ = stringr::str_to_title(stringr::str_trim(Organ))
  ) %>%
  left_join(tlp_lookup, by = c("Tree", "Organ"))

psi_data$TLP <- psi_data$TLP * -1

tlp_per_tree <- psi_data %>%
  filter(!is.na(TLP), Organ %in% c("Leaf","Stem")) %>%
  distinct(Tree, Group, Organ, TLP) %>%
  mutate(Tree = factor(sprintf("%03d", Tree)))
psi_data_clean <- subset(psi_data, !is.na(TLP))
psi_data_clean <- psi_data %>%
  filter(!is.na(TLP))

ggplot(psi_data_clean, aes(x = factor(Group), y = TLP, color = Organ)) +
  geom_point() +
  scale_color_manual(
    values = c("Leaf" = "#1b9e77", "Stem" = "#d95f02")  
  ) +
  labs(
    x = "Tree",
    y = expression(psi),
    color = "Organ",
    title = "TLP",
  ) +
  theme_minimal()





library(dplyr)
library(ggplot2)

# 1) Predawn water potentials (boxes)
pd <- psi_data %>%
  filter(Time == "md", Organ %in% c("Leaf","Stem")) %>%
  mutate(Group = factor(Group))

# 2) One TLP per Group × Organ (mean across trees in the group)
tlp_avg <- psi_data_clean %>%
  filter(!is.na(TLP), Organ %in% c("Leaf","Stem")) %>%
  group_by(Group, Organ) %>%
  summarise(TLP = mean(TLP, na.rm = TRUE), .groups = "drop") %>%
  mutate(Group = factor(Group))

# 3) Shared dodge so boxes and TLP points align
dodge <- position_dodge(width = 0.75)

ggplot() +
  # Boxplots of predawn water potential by Organ
  geom_boxplot(
    data = pd,
    aes(x = Group, y = Water.pot, fill = Organ),
    position = dodge,
    width = 0.6,
    outlier.shape = NA,
    alpha = 0.7
  ) +
  # (optional) raw predawn points
  # geom_point(
  #   data = pd,
  #   aes(x = Group, y = Water.pot, fill = Organ),
  #   position = dodge,
  #   size = 1.6,
  #   shape = 21,
  #   alpha = 0.4,
  #   stroke = 0
  # ) +
  # Mean TLP point per Group × Organ
  geom_point(
    data = tlp_avg,
    aes(x = Group, y = TLP, color = Organ),
    position = dodge,
    size = 3
  ) +
  scale_fill_manual(
    name = "Organ",
    values = c("Leaf" = "#1b9e77", "Stem" = "#d95f02")
  ) +
  scale_color_manual(
    name = "Organ",
    values = c("Leaf" = "#1b9e77", "Stem" = "#d95f02")
  ) +
  labs(
    x = "Group",
    y = "Water Potential (MPa)",
    title = "Midday Water Potentials",
    subtitle = "Dots = Mean TLP"
  ) +
  theme_minimal()

