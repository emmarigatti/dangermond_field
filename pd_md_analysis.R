#----------------------------------------------------------------------- Load libraries

library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(purrr)

#----------------------------------------------------------------------- Set working directory
setwd("~/Desktop/GitHub/dangermond_field/pressure_bomb")

#----------------------------------------------------------------------- Read in data
psi_data <- read.csv("20250904_psi.csv")
psi_data <- psi_data %>% filter(Tree != 88)

#----------------------------------------------------------------------- Group tree by location and split into leaf and stem

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
    n        = n(),
    mean_tlp = mean(TLP, na.rm = TRUE),
    sd_tlp = sd(TLP, na.rm = TRUE),
    se_tlp   = sd_tlp / sqrt(n())
    
  )

#----------------------------------------------------------------------- Box plot

ggplot(
  psi_data %>% filter(Time == "md"),
  aes(x = factor(Group), y = Water.pot, fill = Organ)
) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("Leaf" = "#1b9e77", "Stem" = "#d95f02")) +
  geom_point() +
  labs(
    x = "Group",
    y = "Water Potential (MPa)",
    fill = "Time",
    title = "Leaf vs Stem Middays",
    subtitle = "By location across a hillslope gradient"
  ) +
  theme_minimal()
#----------------------------------------------------------------------- Distribution

ggplot(
  psi_data %>%
    filter(Organ == "Stem", Time %in% c("pd", "md"), !is.na(Water.pot)),
  aes(x = Water.pot, fill = Time)
) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Stem Midday vs Predawn",
    x = "Water Potential (MPa)",
    y = "Density",
    fill = "Time"
  ) +
  #scale_fill_manual(values = c("Leaf" = "#1b9e77", "Stem" = "#d95f02")) +
  theme_minimal(base_size = 12)



#----------------------------------------------------------------------- Difference between pd and md

delta_psi_time <- psi_data %>%
  pivot_wider(names_from = Time, values_from = Water.pot) %>%
  mutate(delta_psi = md - pd)

delta_psi_organ <- psi_data %>%
  pivot_wider(names_from = Organ, values_from = Water.pot) %>%
  mutate(delta_psi = Leaf - Stem)

ggplot(delta_psi_organ, aes(x = factor(Group), y = delta_psi, fill = Time)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  #scale_fill_manual(values = c("Leaf" = "#1b9e77", "Stem" = "#d95f02")) +
  labs(
    x = "Group",
    y = expression(Delta*psi),
    fill = "Time",
    title = expression(Delta*psi~"Water Potential"),,
    subtitle = "Leaf - Stem water potential"
  ) +
  theme_minimal()
 #----------------------------------------------------------------------- Difference between pd and md 

psi_wide <- psi_data %>%
  mutate(Organ = factor(Organ, levels = c("Leaf", "Stem"))) %>%
  pivot_wider(
    names_from = Time,
    values_from = Water.pot
  )

psi_wide <- psi_data %>%
  select(Tree, Group, Time, Organ, Water.pot) %>%
  pivot_wider(names_from = Organ, values_from = Water.pot) %>%  # Leaf & Stem columns
  filter(is.finite(Leaf), is.finite(Stem))

ggplot(psi_wide, aes(x = Stem, y = Leaf, color = Group)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Time) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    x = "Stem",
    y = "Leaf",
    title = "Stem–Leaf Coordination Across Hillslope Gradient"
  ) +
  theme_minimal()

#----------------------------------------------------------------------- TLP stuff

tlp_leaf <- read.csv("/Users/emmarigatti/Desktop/GitHub/pv_curves/csvs/sept2025/tlp_summary_leaf.csv")
tlp_leaf <- tlp_leaf[,-1]
tlp_leaf[5, 1] <- "098"

tlp_stem <- read.csv("/Users/emmarigatti/Desktop/GitHub/pv_curves/csvs/sept2025/tlp_summary_stem.csv")
tlp_stem <- tlp_stem[,-1]
tlp_stem[5, 1] <- "098"

series_leaf <- readRDS("/Users/emmarigatti/Desktop/GitHub/pv_curves/csvs/sept2025/tlp_summary_leaf.rds")
series_stem <- readRDS("/Users/emmarigatti/Desktop/GitHub/pv_curves/csvs/sept2025/tlp_summary_stem.rds")

#----------------------------------------------------------------------- Join to psi data

tlp_leaf <- tlp_leaf %>%
  mutate(Tree = sprintf("%03d", as.integer(Tree)))


tlp_stem <- tlp_stem %>%
  mutate(Tree = sprintf("%03d", as.integer(Tree)))

psi_data <- psi_data %>%
  mutate(Tree = sprintf("%03d", as.integer(Tree)))

psi_data <- psi_data %>%
  # Join in stem and leaf TLP tables
  left_join(tlp_stem %>% select(Tree, TLP_stem = TLP), by = "Tree") %>%
  left_join(tlp_leaf %>% select(Tree, TLP_leaf = TLP), by = "Tree") %>%
  # Single TLP column: pick stem if Organ == Stem, leaf if Organ == Leaf
  mutate(TLP = case_when(
    Organ == "Stem" ~ TLP_stem,
    Organ == "Leaf" ~ TLP_leaf,
    TRUE ~ NA_real_
  )) %>%
  select(-TLP_stem, -TLP_leaf)

#----------------------------------------------------------------------- Plot TLP by slope separated into Leaf and Stem
ggplot(
  psi_data %>%
    filter(!is.na(TLP), Time == "pd") %>%
    mutate(Organ = factor(Organ, levels = c("Leaf", "Stem"))),
  aes(x = Group, y = TLP, fill = Organ)
) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.7, outlier.shape = NA) +
  geom_jitter(aes(group = interaction(Group, Organ)),
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
              alpha = 0.5, size = 1.6) +
  labs(x = "Group", y = "TLP (MPa)", fill = "Organ", title = "TLP by Slope Position") +
  scale_fill_manual(
    values = c("Leaf" = "#1b9e77", "Stem" = "#d95f02")
  ) +
  scale_y_reverse() +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.minor = element_blank())

#----------------------------------------------------------------------- Distribution of leaf TLPs
ggplot(tlp_leaf, aes(x = TLP)) +
  geom_density(fill = "#1b9e77", alpha = 0.6) +
  scale_x_reverse() +  
  labs(
    title = "Leaf TLPs",
    x = "TLP (MPa)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12)

ggplot(tlp_stem, aes(x = TLP)) +
  geom_density(fill = "#d95f02", alpha = 0.6) +
  scale_x_reverse() +  
  labs(
    title = "Stem TLPs",
    x = "TLP (MPa)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12)

#----------------------------------------------------------------------- Plot both densities together
tlp_leaf2 <- tlp_leaf %>%
  mutate(Organ = "Leaf")

tlp_stem2 <- tlp_stem %>%
  mutate(Organ = "Stem")

tlp_all <- bind_rows(tlp_leaf2, tlp_stem2)

ggplot(tlp_all, aes(x = TLP, fill = Organ)) +
  geom_density(alpha = 0.6) +
  scale_x_reverse() +
  labs(
    title = "Leaf vs Stem TLPs",
    x = "TLP (MPa)",
    y = "Density",
    fill = "Organ"
  ) +
  scale_fill_manual(values = c("Leaf" = "#1b9e77", "Stem" = "#d95f02")) +
  theme_minimal(base_size = 12)

#----------------------------------------------------------------------- Plot pd and md by TLP

psi_binned <- psi_data %>%
  filter(Organ == "Stem") %>%
  mutate(
    TLP_abs = abs(TLP), 
    TLP_bin = cut(TLP_abs,
                  breaks = seq(0, 5, by = 1),    
                  labels = c("0–1", "1–2", "2–3", "3–4", "4–5"),
                  right = FALSE, include.lowest = TRUE)
  )

ggplot(psi_binned, aes(x = TLP_bin, y = Water.pot, fill = Time)) +
  geom_boxplot(outlier.shape = NA, width = 0.7, alpha = 0.7) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.7),
              alpha = 0.6, size = 1.8) +
  labs(
    title = "Stem Predawn Water Potentials by TLP",
    x = "TLP",
    y = "Predawn",
    fill = "Time"
  ) +
  #scale_fill_manual(values = c("Leaf" = "#1b9e77", "Stem" = "#d95f02")) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

#----------------------------------------------------------------------- Plot pd and md vs TLP in 1:1

org <- psi_data %>%
  filter(Organ == "Leaf", Time %in% c("pd", "md")) %>%
  filter(is.finite(TLP), is.finite(Water.pot))

ggplot(org, aes(x = abs(TLP), y = Water.pot, color = Group)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  facet_wrap(~ Time) +
  labs(
    title = "Leaf Predawn and Midday vs TLP",
    x = "TLP",
    y = "MPa",
    color = "Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

#----------------------------------------------------------------------- 1:1 binned by TLP
# Faceted by time
tlp_key <- psi_data %>%
  filter(Organ == "Stem", Time == "pd") %>%
  select(Tree, TLP_pd = TLP) %>%
  distinct()

psi_wide <- psi_data %>%
  select(Tree, Time, Organ, Water.pot) %>%
  pivot_wider(names_from = Organ, values_from = Water.pot) %>%
  filter(is.finite(Leaf), is.finite(Stem)) %>%
  left_join(tlp_key, by = "Tree") %>%
  mutate(
    TLP_abs = abs(TLP_pd),
    TLP_bin = cut(
      TLP_abs,
      breaks = seq(0, 5, 1),
      labels = c("0–1", "1–2", "2–3", "3–4", "4–5"),
      right = FALSE, include.lowest = TRUE
    )
  )

ggplot(psi_wide, aes(x = pd, y = md, color = TLP_bin)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Time) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    x = "Predawn",
    y = "Midday",
    color = "Leaf TLP bin",
    title = "Stem–Leaf Coordination Binned by Stem TLP"
  ) +
  theme_minimal()

# Faceted by Organ
tlp_key <- psi_data %>%
  filter(Organ == "Stem", Time == "pd") %>%
  select(Tree, TLP_pd = TLP) %>%
  distinct()

psi_timewide <- psi_data %>%
  select(Tree, Organ, Time, Water.pot) %>%
  tidyr::pivot_wider(names_from = Time, values_from = Water.pot) %>%
  dplyr::filter(is.finite(pd), is.finite(md)) %>%
  dplyr::left_join(tlp_key, by = "Tree") %>%
  dplyr::mutate(
    TLP_abs = abs(TLP_pd),
    TLP_bin = cut(TLP_abs,
                  breaks = seq(0, 5, 1),
                  labels = c("0–1","1–2","2–3","3–4","4–5"),
                  right = FALSE, include.lowest = TRUE)
  )

ggplot(psi_timewide, aes(x = pd, y = md, color = TLP_bin)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Organ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    x = "Predawn",
    y = "Midday",
    color = "Stem TLP bin",
    title = "Pd-Md Coordination Binned by Stem TLP"
  ) +
  theme_minimal(base_size = 13)

#----------------------------------------------------------------------- 1:1 binned by TLP