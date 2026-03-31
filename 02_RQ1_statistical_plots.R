library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)
library(patchwork)
library(RColorBrewer)

# Custom color palette
color_primary <- "#2C5F7C"
color_secondary <- "#7FA99B"
color_highlight <- "#E67E22"
color_male <- "#3498DB"
color_female <- "#E74C3C"

# Publication theme
theme_publication <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_blank(),  # No titles
      plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5, margin = margin(b = 15)),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 11),
      legend.title = element_text(face = "bold", size = 11),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
      plot.margin = margin(10, 10, 10, 10)
    )
}

df <- read_excel("food_survey_clean_2.xlsx", sheet = 1)

# Create collapsed age groups
df <- df %>%
  mutate(age_collapsed = case_when(
    age == "18-24" ~ "18-24",
    age == "25-34" ~ "25-34",
    age == "35-44" ~ "35-44",
    age == "45-54" ~ "45-54",
    age %in% c("55-64", "65+") ~ "55+",
    TRUE ~ NA_character_
  ))

df$age_collapsed <- factor(df$age_collapsed, 
                           levels = c("18-24", "25-34", "35-44", "45-54", "55+"),
                           ordered = TRUE)

# Create collapsed income groups
df <- df %>%
  mutate(income_collapsed = case_when(
    income %in% c("Under £15,000", "£15,000-£24,999") ~ "<£25k",
    income %in% c("£25,000-£34,999", "£35,000-£49,999") ~ "£25-50k",
    income %in% c("£50,000-£74,999", "£75,000-£99,999", "£100,000 or more") ~ "£50k+",
    income == "Prefer not to say" ~ "Not disclosed",
    TRUE ~ NA_character_
  ))

df$income_collapsed <- factor(df$income_collapsed,
                              levels = c("<£25k", "£25-50k", "£50k+", "Not disclosed"),
                              ordered = TRUE)

participants <- df %>% filter(participation_status == "participant")
non_participants <- df %>% filter(participation_status == "non_participant")


# FIGURE 1: All Barriers by Gender (Comparison)

barriers_gender_long <- non_participants_gender %>%
  select(gender, all_of(barrier_cols)) %>%
  pivot_longer(cols = all_of(barrier_cols), names_to = "barrier", values_to = "endorsed") %>%
  group_by(gender, barrier) %>%
  summarise(percentage = mean(endorsed, na.rm = TRUE) * 100, .groups = "drop") %>%
  mutate(barrier_label = barrier_labels[barrier])

# Order by average
barrier_order <- barriers_gender_long %>%
  group_by(barrier_label) %>%
  summarise(avg = mean(percentage)) %>%
  arrange(avg) %>%
  pull(barrier_label)

barriers_gender_long$barrier_label <- factor(barriers_gender_long$barrier_label, 
                                             levels = barrier_order)

p4 <- ggplot(barriers_gender_long, aes(x = barrier_label, y = percentage, fill = gender)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(width = 0.8),
            hjust = -0.1, size = 3.2, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Man" = color_male, "Woman" = color_female),
                    name = "Gender") +
  scale_y_continuous(limits = c(0, 65), 
                     breaks = seq(0, 60, 20),
                     expand = c(0, 0)) +
  labs(
    subtitle = "Non-participants: Men (n=22) vs Women (n=36)",
    x = NULL,
    y = "Percentage Endorsing Barrier (%)"
  ) +
  theme_publication() +
  theme(legend.position = "bottom")

ggsave("figure1_all_barriers_gender.png", p4, width = 9, height = 5.5, dpi = 300)
ggsave("figure1_all_barriers_gender.pdf", p4, width = 9, height = 5.5)

# FIGURE 2: Geographic Barrier by Income

non_participants_income <- non_participants %>%
  filter(income_collapsed != "Not disclosed", !is.na(income_collapsed))

income_geo_data <- non_participants_income %>%
  group_by(income_collapsed) %>%
  summarise(
    n_endorsed = sum(bar_area, na.rm = TRUE),
    n_total = n(),
    percentage = mean(bar_area, na.rm = TRUE) * 100,
    .groups = "drop"
  )

p5 <- ggplot(income_geo_data, aes(x = income_collapsed, y = percentage)) +
  geom_col(fill = color_primary, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d/%d)", percentage, n_endorsed, n_total)),
            vjust = -0.3, size = 4.5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 50), 
                     breaks = seq(0, 50, 10),
                     expand = c(0, 0)) +
  labs(
    subtitle = "χ² = 5.69, p = 0.058",
    x = "Annual Household Income",
    y = "Geographic Barrier (%)"
  ) +
  theme_publication()

ggsave("figure2_geographic_barrier_income.png", p5, width = 7, height = 5, dpi = 300)
ggsave("figure2_geographic_barrier_income.pdf", p5, width = 7, height = 5)


# FIGURE 3: Cost Barrier by Income

income_cost_data <- non_participants_income %>%
  group_by(income_collapsed) %>%
  summarise(
    n_endorsed = sum(bar_cost, na.rm = TRUE),
    n_total = n(),
    percentage = mean(bar_cost, na.rm = TRUE) * 100,
    groups = "drop"
  )

p6 <- ggplot(income_cost_data, aes(x = income_collapsed, y = percentage)) +
  geom_col(fill = color_primary, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d/%d)", percentage, n_endorsed, n_total)),
            vjust = -0.3, size = 4.5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 70), 
                     breaks = seq(0, 70, 10),
                     expand = c(0, 0)) +
  labs(
    subtitle = "χ² = 1.60, p = 0.449",
    x = "Annual Household Income",
    y = "Cost Barrier (%)"
  ) +
  theme_publication()

ggsave("figure3_cost_barrier_income.png", p6, width = 7, height = 5, dpi = 300)
ggsave("figure3_cost_barrier_income.pdf", p6, width = 7, height = 5)


# FIGURE 4: Opposition to Industrial Agriculture by Age (SIGNIFICANT)

opposition_age <- participants %>%
  group_by(age_collapsed) %>%
  summarise(
    n_endorsed = sum(mot_opp_ind_agr, na.rm = TRUE),
    n_total = n(),
    percentage = mean(mot_opp_ind_agr, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(!is.na(age_collapsed))

p7 <- ggplot(opposition_age, aes(x = age_collapsed, y = percentage, group = 1)) +
  geom_line(color = color_primary, size = 1.5) +
  geom_point(color = color_primary, size = 4, shape = 21, fill = "white", stroke = 2) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            vjust = -1.5, size = 4, fontface = "bold") +
  scale_y_continuous(limits = c(0, 110), 
                     breaks = seq(0, 100, 20),
                     expand = c(0, 0)) +
  labs(
    subtitle = "χ² = 10.84, p = 0.028*",
    x = "Age Group",
    y = "Opposition to Industrial Agriculture (%)"
  ) +
  theme_publication()

ggsave("figure4_opposition_age.png", p7, width = 8, height = 5, dpi = 300)
ggsave("figure4_opposition_age.pdf", p7, width = 8, height = 5)

# FIGURE 5: Organic Production by Age

organic_age <- participants %>%
  group_by(age_collapsed) %>%
  summarise(
    n_endorsed = sum(mot_organic, na.rm = TRUE),
    n_total = n(),
    percentage = mean(mot_organic, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(!is.na(age_collapsed))

p8 <- ggplot(organic_age, aes(x = age_collapsed, y = percentage, group = 1)) +
  geom_line(color = color_secondary, size = 1.5) +
  geom_point(color = color_secondary, size = 4, shape = 22, fill = "white", stroke = 2) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            vjust = -1.5, size = 4, fontface = "bold") +
  scale_y_continuous(limits = c(0, 90), 
                     breaks = seq(0, 90, 20),
                     expand = c(0, 0)) +
  labs(
    subtitle = "χ² = 10.35, p = 0.035*",
    x = "Age Group",
    y = "Organic Production (%)"
  ) +
  theme_publication()

ggsave("figure5_organic_age.png", p8, width = 8, height = 5, dpi = 300)
ggsave("figure5_organic_age.pdf", p8, width = 8, height = 5)


# FIGURE 6: Participation Rates by Location

participation_location <- df %>%
  group_by(location) %>%
  summarise(
    participants = sum(participation_status == "participant"),
    total = n(),
    participation_rate = mean(participation_status == "participant") * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(participation_rate))

p10 <- ggplot(participation_location, 
              aes(x = reorder(location, participation_rate), y = participation_rate)) +
  geom_col(fill = color_highlight, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%\n(%d/%d)", 
                                participation_rate, participants, total)),
            vjust = -0.3, size = 4, fontface = "bold") +
  scale_y_continuous(limits = c(0, 80), 
                     breaks = seq(0, 80, 20),
                     expand = c(0, 0)) +
  labs(
    subtitle = "χ² = 4.38, p = 0.223",
    x = "Location Type",
    y = "Participation Rate (%)"
  ) +
  theme_publication()

ggsave("figure6_participation_location.png", p10, width = 7, height = 5, dpi = 300)
ggsave("figure6_participation_location.pdf", p10, width = 7, height = 5)
