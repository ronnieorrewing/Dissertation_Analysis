library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

df <- read_excel(file.choose())

motivation_summary <- df %>%
  filter(participation_status == "participant") %>%
  summarise(across(starts_with("mot_"), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "motivation", values_to = "proportion") %>%
  mutate(percent = proportion * 100)

ggplot(motivation_summary, aes(x = percent, y = reorder(motivation, percent))) +
  geom_col(fill = "#4C9F70") +
  labs(x = "Percentage of participants (%)", y = NULL,
       title = "Motivations for engaging in alternative food systems") +
  theme_minimal()

barrier_summary <- df %>%
  filter(participation_status == "non_participant") %>%
  summarise(across(starts_with("bar_"), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "barrier", values_to = "proportion") %>%
  mutate(percent = proportion * 100)

ggplot(barrier_summary, aes(x = percent, y = reorder(barrier, percent))) +
  geom_col(fill = "#D95F02") +
  labs(x = "Percentage of non-participants (%)", y = NULL,
       title = "Barriers to participation in alternative food systems") +
  theme_minimal()

challenge_summary <- df %>%
  filter(participation_status == "participant") %>%
  summarise(across(starts_with("chall_"), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "challenge", values_to = "proportion") %>%
  mutate(percent = proportion * 100)

ggplot(challenge_summary, aes(x = percent, y = reorder(challenge, percent))) +
  geom_col(fill = "#7570B3") +
  labs(x = "Percentage of participants (%)", y = NULL,
       title = "Challenges experienced by participants") +
  theme_minimal()

df$income <- factor(df$income, levels = c(
  "Under £15,000", "£15,000-£24,999", "£25,000-£34,999",
  "£35,000-£49,999", "£50,000-£74,999", "£75,000-£99,999",
  "£100,000 or more", "Prefer not to say"
), ordered = TRUE)

df$location <- factor(df$location,
                      levels = c("Rural", "Town", "City", "Large city"),
                      ordered = TRUE)

df %>%
  filter(participation_status == "non_participant") %>%
  group_by(income) %>%
  summarise(percent_cost = mean(bar_cost, na.rm = TRUE) * 100) %>%
  ggplot(aes(x = income, y = percent_cost)) +
  geom_col(fill = "#D95F02") +
  labs(x = "Income band",
       y = "Percentage reporting cost as a barrier",
       title = "Cost as a barrier by income group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df %>%
  filter(participation_status == "participant") %>%
  group_by(age) %>%
  summarise(percent_env = mean(mot_envconcern, na.rm = TRUE) * 100) %>%
  ggplot(aes(x = age, y = percent_env)) +
  geom_col(fill = "#4C9F70") +
  labs(x = "Age group",
       y = "Percentage motivated by environmental concerns",
       title = "Environmental motivations by age group") +
  theme_minimal()

df %>%
  filter(participation_status == "participant") %>%
  group_by(location) %>%
  summarise(percent_area = mean(mot_comcon, na.rm = TRUE) * 100) %>%
  ggplot(aes(x = location, y = percent_area)) +
  geom_col(fill = "#4C9F70") +
  labs(x = "Location",
       y = "Motivation for community connection",
       title = "Community connection motivations by location") +
  theme_minimal()
