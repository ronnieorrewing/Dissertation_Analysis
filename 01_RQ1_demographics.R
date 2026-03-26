library(readxl)
library(dplyr)
library(tidyr)

df <- read_excel(file.choose())

demographics <- df %>%
  select(participation_status, age, gender, income, education, location)

demo_table <- demographics %>%
  pivot_longer(
    cols = -participation_status,
    names_to = "variable",
    values_to = "category"
  ) %>%
  filter(!is.na(category)) %>%
  group_by(variable, category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  ungroup()

print(demo_table, n = 25)
