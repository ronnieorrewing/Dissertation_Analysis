library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)
library(patchwork)

df <- read_excel(file.choose())

color_primary   <- "#2C5F7C"
color_secondary <- "#7FA99B"
color_highlight <- "#E67E22"
color_env       <- "#2E7D32"
color_health    <- "#D32F2F"
color_animal    <- "#F57C00"

theme_pub <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_blank(),
      plot.subtitle    = element_text(size = 11, color = "gray40",
                                      hjust = 0.5, margin = margin(b = 15)),
      axis.title       = element_text(face = "bold", size = 12),
      axis.text        = element_text(size = 11),
      legend.title     = element_text(face = "bold", size = 11),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.margin      = margin(10, 10, 10, 10)
    )
}

df$is_participant <- ifelse(
  is.na(df$participation_status) |
    grepl("None currently", df$participation_status, ignore.case = TRUE),
  "Non-participant", "Participant"
)

df$has_dietary_restriction <- ifelse(
  is.na(df$diet) | grepl("None", df$diet, ignore.case = TRUE),
  "No restriction", "Has restriction"
)

df$diet_reason_health <- ifelse(
  grepl("Health", df$dietary_choice, ignore.case = TRUE),
  "Health reason", "No health reason"
)

df$diet_reason_env <- ifelse(
  grepl("Environmental", df$dietary_choice, ignore.case = TRUE),
  "Environmental reason", "No environmental reason"
)

df$diet_reason_animal <- ifelse(
  grepl("Animal welfare", df$dietary_choice, ignore.case = TRUE),
  "Animal welfare", "No animal welfare"
)

df$healthy_interest_group <- case_when(
  df$interest_in_healtheating %in% c("Not interested", "Slightly interested") ~ "Low",
  df$interest_in_healtheating == "Moderately interested" ~ "Medium",
  df$interest_in_healtheating %in% c("Very Interested", "Extremely interested") ~ "High",
  TRUE ~ NA_character_
)
df$healthy_interest_group <- factor(df$healthy_interest_group,
                                    levels = c("Low", "Medium", "High"),
                                    ordered = TRUE)

rest_data <- df %>%
  filter(!is.na(has_dietary_restriction)) %>%
  group_by(has_dietary_restriction, is_participant) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(has_dietary_restriction) %>%
  mutate(total = sum(n), percentage = n / total * 100) %>%
  filter(is_participant == "Participant")

p1 <- ggplot(rest_data, aes(x = has_dietary_restriction, y = percentage)) +
  geom_col(fill = color_primary, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 80, 20), expand = c(0, 0)) +
  labs(subtitle = "Chi-square = 3.38, p = 0.066",
       x = "Dietary Status", y = "Participation Rate (%)") +
  theme_pub()

motivations_data <- rbind(
  df %>%
    group_by(diet_reason_env, is_participant) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(diet_reason_env) %>%
    mutate(total = sum(n), percentage = n / total * 100,
           motivation_type = "Environmental") %>%
    filter(is_participant == "Participant", diet_reason_env == "Environmental reason"),
  df %>%
    group_by(diet_reason_health, is_participant) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(diet_reason_health) %>%
    mutate(total = sum(n), percentage = n / total * 100,
           motivation_type = "Health") %>%
    filter(is_participant == "Participant", diet_reason_health == "Health reason"),
  df %>%
    group_by(diet_reason_animal, is_participant) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(diet_reason_animal) %>%
    mutate(total = sum(n), percentage = n / total * 100,
           motivation_type = "Animal welfare") %>%
    filter(is_participant == "Participant", diet_reason_animal == "Animal welfare")
)

motivations_data$motivation_type <- factor(motivations_data$motivation_type,
                                           levels = c("Environmental", "Health", "Animal welfare"))
motivations_data$p_value <- c("p = 0.020*", "p = 0.453", "p = 0.178")[
  as.numeric(motivations_data$motivation_type)]

p2 <- ggplot(motivations_data, aes(x = motivation_type, y = percentage,
                                   fill = motivation_type)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            vjust = -1.2, size = 5, fontface = "bold") +
  geom_text(aes(label = p_value), vjust = -2.5, size = 3.5, fontface = "italic") +
  scale_fill_manual(values = c("Environmental" = color_env,
                               "Health"        = color_health,
                               "Animal welfare"= color_animal)) +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 80, 20), expand = c(0, 0)) +
  labs(subtitle = "Participation rates among those citing each dietary motivation",
       x = "Dietary Choice Motivation", y = "Participation Rate (%)") +
  theme_pub()

hc_vars <- c("hc_think_issues", "hc_conscious_nutrition", "hc_avoid_additives ",
             "hc_seek_info", "hc_prefer_organic")

hc_labels <- c(
  "hc_think_issues"       = "Think about\nhealth issues",
  "hc_conscious_nutrition"= "Conscious of\nnutrition",
  "hc_avoid_additives "   = "Avoid food\nadditives",
  "hc_seek_info"          = "Seek nutrition\ninformation",
  "hc_prefer_organic"     = "Prefer\norganic food"
)

hc_data <- data.frame()
for (var in hc_vars) {
  part_mean <- mean(df[df$is_participant == "Participant", var][[1]], na.rm = TRUE)
  non_mean  <- mean(df[df$is_participant == "Non-participant", var][[1]], na.rm = TRUE)
  hc_data   <- rbind(hc_data, data.frame(
    variable    = hc_labels[var],
    Participant = part_mean,
    `Non-participant` = non_mean,
    difference  = part_mean - non_mean,
    check.names = FALSE
  ))
}

hc_long <- hc_data %>%
  pivot_longer(cols = c("Participant", "Non-participant"),
               names_to = "group", values_to = "mean_score")

hc_long$sig <- ""
hc_long$sig[hc_long$variable == "Think about\nhealth issues"]   <- "*"
hc_long$sig[hc_long$variable == "Avoid food\nadditives"]        <- "*"
hc_long$sig[hc_long$variable == "Seek nutrition\ninformation"]  <- "**"
hc_long$sig[hc_long$variable == "Prefer\norganic food"]         <- "*"

p3 <- ggplot(hc_long, aes(x = variable, y = mean_score, fill = group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(data = hc_long %>% filter(group == "Participant"),
            aes(label = sig), position = position_dodge(width = 0.8),
            vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_manual(values = c("Participant"     = color_primary,
                               "Non-participant" = color_secondary),
                    name = "Group") +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1), expand = c(0, 0)) +
  labs(subtitle = "Mean scores on 1-5 scale. * p<0.05, ** p<0.01",
       x = "Health Consciousness Dimension", y = "Mean Score") +
  theme_pub() +
  theme(axis.text.x = element_text(size = 10))

df$info_seeking_group <- cut(df$hc_seek_info,
                             breaks = c(0, 2.5, 3.5, 5),
                             labels = c("Low (1-2)", "Medium (3)", "High (4-5)"),
                             include.lowest = TRUE)

info_data <- df %>%
  filter(!is.na(info_seeking_group)) %>%
  group_by(info_seeking_group, is_participant) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(info_seeking_group) %>%
  mutate(total = sum(n), percentage = n / total * 100) %>%
  filter(is_participant == "Participant")

p4 <- ggplot(info_data, aes(x = info_seeking_group, y = percentage)) +
  geom_col(fill = color_highlight, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d/%d)", percentage, n, total)),
            vjust = -0.3, size = 4, fontface = "bold") +
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 80, 20), expand = c(0, 0)) +
  labs(subtitle = "Mann-Whitney U = 2379.00, p = 0.002**",
       x = "Information-Seeking Behaviour", y = "Participation Rate (%)") +
  theme_pub()

p5_combined <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    caption = "Note: * p<0.05, ** p<0.01. HC = Health Consciousness",
    theme   = theme(plot.caption = element_text(size = 9, hjust = 0.5))
  )
print(p5_combined)
ggsave("fig_diet_combined_panel.png", p5_combined, width = 16, height = 12, dpi = 300)

hc_data_ordered <- hc_data %>%
  arrange(desc(difference)) %>%
  mutate(variable  = factor(variable, levels = variable),
         significant = c("**", "*", "*", "", "*"))

p6 <- ggplot(hc_data_ordered, aes(x = difference, y = variable)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = 0, xend = difference, y = variable, yend = variable),
               color = color_primary, linewidth = 1.5) +
  geom_point(size = 5, color = color_primary) +
  geom_text(aes(label = significant), hjust = -0.5, size = 6, fontface = "bold") +
  geom_text(aes(label = sprintf("%.2f", difference)), hjust = -1.5, size = 4, fontface = "bold") +
  scale_x_continuous(limits = c(0, 0.65), breaks = seq(0, 0.6, 0.2)) +
  labs(subtitle = "Difference = Participant mean - Non-participant mean. * p<0.05, ** p<0.01",
       x = "Difference in Mean Score\n(Participants - Non-participants)",
       y = "Health Consciousness Dimension") +
  theme_pub() +
  theme(panel.grid.major.y = element_line(color = "gray90"))

print(p6)
ggsave("fig_diet_hc_differences.png", p6, width = 9, height = 6, dpi = 300)

env_detail <- df %>%
  group_by(diet_reason_env, is_participant) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(diet_reason_env) %>%
  mutate(total = sum(n), percentage = n / total * 100)

p7 <- ggplot(env_detail, aes(x = diet_reason_env, y = percentage, fill = is_participant)) +
  geom_col(position = "fill", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", percentage, n)),
            position = position_fill(vjust = 0.5),
            size = 4, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("Participant" = color_primary, "Non-participant" = color_secondary),
                    name = "Status") +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  labs(subtitle = "Chi-square = 5.40, p = 0.020*",
       x = "Dietary Choice Reason", y = "Proportion") +
  theme_pub()

print(p7)
ggsave("fig_diet_environmental_detail.png", p7, width = 8, height = 5, dpi = 300)

interest_data <- df %>%
  filter(!is.na(healthy_interest_group)) %>%
  group_by(healthy_interest_group, is_participant) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(healthy_interest_group) %>%
  mutate(total = sum(n), percentage = n / total * 100) %>%
  filter(is_participant == "Participant")

p8 <- ggplot(interest_data, aes(x = healthy_interest_group, y = percentage)) +
  geom_col(fill = color_secondary, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 80, 20), expand = c(0, 0)) +
  labs(subtitle = "Chi-square = 1.12, p = 0.571 (not significant)",
       x = "Interest in Healthy Eating", y = "Participation Rate (%)") +
  theme_pub()

print(p8)
ggsave("fig_diet_healthy_interest.png", p8, width = 7, height = 5, dpi = 300)
