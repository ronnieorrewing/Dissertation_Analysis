library(readxl)
library(ggplot2)

df <- read_excel(file.choose())

age_plot_data <- df[!is.na(df$age), ]
age_plot_data$age <- factor(age_plot_data$age,
                            levels = c("18-24", "25-34", "35-44", "45+"))

ggplot(age_plot_data, aes(x = age, y = fsk_food_origin)) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  labs(x = "Age group", y = "Importance of food origin") +
  theme_classic(base_size = 12)

ggplot(age_plot_data, aes(x = age, y = fsc_corp_over_com)) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  labs(x = "Age group",
       y = "Perceived corporate prioritisation of profit over community") +
  theme_classic(base_size = 12)

income_plot_data <- df[df$income != "Prefer not to say" & !is.na(df$income), ]
income_plot_data$income <- factor(
  income_plot_data$income,
  levels = c("Low (<£24,999)", "Medium (£25-49k)", "High (£50,000+)")
)

ggplot(income_plot_data, aes(x = income, y = fsc_corp_over_com)) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  labs(x = "Income bracket",
       y = "Perceived corporate prioritisation of profit over community") +
  theme_classic(base_size = 12)

ggplot(df[!is.na(df$employment), ],
       aes(x = employment, y = fsk_farm_to_plate)) +
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  labs(x = "Employment status", y = "Importance of farm-to-plate food") +
  theme_classic(base_size = 12)

ggplot(age_plot_data, aes(x = age, y = fsk_food_origin)) +
  stat_summary(fun = median, geom = "point", size = 3) +
  stat_summary(
    fun.data = function(x) {
      m  <- median(x)
      ci <- quantile(x, probs = c(0.25, 0.75))
      data.frame(y = m, ymin = ci[1], ymax = ci[2])
    },
    geom = "errorbar", width = 0.2
  ) +
  labs(x = "Age group", y = "Median importance of food origin") +
  theme_classic(base_size = 12)
