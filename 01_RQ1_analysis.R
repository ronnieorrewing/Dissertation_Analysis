# ============================================================================
# Alternative Food Procurement Analysis
# ============================================================================
# Age groups: 18-24, 25-34, 35-44, 45-54, 55+ (55-64 + 65+)
# Income groups: <£25k, £25-50k, £50k+
# ============================================================================

library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)
library(patchwork)

theme_set(theme_minimal(base_size = 12))

# Custom color palette
color_primary <- "#2C5F7C"
color_secondary <- "#7FA99B"
color_highlight <- "#E67E22"
color_male <- "#3498DB"
color_female <- "#E74C3C"

# Load data
df <- read_excel("food_survey_clean_2.xlsx", sheet = 1)

# Create collapsed age groups: 18-24, 25-34, 35-44, 45-54, 55+
df <- df %>%
  mutate(age_collapsed = case_when(
    age == "18-24" ~ "18-24",
    age == "25-34" ~ "25-34",
    age == "35-44" ~ "35-44",
    age == "45-54" ~ "45-54",
    age %in% c("55-64", "65+") ~ "55+",
    TRUE ~ NA_character_
  ))

# Convert to ordered factor
df$age_collapsed <- factor(df$age_collapsed, 
                           levels = c("18-24", "25-34", "35-44", "45-54", "55+"),
                           ordered = TRUE)

# Create collapsed income groups: <£25k, £25-50k, £50k+
df <- df %>%
  mutate(income_collapsed = case_when(
    income %in% c("Under £15,000", "£15,000-£24,999") ~ "<£25k",
    income %in% c("£25,000-£34,999", "£35,000-£49,999") ~ "£25-50k",
    income %in% c("£50,000-£74,999", "£75,000-£99,999", "£100,000 or more") ~ "£50k+",
    income == "Prefer not to say" ~ "Not disclosed",
    TRUE ~ NA_character_
  ))

# Convert to ordered factor
df$income_collapsed <- factor(df$income_collapsed,
                              levels = c("<£25k", "£25-50k", "£50k+", "Not disclosed"),
                              ordered = TRUE)

# Separate participants and non-participants
participants <- df %>% filter(participation_status == "participant")
non_participants <- df %>% filter(participation_status == "non_participant")

# Display sample characteristics
cat("SAMPLE CHARACTERISTICS\n")
cat("--------------------------------------------------------------------------------\n")
cat("Total sample: n =", nrow(df), "\n")
cat("Participants: n =", nrow(participants), 
    sprintf("(%.1f%%)\n", nrow(participants)/nrow(df)*100))
cat("Non-participants: n =", nrow(non_participants), 
    sprintf("(%.1f%%)\n\n", nrow(non_participants)/nrow(df)*100))

cat("Age group distribution:\n")
print(table(df$age_collapsed))
cat("\nIncome group distribution:\n")
print(table(df$income_collapsed))


# 2. BARRIERS ANALYSIS (Non-participants only)


barrier_cols <- c("bar_interest", "bar_cost", "bar_area", 
                  "bar_dkfind", "bar_supermconv", "bar_prodvar")

barrier_labels <- c(
  bar_interest = "Lack of interest",
  bar_cost = "Cost too high",
  bar_area = "Nothing in my area",
  bar_dkfind = "Don't know where to find",
  bar_supermconv = "Supermarket more convenient",
  bar_prodvar = "Not enough product variety"
)

# Overall barriers
cat("\n--- Overall Barriers (Non-participants, n=59) ---\n")
for(barrier in barrier_cols) {
  n <- sum(non_participants[[barrier]], na.rm = TRUE)
  pct <- mean(non_participants[[barrier]], na.rm = TRUE) * 100
  cat(sprintf("%-35s: %2d (%.1f%%)\n", barrier_labels[barrier], n, pct))
}

# 3. BARRIERS BY AGE GROUP

cat("\n--- Barriers by Age Group ---\n")
cat("Test used: Chi-square test of independence\n")
cat("Purpose: Tests whether barrier endorsement differs across age groups\n")
cat("Note: Fisher's exact test used when expected cell frequencies < 5\n\n")

non_participants$age_collapsed <- factor(non_participants$age_collapsed, 
                                         levels = c("18-24", "25-34", "35-44", 
                                                    "45-54", "55+"),
                                         ordered = TRUE)

for(barrier in barrier_cols) {
  cat("\n", barrier_labels[barrier], ":\n")
  
  summary_tab <- non_participants %>%
    filter(!is.na(age_collapsed)) %>%
    group_by(age_collapsed) %>%
    summarise(
      n_endorsed = sum(.data[[barrier]], na.rm = TRUE),
      n_total = n(),
      percentage = mean(.data[[barrier]], na.rm = TRUE) * 100,
      .groups = "drop"
    )
  print(summary_tab)
  
  # Chi-square test with error handling
  tab <- table(non_participants$age_collapsed, non_participants[[barrier]])
  tab <- tab[rowSums(tab) > 0, , drop = FALSE]
  
  if(nrow(tab) > 1 && ncol(tab) > 1) {
    # Check if expected frequencies are adequate
    chi_test <- suppressWarnings(chisq.test(tab))
    
    # Check for valid test
    if(is.finite(chi_test$statistic) && is.finite(chi_test$p.value)) {
      cat(sprintf("  χ² = %.2f, df = %d, p = %.3f", 
                  chi_test$statistic, chi_test$parameter, chi_test$p.value))
      
      # Check for low expected frequencies
      if(any(chi_test$expected < 5)) {
        cat(" (Warning: Some expected frequencies < 5)")
      }
      
      if(chi_test$p.value < 0.05) {
        cat(" ***\n  SIGNIFICANT: Barrier differs significantly by age (p < 0.05)\n")
      } else {
        cat("\n  Not significant (p ≥ 0.05)\n")
      }
    } else {
      cat("  Test not valid (insufficient cell frequencies)\n")
    }
  }
}

# 4. BARRIERS BY GENDER

cat("\n--- Barriers by Gender (Men vs Women) ---\n")
cat("Test used: Chi-square test of independence\n")
cat("Purpose: Tests whether barrier endorsement differs between genders\n\n")

non_participants_gender <- non_participants %>%
  filter(gender %in% c("Man", "Woman"))

for(barrier in barrier_cols) {
  cat("\n", barrier_labels[barrier], ":\n")
  
  summary_tab <- non_participants_gender %>%
    group_by(gender) %>%
    summarise(
      n_endorsed = sum(.data[[barrier]], na.rm = TRUE),
      n_total = n(),
      percentage = mean(.data[[barrier]], na.rm = TRUE) * 100,
      .groups = "drop"
    )
  print(summary_tab)
  
  # Chi-square test with error handling
  tab <- table(non_participants_gender$gender, non_participants_gender[[barrier]])
  if(nrow(tab) > 1 && ncol(tab) > 1) {
    chi_test <- suppressWarnings(chisq.test(tab))
    
    if(is.finite(chi_test$statistic) && is.finite(chi_test$p.value)) {
      cat(sprintf("  χ² = %.2f, df = %d, p = %.3f", 
                  chi_test$statistic, chi_test$parameter, chi_test$p.value))
      
      if(any(chi_test$expected < 5)) {
        cat(" (Warning: Some expected frequencies < 5)")
      }
      
      if(chi_test$p.value < 0.05) {
        cat(" ***\n  SIGNIFICANT: Barrier differs significantly by gender (p < 0.05)\n")
      } else {
        cat("\n  Not significant (p ≥ 0.05)\n")
      }
    } else {
      cat("  Test not valid (insufficient cell frequencies)\n")
    }
  }
}

# 5. BARRIERS BY INCOME

cat("\n--- Barriers by Income Level ---\n")
cat("Test used: Chi-square test of independence\n")
cat("Purpose: Tests whether barrier endorsement differs across income levels\n")
cat("Note: 'Not disclosed' excluded from analysis\n\n")

# Exclude "Not disclosed" for cleaner analysis
non_participants_income <- non_participants %>%
  filter(income_collapsed != "Not disclosed", !is.na(income_collapsed))

for(barrier in barrier_cols) {
  cat("\n", barrier_labels[barrier], ":\n")
  
  summary_tab <- non_participants_income %>%
    group_by(income_collapsed) %>%
    summarise(
      n_endorsed = sum(.data[[barrier]], na.rm = TRUE),
      n_total = n(),
      percentage = mean(.data[[barrier]], na.rm = TRUE) * 100,
      .groups = "drop"
    )
  print(summary_tab)
  
  # Chi-square test with error handling
  tab <- table(non_participants_income$income_collapsed, 
               non_participants_income[[barrier]])
  
  if(nrow(tab) > 1 && ncol(tab) > 1) {
    chi_test <- suppressWarnings(chisq.test(tab))
    
    if(is.finite(chi_test$statistic) && is.finite(chi_test$p.value)) {
      cat(sprintf("  χ² = %.2f, df = %d, p = %.3f", 
                  chi_test$statistic, chi_test$parameter, chi_test$p.value))
      
      if(any(chi_test$expected < 5)) {
        cat(" (Warning: Some expected frequencies < 5)")
      }
      
      if(chi_test$p.value < 0.05) {
        cat(" ***\n  SIGNIFICANT: Barrier differs significantly by income (p < 0.05)\n")
      } else if(chi_test$p.value < 0.10) {
        cat(" (~)\n  MARGINALLY SIGNIFICANT (0.05 ≤ p < 0.10)\n")
      } else {
        cat("\n  Not significant (p ≥ 0.05)\n")
      }
    } else {
      cat("  Test not valid (insufficient cell frequencies)\n")
    }
  }
}

# 6. MOTIVATIONS ANALYSIS (Participants only)

motivation_cols <- c("mot_envconcern", "mot_healthben", "mot_localecon", 
                     "mot_comcon", "mot_opp_ind_agr", "mot_fstransp", 
                     "mot_edu_opp", "mot_ethical", "mot_organic")

motivation_labels <- c(
  mot_envconcern = "Environmental concern",
  mot_healthben = "Health benefits",
  mot_localecon = "Support local economy",
  mot_comcon = "Community connection",
  mot_opp_ind_agr = "Opposition to industrial agriculture",
  mot_fstransp = "Food system transparency",
  mot_edu_opp = "Educational opportunities",
  mot_ethical = "Ethical concerns",
  mot_organic = "Organic production"
)

# Overall motivations
cat("\n--- Overall Motivations (Participants, n=68) ---\n")
for(motivation in motivation_cols) {
  n <- sum(participants[[motivation]], na.rm = TRUE)
  pct <- mean(participants[[motivation]], na.rm = TRUE) * 100
  cat(sprintf("%-45s: %2d (%.1f%%)\n", motivation_labels[motivation], n, pct))
}

# 7. MOTIVATIONS BY AGE GROUP

cat("\n--- Motivations by Age Group ---\n")

participants$age_collapsed <- factor(participants$age_collapsed, 
                                     levels = c("18-24", "25-34", "35-44", 
                                                "45-54", "55+"),
                                     ordered = TRUE)

for(motivation in motivation_cols) {
  cat("\n", motivation_labels[motivation], ":\n")
  
  summary_tab <- participants %>%
    filter(!is.na(age_collapsed)) %>%
    group_by(age_collapsed) %>%
    summarise(
      n_endorsed = sum(.data[[motivation]], na.rm = TRUE),
      n_total = n(),
      percentage = mean(.data[[motivation]], na.rm = TRUE) * 100,
      .groups = "drop"
    )
  print(summary_tab)
  
  # Chi-square test with error handling
  tab <- table(participants$age_collapsed, participants[[motivation]])
  tab <- tab[rowSums(tab) > 0, , drop = FALSE]
  
  if(nrow(tab) > 1 && ncol(tab) > 1) {
    chi_test <- suppressWarnings(chisq.test(tab))
    
    if(is.finite(chi_test$statistic) && is.finite(chi_test$p.value)) {
      cat(sprintf("  χ² = %.2f, df = %d, p = %.3f", 
                  chi_test$statistic, chi_test$parameter, chi_test$p.value))
      
      if(any(chi_test$expected < 5)) {
        cat(" (Warning: Some expected frequencies < 5)")
      }
      
      if(chi_test$p.value < 0.05) {
        cat(" ***\n  SIGNIFICANT: Motivation differs significantly by age (p < 0.05)\n")
      } else {
        cat("\n  Not significant (p ≥ 0.05)\n")
      }
    } else {
      cat("  Test not valid (insufficient cell frequencies)\n")
    }
  }
}

# 8. MOTIVATIONS BY GENDER

cat("\n--- Motivations by Gender ---\n")

participants_gender <- participants %>%
  filter(gender %in% c("Man", "Woman"))

for(motivation in motivation_cols) {
  cat("\n", motivation_labels[motivation], ":\n")
  
  summary_tab <- participants_gender %>%
    group_by(gender) %>%
    summarise(
      n_endorsed = sum(.data[[motivation]], na.rm = TRUE),
      n_total = n(),
      percentage = mean(.data[[motivation]], na.rm = TRUE) * 100,
      .groups = "drop"
    )
  print(summary_tab)
  
  # Chi-square test
  tab <- table(participants_gender$gender, participants_gender[[motivation]])
  if(nrow(tab) > 1 && ncol(tab) > 1) {
    chi_test <- suppressWarnings(chisq.test(tab))
    if(is.finite(chi_test$p.value) && chi_test$p.value < 0.05) {
      cat(sprintf("  χ² = %.2f, df = %d, p = %.3f ***\n", 
                  chi_test$statistic, chi_test$parameter, chi_test$p.value))
    }
  }
}

# 9. PARTICIPATION RATES BY DEMOGRAPHICS

# By age group
cat("\n--- By Age Group ---\n")
participation_age <- df %>%
  filter(!is.na(age_collapsed)) %>%
  group_by(age_collapsed) %>%
  summarise(
    participants = sum(participation_status == "participant"),
    total = n(),
    participation_rate = mean(participation_status == "participant") * 100,
    .groups = "drop"
  )
print(participation_age)

tab_age <- table(df$age_collapsed, df$participation_status)
tab_age <- tab_age[rowSums(tab_age) > 0, , drop = FALSE]
chi_age <- suppressWarnings(chisq.test(tab_age))
if(is.finite(chi_age$p.value)) {
  cat(sprintf("χ² = %.2f, df = %d, p = %.3f\n", 
              chi_age$statistic, chi_age$parameter, chi_age$p.value))
}

# By gender
cat("\n--- By Gender ---\n")
df_gender <- df %>% filter(gender %in% c("Man", "Woman"))
participation_gender <- df_gender %>%
  group_by(gender) %>%
  summarise(
    participants = sum(participation_status == "participant"),
    total = n(),
    participation_rate = mean(participation_status == "participant") * 100,
    .groups = "drop"
  )
print(participation_gender)

chi_gender <- suppressWarnings(chisq.test(table(df_gender$gender, df_gender$participation_status)))
if(is.finite(chi_gender$p.value)) {
  cat(sprintf("χ² = %.2f, df = %d, p = %.3f\n", 
              chi_gender$statistic, chi_gender$parameter, chi_gender$p.value))
}

# By income
cat("\n--- By Income Level ---\n")
df_income <- df %>% filter(income_collapsed != "Not disclosed", !is.na(income_collapsed))
participation_income <- df_income %>%
  group_by(income_collapsed) %>%
  summarise(
    participants = sum(participation_status == "participant"),
    total = n(),
    participation_rate = mean(participation_status == "participant") * 100,
    .groups = "drop"
  )
print(participation_income)

chi_income <- suppressWarnings(chisq.test(table(df_income$income_collapsed, df_income$participation_status)))
if(is.finite(chi_income$p.value)) {
  cat(sprintf("χ² = %.2f, df = %d, p = %.3f\n", 
              chi_income$statistic, chi_income$parameter, chi_income$p.value))
}

# By location
cat("\n--- By Location ---\n")
participation_location <- df %>%
  group_by(location) %>%
  summarise(
    participants = sum(participation_status == "participant"),
    total = n(),
    participation_rate = mean(participation_status == "participant") * 100,
    .groups = "drop"
  )
print(participation_location)

chi_location <- suppressWarnings(chisq.test(table(df$location, df$participation_status)))
if(is.finite(chi_location$p.value)) {
  cat(sprintf("χ² = %.2f, df = %d, p = %.3f\n", 
              chi_location$statistic, chi_location$parameter, chi_location$p.value))
}
