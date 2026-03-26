library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(psych)
library(car)
library(effectsize)
library(purrr)

df_raw <- read_excel(file.choose())

df <- df_raw[!is.na(df_raw$participation_status), ]

df$participation_status <- factor(
  df$participation_status,
  levels = c("participant", "non_participant"),
  labels = c("Participant", "Non-Participant")
)

pfa_items <- c("pfa_diff_environment", "pfa_control",
               "pfa_can_influence", "pfa_cannot_change")
ce_items  <- c("ce_communities", "ce_consumer_demand",
               "ce_afs_solutions", "ce_necessary")
fca_items <- c("fca_knowledgeable", "fca_contribute",
               "fca_influence_others", "fca_afford",
               "fca_enough_food", "fca_struc_barriers")
all_items <- c(pfa_items, ce_items, fca_items)

df$pfa_cannot_change_r <- 8 - df$pfa_cannot_change

pfa_items_r <- c("pfa_diff_environment", "pfa_control",
                 "pfa_can_influence", "pfa_cannot_change_r")

df$pfa_score   <- rowMeans(df[, pfa_items_r], na.rm = TRUE)
df$ce_score    <- rowMeans(df[, ce_items],    na.rm = TRUE)
df$fca_score   <- rowMeans(df[, fca_items],   na.rm = TRUE)
df$part_binary <- as.integer(df$participation_status == "Participant")

afp <- df[df$participation_status == "Participant", ]
non <- df[df$participation_status == "Non-Participant", ]

cat("PFA alpha:", round(psych::alpha(df[, pfa_items_r])$total$raw_alpha, 3), "\n")
cat("CE  alpha:", round(psych::alpha(df[, ce_items])$total$raw_alpha, 3), "\n")
cat("FCA alpha:", round(psych::alpha(df[, fca_items])$total$raw_alpha, 3), "\n")

for (score in c("pfa_score", "ce_score", "fca_score")) {
  afp_vals <- afp[[score]]; non_vals <- non[[score]]
  cat(score, ": Participant M =", round(mean(afp_vals, na.rm = TRUE), 2),
      "SD =", round(sd(afp_vals, na.rm = TRUE), 2),
      "| Non-Participant M =", round(mean(non_vals, na.rm = TRUE), 2),
      "SD =", round(sd(non_vals, na.rm = TRUE), 2), "\n")
}

for (score in c("pfa_score", "ce_score", "fca_score")) {
  sw_afp <- shapiro.test(afp[[score]])
  sw_non <- shapiro.test(non[[score]])
  cat(score, ": Par W =", round(sw_afp$statistic, 4), "p =", round(sw_afp$p.value, 4),
      "| Non W =", round(sw_non$statistic, 4), "p =", round(sw_non$p.value, 4), "\n")
}

for (score in c("pfa_score", "ce_score", "fca_score")) {
  lev <- leveneTest(df[[score]] ~ df$participation_status)
  cat(score, ": F =", round(lev$`F value`[1], 3),
      "p =", round(lev$`Pr(>F)`[1], 4), "\n")
}

for (score in c("pfa_score", "ce_score", "fca_score")) {
  afp_vals <- afp[[score]]; non_vals <- non[[score]]
  t_res <- t.test(afp_vals, non_vals, var.equal = FALSE)
  mw    <- wilcox.test(afp_vals, non_vals, exact = FALSE)
  n1 <- sum(!is.na(afp_vals)); n2 <- sum(!is.na(non_vals))
  pool_sd <- sqrt(((n1-1)*sd(afp_vals,na.rm=TRUE)^2 +
                     (n2-1)*sd(non_vals,na.rm=TRUE)^2) / (n1+n2-2))
  d    <- abs((mean(afp_vals,na.rm=TRUE) - mean(non_vals,na.rm=TRUE)) / pool_sd)
  r_rb <- 1 - (2*mw$statistic) / (n1*n2)
  cat(score, ": t(", round(t_res$parameter,1), ")=", round(t_res$statistic,3),
      "p=", round(t_res$p.value,4), "d=", round(d,3),
      "| U=", mw$statistic, "p=", round(mw$p.value,4), "r=", round(r_rb,3), "\n")
}

item_pvals <- item_afp_m <- item_non_m <- item_t <- item_d <- numeric(length(all_items))

for (i in seq_along(all_items)) {
  item     <- all_items[i]
  afp_vals <- afp[[item]]; non_vals <- non[[item]]
  t_res    <- t.test(afp_vals, non_vals, var.equal = FALSE)
  n1 <- sum(!is.na(afp_vals)); n2 <- sum(!is.na(non_vals))
  pool_sd  <- sqrt(((n1-1)*sd(afp_vals,na.rm=TRUE)^2 +
                      (n2-1)*sd(non_vals,na.rm=TRUE)^2) / (n1+n2-2))
  item_pvals[i] <- t_res$p.value
  item_afp_m[i] <- mean(afp_vals, na.rm = TRUE)
  item_non_m[i] <- mean(non_vals, na.rm = TRUE)
  item_t[i]     <- t_res$statistic
  item_d[i]     <- abs((item_afp_m[i] - item_non_m[i]) / pool_sd)
}

bh_sig <- p.adjust(item_pvals, method = "BH") < 0.05

item_results <- data.frame(
  item   = all_items,
  AFP_M  = round(item_afp_m, 2),
  Non_M  = round(item_non_m, 2),
  t      = round(item_t, 3),
  p      = round(item_pvals, 4),
  d      = round(item_d, 3),
  BH_sig = ifelse(bh_sig, "YES", "no"),
  sig    = ifelse(item_pvals < 0.001, "***",
           ifelse(item_pvals < 0.01,  "**",
           ifelse(item_pvals < 0.05,  "*", "")))
)
print(item_results, row.names = FALSE)

df_lr       <- df[!is.na(df$pfa_score) & !is.na(df$ce_score) & !is.na(df$fca_score), ]
logit_model <- glm(part_binary ~ pfa_score + ce_score + fca_score,
                   data = df_lr, family = binomial(link = "logit"))
print(summary(logit_model))
cat("Odds Ratios:\n")
print(round(exp(coef(logit_model)), 3))

C_PAR <- "#2E7D5B"
C_NON <- "#C0392B"

box_long <- data.frame(
  group     = rep(df$participation_status, 3),
  construct = c(rep("PFA", nrow(df)), rep("CE", nrow(df)), rep("FCA", nrow(df))),
  score     = c(df$pfa_score, df$ce_score, df$fca_score)
)
box_long$construct <- factor(box_long$construct, levels = c("PFA","CE","FCA"))

box_plot <- ggplot(box_long, aes(x = group, y = score, fill = group)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.size = 1.5, width = 0.5) +
  geom_jitter(width = 0.12, alpha = 0.3, size = 1.2) +
  facet_wrap(~construct) +
  scale_fill_manual(values = c("Participant" = C_PAR, "Non-Participant" = C_NON), name = "") +
  scale_y_continuous(limits = c(1,7), breaks = 1:7) +
  labs(title    = "Composite Agency Scores by Participation Status",
       subtitle = "Individual data points overlaid. * p<.05  ** p<.01  *** p<.001",
       x = NULL, y = "Mean Score (1-7)") +
  theme_classic(base_size = 12) +
  theme(plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(size = 10, colour = "grey50"),
        strip.text      = element_text(face = "bold", size = 11),
        legend.position = "bottom",
        axis.text.x     = element_text(size = 9))

print(box_plot)
ggsave("plot1_composite_boxplots.png", box_plot, width = 10, height = 5, dpi = 180)

make_item_plot <- function(items, item_labels, title) {
  means_list <- list()
  for (item in items) {
    afp_vals <- afp[[item]]; non_vals <- non[[item]]
    means_list[[length(means_list)+1]] <- data.frame(
      item  = item,
      group = c("Participant","Non-Participant"),
      mean  = c(mean(afp_vals,na.rm=TRUE), mean(non_vals,na.rm=TRUE)),
      se    = c(sd(afp_vals,na.rm=TRUE)/sqrt(sum(!is.na(afp_vals))),
                sd(non_vals,na.rm=TRUE)/sqrt(sum(!is.na(non_vals))))
    )
  }
  means_df       <- do.call(rbind, means_list)
  means_df$item  <- factor(means_df$item,  levels = items, labels = item_labels)
  means_df$group <- factor(means_df$group, levels = c("Participant","Non-Participant"))
  sig_df         <- item_results[item_results$item %in% items, ]
  sig_df$item    <- factor(sig_df$item, levels = items, labels = item_labels)
  sig_df$y_pos   <- pmax(sig_df$AFP_M, sig_df$Non_M) + 0.35

  ggplot(means_df, aes(x = item, y = mean, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(0.7),
             width = 0.62, alpha = 0.88) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  position = position_dodge(0.7), width = 0.2, linewidth = 0.5) +
    geom_text(data = sig_df, aes(x = item, y = y_pos, label = sig),
              inherit.aes = FALSE, size = 5, fontface = "bold", colour = "#222222") +
    geom_hline(yintercept = 4, linetype = "dashed", colour = "grey60", linewidth = 0.5) +
    scale_fill_manual(values = c("Participant" = C_PAR, "Non-Participant" = C_NON), name = "") +
    scale_y_continuous(limits = c(0, 8.2), breaks = 1:7) +
    labs(title = title, x = NULL, y = "Mean Score (1-7)") +
    theme_classic(base_size = 11) +
    theme(plot.title    = element_text(face = "bold", size = 9),
          axis.text.x   = element_text(angle = 30, hjust = 1, size = 9),
          legend.position = "bottom")
}

p_pfa <- make_item_plot(pfa_items,
  c("Env. Difference","Personal Control","Can Influence","Cannot Change"),
  "Personal Food Agency (PFA)")

p_ce  <- make_item_plot(ce_items,
  c("Communities","Consumer Demand","AFS Solutions","Collective Action"),
  "Collective Efficacy (CE)")

p_fca <- make_item_plot(fca_items,
  c("Knowledgeable","Can Contribute","Influence Others","Can Afford","Enough Access","Struct. Barriers"),
  "Food Citizenship Agency (FCA)")

combined_plot <- (p_pfa | p_ce | p_fca) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Item-Level Agency Scores by AFP Participation Status",
    subtitle = "Error bars = +/- 1 SE  |  * p<.05  ** p<.01  *** p<.001 (BH-corrected)",
    theme    = theme(plot.title    = element_text(face = "bold", size = 13),
                     plot.subtitle = element_text(size = 10, colour = "grey40"))
  ) & theme(legend.position = "bottom")

print(combined_plot)
ggsave("plot2_item_bars.png", combined_plot, width = 16, height = 6, dpi = 180)

effect_df <- data.frame(
  item      = all_items,
  d         = item_results$d,
  construct = c(rep("PFA", length(pfa_items)),
                rep("CE",  length(ce_items)),
                rep("FCA", length(fca_items))),
  bh_sig    = bh_sig,
  label     = c("Makes env. difference","Personal control",
                "Can influence (PFA)","Cannot change (r)",
                "Communities can act","Consumer demand",
                "AFS are solution","Collective action nec.",
                "Feel knowledgeable","Can contribute",
                "Influence others","Can afford AFS",
                "Enough food access","Aware struct. barriers")
)
effect_df$construct <- factor(effect_df$construct, levels = c("PFA","CE","FCA"))

effect_plot <- ggplot(effect_df, aes(x = d, y = reorder(label, d), colour = construct)) +
  geom_segment(aes(x = 0, xend = d, yend = reorder(label, d)),
               linewidth = 1, alpha = 0.6) +
  geom_point(aes(size = bh_sig, alpha = bh_sig)) +
  scale_size_manual(values  = c("TRUE" = 4, "FALSE" = 2),  guide = "none") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.4), guide = "none") +
  scale_colour_manual(values = c("PFA" = "#4A90D9", "CE" = "#E67E22", "FCA" = "#2E7D5B"),
                      name = "Construct") +
  geom_vline(xintercept = c(0.2, 0.5, 0.8), linetype = "dashed",
             colour = "grey70", linewidth = 0.5) +
  annotate("text", x = 0.2, y = 0.4, label = "small",  size = 3, colour = "grey60") +
  annotate("text", x = 0.5, y = 0.4, label = "medium", size = 3, colour = "grey60") +
  annotate("text", x = 0.8, y = 0.4, label = "large",  size = 3, colour = "grey60") +
  labs(title    = "Effect Sizes for All Item-Level Group Differences",
       subtitle = "Large points = BH-significant | Faded = not significant",
       x = "Cohen's d", y = NULL) +
  theme_classic(base_size = 11) +
  theme(plot.title      = element_text(face = "bold"),
        plot.subtitle   = element_text(colour = "grey50", size = 9),
        legend.position = "right")

print(effect_plot)
ggsave("plot3_effect_sizes.png", effect_plot, width = 9, height = 6, dpi = 180)
