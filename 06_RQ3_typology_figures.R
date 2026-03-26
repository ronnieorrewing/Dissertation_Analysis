library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

df_raw <- read_excel(file.choose())

df <- df_raw[!is.na(df_raw$participation_status), ]

df$participation_status <- factor(
  df$participation_status,
  levels = c("participant", "non_participant"),
  labels = c("AFP Participant", "Non-Participant")
)

df$pfa_cannot_change_r <- 8 - df$pfa_cannot_change

pfa_items_r <- c("pfa_diff_environment", "pfa_control",
                 "pfa_can_influence", "pfa_cannot_change_r")
ce_items    <- c("ce_communities", "ce_consumer_demand",
                 "ce_afs_solutions", "ce_necessary")
fca_items   <- c("fca_knowledgeable", "fca_contribute",
                 "fca_influence_others", "fca_afford",
                 "fca_enough_food", "fca_struc_barriers")

df$pfa_score   <- rowMeans(df[, pfa_items_r], na.rm = TRUE)
df$ce_score    <- rowMeans(df[, ce_items],    na.rm = TRUE)
df$fca_score   <- rowMeans(df[, fca_items],   na.rm = TRUE)
df$agency_mean <- rowMeans(df[, c("pfa_score","ce_score","fca_score")], na.rm = TRUE)

df$typology <- dplyr::case_when(
  df$participation_status == "AFP Participant" &
    df$agency_mean >= 5.5 & df$fca_score >= 5.8 ~ "Type 1",
  df$participation_status == "AFP Participant"   ~ "Type 2",
  df$participation_status == "Non-Participant"   &
    df$agency_mean >= 4.5 & df$ce_score  >= 5.5 ~ "Type 3",
  TRUE ~ "Type 4"
)

df$typology <- factor(df$typology, levels = c("Type 1","Type 2","Type 3","Type 4"))

type_colours <- c(
  "Type 1" = "#2E7D5B",
  "Type 2" = "#2E5B8A",
  "Type 3" = "#E67E22",
  "Type 4" = "#C0392B"
)

type_labels <- c(
  "Type 1" = "Type 1: Committed Food Citizen (n=23)",
  "Type 2" = "Type 2: Constrained Engager (n=45)",
  "Type 3" = "Type 3: Sympathetic Bystander (n=37)",
  "Type 4" = "Type 4: Disengaged / Indifferent (n=22)"
)

type_labels_short <- c(
  "Type 1" = "Type 1:\nCommitted Food Citizen",
  "Type 2" = "Type 2:\nConstrained Engager",
  "Type 3" = "Type 3:\nSympathetic Bystander",
  "Type 4" = "Type 4:\nDisengaged / Indifferent"
)

subscale_long <- data.frame(
  typology = rep(df$typology, 3),
  subscale = c(rep("Personal Food Agency (PFA)", nrow(df)),
               rep("Collective Efficacy (CE)",   nrow(df)),
               rep("Food Citizenship Agency (FCA)", nrow(df))),
  score    = c(df$pfa_score, df$ce_score, df$fca_score)
)
subscale_long$subscale <- factor(subscale_long$subscale,
  levels = c("Personal Food Agency (PFA)",
             "Collective Efficacy (CE)",
             "Food Citizenship Agency (FCA)"))

figA1 <- ggplot(subscale_long, aes(x = score, y = typology, colour = typology)) +
  geom_jitter(height = 0.12, alpha = 0.55, size = 2.2, shape = 16) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.6, linewidth = 0.9, fatten = 0) +
  geom_vline(xintercept = 4, linetype = "dashed", colour = "grey60", linewidth = 0.6) +
  facet_wrap(~subscale, nrow = 1) +
  scale_colour_manual(values = type_colours, guide = "none") +
  scale_x_continuous(limits = c(1,7), breaks = 1:7) +
  labs(title    = "Figure A1. Subscale Score Distributions by Typology Profile",
       subtitle = "Points = individual participants | Bar = group mean | Dashed = scale midpoint (4.0)",
       x = "Mean Score (1\u20137)", y = NULL) +
  theme_classic(base_size = 11) +
  theme(plot.title       = element_text(face = "bold", size = 11),
        plot.subtitle    = element_text(size = 9, colour = "grey50"),
        strip.text       = element_text(face = "bold", size = 10),
        axis.text.y      = element_text(size = 9),
        panel.background = element_rect(fill = "#FAFAFA"),
        strip.background = element_rect(fill = "#FAFAFA", colour = "grey80"))

print(figA1)
ggsave("figA1_score_distributions.png", figA1, width = 13, height = 4.5, dpi = 180)

set.seed(42)
df$jitter_y <- runif(nrow(df), -0.06, 0.06) + 0.1

figA2 <- ggplot(df, aes(x = agency_mean, y = jitter_y, colour = typology)) +
  geom_point(alpha = 0.75, size = 2.8, shape = 16) +
  geom_vline(xintercept = 4.5, linetype = "dashed",
             colour = "#C0392B", linewidth = 1, alpha = 0.8) +
  geom_vline(xintercept = 5.5, linetype = "dashed",
             colour = "#2E7D5B", linewidth = 1, alpha = 0.8) +
  annotate("text", x = 4.5, y = 0.22, label = "4.5 threshold",
           size = 3, colour = "#C0392B", hjust = -0.05) +
  annotate("text", x = 5.5, y = 0.22, label = "5.5 threshold",
           size = 3, colour = "#2E7D5B", hjust = -0.05) +
  scale_colour_manual(values = type_colours, labels = type_labels, name = NULL) +
  scale_x_continuous(limits = c(1,7), breaks = 1:7) +
  scale_y_continuous(limits = c(0, 0.3)) +
  labs(title    = "Figure A2. Overall Agency Score Distribution (N = 127)",
       subtitle = "Each point = one participant. Dashed lines = classification thresholds.",
       x = "Overall Agency Mean Score (1\u20137)", y = NULL) +
  theme_classic(base_size = 11) +
  theme(plot.title       = element_text(face = "bold", size = 11),
        plot.subtitle    = element_text(size = 9, colour = "grey50"),
        axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank(),
        axis.line.y      = element_blank(),
        legend.position  = "bottom",
        legend.text      = element_text(size = 8),
        panel.background = element_rect(fill = "#FAFAFA"))

print(figA2)
ggsave("figA2_agency_distribution.png", figA2, width = 12, height = 3.8, dpi = 180)

make_heatmap <- function(group_var, var_order, fill_low, fill_high,
                         legend_label, x_label, fig_title, fig_sub) {
  ct <- as.data.frame(table(df$typology, df[[group_var]]))
  colnames(ct) <- c("typology", "group", "n")
  ct <- ct[ct$group %in% var_order, ]
  ct$group <- factor(ct$group, levels = var_order)
  totals <- aggregate(n ~ group, data = ct, FUN = sum)
  ct <- merge(ct, totals, by = "group", suffixes = c("","_total"))
  ct$pct   <- ct$n / ct$n_total * 100
  ct$label <- paste0(ct$n, "\n(", round(ct$pct), "%)")

  ggplot(ct, aes(x = group, y = typology, fill = pct)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = label, colour = pct > 45),
              size = 3.2, fontface = "bold") +
    scale_fill_gradient(low = fill_low, high = fill_high, name = legend_label) +
    scale_colour_manual(values = c("FALSE" = "#1a1a1a", "TRUE" = "white"), guide = "none") +
    scale_y_discrete(labels = type_labels_short) +
    labs(title = fig_title, subtitle = fig_sub, x = x_label, y = NULL) +
    theme_classic(base_size = 11) +
    theme(plot.title    = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 9, colour = "grey50"),
          axis.text.y   = element_text(size = 9),
          legend.position = "right")
}

cat("All typology figures saved.\n")
