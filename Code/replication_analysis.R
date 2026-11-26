# POLI 100X Replication & Sensitivity Analysis
# Butler & Nickerson (2011): "Can Learning Constituency Opinion Affect How Legislators Vote?"
# Author: Nolan Yumiaco (UC San Diego)

library(tidyverse)
library(estimatr)
options(scipen = 999)

# Resolve data path whether script is run from project root or Code/
data_path <- if (file.exists("Data/butler_nickerson_clean.csv")) {
  "Data/butler_nickerson_clean.csv"
} else if (file.exists("../Data/butler_nickerson_clean.csv")) {
  "../Data/butler_nickerson_clean.csv"
} else {
  stop("Could not locate Data/butler_nickerson_clean.csv")
}

df <- read_csv(data_path, show_col_types = FALSE) |>
  mutate(
    low_support = as.numeric(q2full <= median(q2full, na.rm = TRUE)),
    flt = treatment * low_support
  )

# Summary statistics table for primary variables
summary_vars <- c("sb24", "treatment", "low_support", "flt", "rep", "dem_pres_2party")
summary_labels <- c(
  sb24 = "Voted Yes on SB-24 (sb24)",
  treatment = "Treatment Assigned (treatment)",
  low_support = "Low-Support District (low_support)",
  flt = "Interaction: Treatment x Low Support (flt)",
  rep = "Republican Legislator (rep)",
  dem_pres_2party = "District Dem. Presidential Vote Share"
)

summary_stats <- map_dfr(summary_vars, function(v) {
  vals <- df[[v]]
  tibble(
    Variable = summary_labels[[v]],
    N = sum(!is.na(vals)),
    Mean = mean(vals, na.rm = TRUE),
    SD = sd(vals, na.rm = TRUE),
    Min = min(vals, na.rm = TRUE),
    Max = max(vals, na.rm = TRUE)
  )
})

# -------------------------------------------------------------------------
# 1. Core Replication Models (Linear Probability Models with CR2 Cluster SEs)
# -------------------------------------------------------------------------

# Model 1: No controls
m1 <- lm_robust(
  sb24 ~ treatment + flt + low_support,
  data = df,
  clusters = match_category
)

# Model 2: Controlling for legislator party and 2004 district presidential vote
m2 <- lm_robust(
  sb24 ~ treatment + flt + low_support + rep + dem_pres_2party,
  data = df,
  clusters = match_category
)

# -------------------------------------------------------------------------
# 2. Extension Approach 1: Iterative Leave-One-Matched-Pair-Out Check (M2)
# -------------------------------------------------------------------------

pairings <- sort(unique(df$match_category))

leave_one_out <- map_dfr(pairings, function(pair_drop) {
  temp_df <- df |>
    filter(match_category != pair_drop)

  mod <- lm_robust(
    sb24 ~ treatment + flt + low_support + rep + dem_pres_2party,
    data = temp_df,
    clusters = match_category
  )

  flt_row <- tidy(mod) |> filter(term == "flt")

  tibble(
    dropped_pair = pair_drop,
    nobs = mod$nobs,
    nclusters = mod$nclusters,
    flt_coef = flt_row$estimate,
    flt_se = flt_row$std.error,
    ci_low = flt_row$conf.low,
    ci_high = flt_row$conf.high,
    p_value = flt_row$p.value
  )
}) |>
  mutate(
    influential_flag = if_else(
      dropped_pair %in% c(21, 32),
      "Top 2 Most Influential (Pairs 21 & 32)",
      "Other Matched Pair"
    )
  )

# Publication-ready leave-one-pair-out plot
baseline_flt <- unname(coef(m2)["flt"])

loo_plot <- ggplot(
  leave_one_out,
  aes(
    x = reorder(factor(dropped_pair), flt_coef),
    y = flt_coef,
    color = influential_flag
  )
) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "#64748b", linewidth = 0.6) +
  geom_hline(yintercept = baseline_flt, linetype = "dashed", color = "#1e3a8a", linewidth = 0.8) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0, alpha = 0.35, linewidth = 0.7) +
  geom_point(size = 2.6) +
  scale_color_manual(
    values = c(
      "Top 2 Most Influential (Pairs 21 & 32)" = "#dc2626",
      "Other Matched Pair" = "#1e3a8a"
    ),
    name = NULL
  ) +
  annotate(
    "text",
    x = 3,
    y = baseline_flt + 0.025,
    label = sprintf("Full-sample M2 FLT = %.3f", baseline_flt),
    color = "#1e3a8a",
    hjust = 0,
    size = 3.4,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 34.5,
    y = -0.210 + 0.04,
    label = "Drop Pair 32: -0.210",
    color = "#dc2626",
    hjust = 1,
    size = 3.2,
    fontface = "bold"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Leave-One-Pair-Out Sensitivity of the Treatment × Low-Support Interaction (FLT)",
    subtitle = "M2 linear probability model with CR2 95% confidence intervals (clustered by matched pair)",
    x = "Dropped Matched Pair (ordered by FLT estimate)",
    y = "Estimated FLT Coefficient (percentage points)"
  ) +
  theme_minimal(base_size = 11.5) +
  theme(
    plot.title = element_text(face = "bold", size = 12.5, color = "#0f172a"),
    plot.subtitle = element_text(color = "#475569", size = 10),
    axis.text.x = element_text(size = 7.5, angle = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

# -------------------------------------------------------------------------
# 3. Extension Approach 2: Drop Both Most Influential Pairs (21 & 32)
# -------------------------------------------------------------------------

drop_influential <- df |>
  filter(!match_category %in% c(21, 32))

m1_drop <- lm_robust(
  sb24 ~ treatment + flt + low_support,
  data = drop_influential,
  clusters = match_category
)

m2_drop <- lm_robust(
  sb24 ~ treatment + flt + low_support + rep + dem_pres_2party,
  data = drop_influential,
  clusters = match_category
)

# Print outputs when run directly from the command line
if (sys.nframe() == 0) {
  cat("\n--- Model 1 (Full Sample, No Controls) ---\n")
  print(summary(m1))
  cat("\n--- Model 2 (Full Sample, With Controls) ---\n")
  print(summary(m2))
  cat("\n--- Leave-One-Pair-Out Summary (FLT) ---\n")
  print(summary(leave_one_out$flt_coef))
  cat("\n--- Model 1 (Dropping Pairs 21 & 32) ---\n")
  print(summary(m1_drop))
  cat("\n--- Model 2 (Dropping Pairs 21 & 32) ---\n")
  print(summary(m2_drop))
}
