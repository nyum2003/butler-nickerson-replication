#Replication Project Code
#Nolan Y

rm(list = ls())
install.packages("estimatr")
library(tidyverse)
library(estimatr)
options(scipen = 999)

setwd("/Poli 100x/Replication.Project/Data")
df <- read_csv("butler_nickerson_clean.csv")

#Creating the FLT
df$low_support <- as.numeric(df$q2full <= median(df$q2full, na.rm = TRUE))
df$flt <- df$treatment * df$low_support

print(head(df), width = Inf)
#Added Summary Stats here 
summary(df)

#Replication Regression with NO controls (m1)
m1 <- lm_robust(
  sb24 ~ treatment + flt + low_support,
  data = df,
  clusters = match_category
)
summary(m1)

#Replication Regression with controls (m2)
m2 <- lm_robust(
  sb24 ~ treatment + flt + low_support + rep + dem_pres_2party,
  data = df,
  clusters = match_category
)
summary(m2)


#Replication Table With NO Controls
m1_table <- tibble(
  model = c("Treatment", "FLT", "Low Support"), M1_Coefs = c(
    coef(summary(m1))["treatment", "Estimate"],
    coef(summary(m1))["flt", "Estimate"],
    coef(summary(m1))["low_support", "Estimate"]
  )
)

m1_table

#Replication Table With Controls
m2_table <- tibble(
  model = c("Treatment", "FLT", "Low Support", "rep", "dem_pres2party"), M2_Coefs = c(
    coef(summary(m2))["treatment", "Estimate"],
    coef(summary(m2))["flt", "Estimate"],
    coef(summary(m2))["low_support", "Estimate"],
    coef(summary(m2))["rep", "Estimate"],
    coef(summary(m2))["dem_pres_2party", "Estimate"]
  )
)

m2_table

#Extension Portion//
# Approach 1
pairings <- unique(df$match_category)
pairings

set.seed(124)

# Open AI was consulted to learn how to use the map_dfr function
#Assisted with building lines 68 - 72
leave_one_out <- map_dfr(pairings, function(pair_drop) {
  
  temp_df <- df |>
    filter(match_category != pair_drop)
  
  mod <- lm_robust(
    sb24 ~ treatment + flt + low_support + rep + dem_pres_2party,
    data = temp_df,
    clusters = match_category
  )
  tibble(
    dropped_pair = pair_drop,
    flt_coef = coef(mod)["flt"],
    flt_se = tidy(mod) |>
      filter(term == "flt") |>
      pull(std.error),
    p_value = tidy(mod) |>
      filter(term == "flt") |>
      pull(p.value)
  )
})

#investigating the result
summary(leave_one_out$flt_coef)

#Graphing and Visualizing Extension 1
ggplot(leave_one_out,
       aes(x = reorder(dropped_pair, flt_coef),
           y = flt_coef)) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = coef(m2)["flt"],
             linetype = "solid")  +
  labs(
    title = "Leave One Pair Out Robustness Check",
    x = "Dropped Matched Pair",
    y = "Interaction Coefficient (FLT)"
  )

# Approach 2 (What if we just drop the two most influential as identified in Extension 1)
drop_influential <- df |>
  filter(match_category != 32 & match_category != 21)

#Running M2 again
m2_drop <- lm_robust(
  sb24 ~ treatment + flt + low_support + rep + dem_pres_2party,
  data = drop_influential,
  clusters = match_category
)

summary(m2_drop)

#Running M1 again
m1_drop <- lm_robust(sb24 ~ treatment + flt + low_support,
                     data = drop_influential,
                     clusters = match_category)
summary(m1_drop)
