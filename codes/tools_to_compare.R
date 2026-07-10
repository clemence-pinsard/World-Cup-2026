library(dplyr)
library(stringr)

df_round16 <- read.csv("round16_diag_infl_biv_pois_priors.csv", stringsAsFactors = FALSE)

df_round32 <- read.csv("round32_diag_infl_biv_pois_priors.csv", stringsAsFactors = FALSE)

# Real outcome
df <- df %>%
  mutate(
    goals_home = as.integer(str_extract(result, "^\\d+")),
    goals_away = as.integer(str_extract(result, "\\d+$")),
    outcome = case_when(
      goals_home > goals_away ~ "H",
      goals_home == goals_away ~ "D",
      goals_home < goals_away ~ "A"
    ),
    o_h = as.integer(outcome == "H"),
    o_d = as.integer(outcome == "D"),
    o_a = as.integer(outcome == "A")
  )

# Brier score per match
df <- df %>%
  mutate(
    brier_match = (prob_h - o_h)^2 + (prob_d - o_d)^2 + (prob_a - o_a)^2
  )

# Mean Brier score
brier_score <- mean(df$brier_match)

cat("Mean Brier score :", round(brier_score, 4), "\n")

# Accuracy 1X2
df <- df %>%
  mutate(pred_outcome = case_when(
    prob_h >= prob_d & prob_h >= prob_a ~ "H",
    prob_d >= prob_h & prob_d >= prob_a ~ "D",
    TRUE ~ "A"
  ))
accuracy_1x2 <- mean(df$pred_outcome == df$outcome)

# Exact score
df <- df %>%
  mutate(
    mlo_score = str_extract(mlo, "^\\d+-\\d+"),
    exact_score_ok = mlo_score == result
  )
exact_score_pct <- mean(df$exact_score_ok)

cat("Accuracy 1X2 :", round(accuracy_1x2, 4), "\n")
cat("Exact score :", round(exact_score_pct, 4), "\n")