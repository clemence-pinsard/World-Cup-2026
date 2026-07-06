results_raw <- read_csv("~/work/World-Cup-2026/data/results.csv",
                        locale = locale(encoding = "latin1"),
                        show_col_types = FALSE)

results <- results_raw %>% 
  filter(date >= as.Date("2025-09-01"))

normalize_name <- function(x) {
  x %>% 
    str_to_upper() %>% 
    stri_trans_general("Latin-ASCII") %>%  
    str_replace_all("[^A-Z0-9]", "")  
}

results <- results %>% 
  mutate(
    home_norm = normalize_name(home_team),
    away_norm = normalize_name(away_team)
  )