ranking_raw <- read_csv("~/work/World-Cup-2026/data/ranking_fifa_historical.csv",
                        locale = locale(encoding = "latin1"),
                        show_col_types = FALSE)

ranking <- ranking_raw %>% 
  filter(date >= as.Date("2025-07-10"))

normalize_name <- function(x) {
  x %>% 
    str_to_upper() %>% 
    stri_trans_general("Latin-ASCII") %>%   
    str_replace_all("[^A-Z0-9]", "")        
}

ranking <- ranking %>% 
  mutate(team_norm = normalize_name(team))