ranking2026_06_11_raw <- read_csv("data/ranking_2026_06_11.csv",
                                  locale = locale(encoding = "latin1"),
                                  show_col_types = FALSE)

ranking2026_06_11_raw <- ranking2026_06_11_raw %>% 
  mutate(team = pays,
         total_points = points,
         id = NA,
         id_num = NA,
         team_short = NA) %>% 
  select(date, team, total_points, id, id_num, team_short)

normalize_name <- function(x) {
  x %>% 
    str_to_upper() %>% 
    stri_trans_general("Latin-ASCII") %>%   
    str_replace_all("[^A-Z0-9]", "")        
}

ranking2026_06_11 <- ranking2026_06_11_raw %>% 
  mutate(team_norm = normalize_name(team))