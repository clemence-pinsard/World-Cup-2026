ranking2025_11_19_raw <- read_csv2("~/work/World-Cup-2026/data/ranking_2025_11_19.csv",
                                  locale = locale(encoding = "latin1"),
                                  show_col_types = FALSE)

ranking2025_11_19_raw <- ranking2025_11_19_raw %>% 
  mutate(team = Team,
         total_points = Total_Points,
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

ranking2025_11_19 <- ranking2025_11_19_raw %>% 
  mutate(team_norm = normalize_name(team))