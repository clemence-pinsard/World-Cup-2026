summary(results_footBayes_ready)

library(dplyr)

opponents_by_team <- lapply(wc48_teams, function(team) {
  home_opponents <- results_footBayes_ready %>%
    filter(home_team == team, !is.na(home_goals), !is.na(away_goals)) %>%
    pull(away_team)
  
  away_opponents <- results_footBayes_ready %>%
    filter(away_team == team, !is.na(home_goals), !is.na(away_goals)) %>%
    pull(home_team)
  
  unique(c(home_opponents, away_opponents))
})

names(opponents_by_team) <- wc48_teams
opponents_by_team

compare_opponents <- function(team1, team2, opponents_list) {
  
  # Vérification que les équipes existent dans la liste
  if (!team1 %in% names(opponents_list)) stop(paste("Équipe introuvable :", team1))
  if (!team2 %in% names(opponents_list)) stop(paste("Équipe introuvable :", team2))
  
  # Adversaires de chaque équipe
  opponents_team1 <- opponents_list[[team1]]
  opponents_team2 <- opponents_list[[team2]]
  
  # Adversaires en commun
  common_opponents <- intersect(opponents_team1, opponents_team2)
  
  # Affichage
  cat("Adversaires de", team1, "(", length(opponents_team1), ") :\n")
  print(opponents_team1)
  
  cat("\nAdversaires de", team2, "(", length(opponents_team2), ") :\n")
  print(opponents_team2)
  
  cat("\nAdversaires en commun (", length(common_opponents), ") :\n")
  print(common_opponents)
  
  # Retourne une liste avec tout
  invisible(list(
    team1           = opponents_team1,
    team2           = opponents_team2,
    common          = common_opponents
  ))
}

compare_opponents("BRAZIL", "MOROCCO",opponents_by_team)
compare_opponents("FRANCE", "SENEGAL",opponents_by_team)
compare_opponents("GHANA", "PANAMA",opponents_by_team)
compare_opponents("ARGENTINA", "ALGERIA",opponents_by_team)

compare_opponents("MEXICO", "SOUTHAFRICA", opponents_by_team)
compare_opponents("SOUTHKOREA", "CZECHREPUBLIC",opponents_by_team)
compare_opponents("CANADA", "BOSNIAANDHERZEGOVINA",opponents_by_team)
compare_opponents("UNITEDSTATES", "PARAGUAY",opponents_by_team)

compare_opponents("QATAR", "SWITZERLAND",opponents_by_team)
compare_opponents("HAITI", "SCOTLAND",opponents_by_team)
compare_opponents("AUSTRALIA", "TURKEY",opponents_by_team)
compare_opponents("GERMANY", "CURACAO",opponents_by_team)

compare_opponents("IVORYCOAST", "ECUADOR",opponents_by_team)
compare_opponents("NETHERLANDS", "JAPAN",opponents_by_team)
compare_opponents("SWEDEN", "TUNISIA",opponents_by_team)
compare_opponents("BELGIUM", "EGYPT",opponents_by_team)

compare_opponents("IRAN", "NEWZEALAND",opponents_by_team)
compare_opponents("SPAIN", "CAPEVERDE",opponents_by_team)
compare_opponents("SAUDIARABIA", "URUGUAY",opponents_by_team)
compare_opponents("IRAQ", "NORWAY",opponents_by_team)

compare_opponents("AUSTRIA", "JORDAN",opponents_by_team)
compare_opponents("PORTUGAL", "DRCONGO",opponents_by_team)
compare_opponents("UZBEKISTAN", "COLOMBIA",opponents_by_team)
compare_opponents("ENGLAND", "CROATIA",opponents_by_team)


