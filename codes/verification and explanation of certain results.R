analyse <- groupstage1 %>% 
  filter((home_team %in% c("BRAZIL", "MOROCCO", "SENEGAL", "FRANCE", "ARGENTINA", "ALGERIA", "GHANA", "PANAMA")) | (away_team %in% c("BRAZIL", "MOROCCO", "SENEGAL", "FRANCE", "ARGENTINA", "ALGERIA", "GHANA", "PANAMA"))) %>% 
  arrange(desc(periods))

resultats_equipe <- function(data, equipe) {
  data %>%
    filter(home_team == equipe | away_team == equipe) %>%
    mutate(
      buts_pour    = ifelse(home_team == equipe, home_goals, away_goals),
      buts_contre  = ifelse(home_team == equipe, away_goals, home_goals),
      resultat     = case_when(
        buts_pour > buts_contre  ~ "V",
        buts_pour == buts_contre ~ "N",
        buts_pour < buts_contre  ~ "D",
        TRUE ~ "NA (non prédit)"
      )
    ) %>%
    select(periods, home_team, home_goals, away_team, away_goals, buts_pour, buts_contre, resultat)
}

resultats_equipe(analyse, "FRANCE")

matchs_na <- analyse %>%
  filter(is.na(home_goals) | is.na(away_goals))

matchs_na %>%
  tidyr::pivot_longer(cols = c(home_team, away_team), values_to = "equipe") %>%
  count(equipe, sort = TRUE)

bilan <- analyse %>%
  filter(!is.na(home_goals), !is.na(away_goals)) %>%
  tidyr::pivot_longer(
    cols = c(home_team, away_team),
    names_to = "cote", values_to = "equipe"
  ) %>%
  mutate(
    buts_pour   = ifelse(cote == "home_team", home_goals, away_goals),
    buts_contre = ifelse(cote == "home_team", away_goals, home_goals),
    resultat    = case_when(
      buts_pour > buts_contre  ~ "V",
      buts_pour == buts_contre ~ "N",
      TRUE                     ~ "D"
    )
  ) %>%
  group_by(equipe) %>%
  summarise(
    J  = n(),
    V  = sum(resultat == "V"),
    N  = sum(resultat == "N"),
    D  = sum(resultat == "D"),
    BP = sum(buts_pour),
    BC = sum(buts_contre),
    diff = BP - BC
  ) %>%
  arrange(desc(V), desc(diff))

bilan

bilan_periode <- analyse %>%
  filter(!is.na(home_goals), !is.na(away_goals)) %>%
  tidyr::pivot_longer(
    cols = c(home_team, away_team),
    names_to = "cote", values_to = "equipe"
  ) %>%
  mutate(
    buts_pour   = ifelse(cote == "home_team", home_goals, away_goals),
    buts_contre = ifelse(cote == "home_team", away_goals, home_goals),
    resultat    = case_when(
      buts_pour > buts_contre  ~ "V",
      buts_pour == buts_contre ~ "N",
      TRUE                     ~ "D"
    )
  ) %>%
  group_by(equipe, periods) %>%
  summarise(
    J       = n(),
    pct_V   = round(mean(resultat == "V") * 100, 1),
    pct_N   = round(mean(resultat == "N") * 100, 1),
    pct_D   = round(mean(resultat == "D") * 100, 1),
    moy_BP  = round(mean(buts_pour),   2),
    moy_BC  = round(mean(buts_contre), 2),
    .groups = "drop"
  ) %>%
  arrange(equipe, periods)

bilan_periode

zoom_equipe <- function(data, equipe) {
  data %>%
    filter(!is.na(home_goals), !is.na(away_goals)) %>%
    tidyr::pivot_longer(
      cols = c(home_team, away_team),
      names_to = "cote", values_to = "equipe_nom"
    ) %>%
    filter(equipe_nom == equipe) %>%
    mutate(
      buts_pour   = ifelse(cote == "home_team", home_goals, away_goals),
      buts_contre = ifelse(cote == "home_team", away_goals, home_goals),
      resultat    = case_when(
        buts_pour > buts_contre  ~ "V",
        buts_pour == buts_contre ~ "N",
        TRUE                     ~ "D"
      )
    ) %>%
    group_by(periods) %>%
    summarise(
      J      = n(),
      pct_V  = round(mean(resultat == "V") * 100, 1),
      pct_N  = round(mean(resultat == "N") * 100, 1),
      pct_D  = round(mean(resultat == "D") * 100, 1),
      moy_BP = round(mean(buts_pour),   2),
      moy_BC = round(mean(buts_contre), 2),
      diff   = round(mean(buts_pour - buts_contre), 2)
    ) %>%
    arrange(periods)
}

zoom_equipe(analyse, "GHANA")

tendance_globale <- analyse %>%
  filter(!is.na(home_goals), !is.na(away_goals)) %>%
  mutate(
    resultat_home = case_when(
      home_goals > away_goals  ~ "V",
      home_goals == away_goals ~ "N",
      TRUE                     ~ "D"
    )
  ) %>%
  group_by(periods) %>%
  summarise(
    J          = n(),
    pct_dom    = round(mean(resultat_home == "V") * 100, 1),  # % victoires domicile
    pct_nul    = round(mean(resultat_home == "N") * 100, 1),
    pct_ext    = round(mean(resultat_home == "D") * 100, 1),  # % victoires extérieur
    moy_buts   = round(mean(home_goals + away_goals), 2)
  )

tendance_globale

zoom_equipe_detail <- function(data, equipe, periodes = NULL) {
  
  if (!is.null(periodes)) {
    data <- data %>% filter(periods %in% periodes)
  }
  
  df <- data %>%
    filter(!is.na(home_goals), !is.na(away_goals)) %>%
    # Récupérer l'adversaire AVANT le pivot
    mutate(
      adversaire = ifelse(home_team == equipe, away_team, home_team)
    ) %>%
    tidyr::pivot_longer(
      cols = c(home_team, away_team),
      names_to = "cote", values_to = "equipe_nom"
    ) %>%
    filter(equipe_nom == equipe) %>%
    mutate(
      buts_pour   = ifelse(cote == "home_team", home_goals, away_goals),
      buts_contre = ifelse(cote == "home_team", away_goals, home_goals),
      resultat    = case_when(
        buts_pour > buts_contre  ~ "V",
        buts_pour == buts_contre ~ "N",
        TRUE                     ~ "D"
      )
    )
  
  # Le reste de la fonction reste identique...
  resume <- df %>%
    group_by(periods) %>%
    summarise(
      J      = n(),
      pct_V  = round(mean(resultat == "V") * 100, 1),
      pct_N  = round(mean(resultat == "N") * 100, 1),
      pct_D  = round(mean(resultat == "D") * 100, 1),
      moy_BP = round(mean(buts_pour),            2),
      moy_BC = round(mean(buts_contre),          2),
      diff   = round(mean(buts_pour - buts_contre), 2),
      .groups = "drop"
    ) %>%
    arrange(periods)
  
  detail_adversaires <- df %>%
    group_by(periods, resultat) %>%
    summarise(
      adversaires = paste0(
        adversaire, " (", buts_pour, "-", buts_contre, ")",
        collapse = ", "
      ),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from  = resultat,
      values_from = adversaires,
      values_fill = "—"
    ) %>%
    { 
      for (col in c("V", "N", "D")) {
        if (!col %in% names(.)) .[[col]] <- "—"
      }
      .
    } %>%
    select(periods, any_of(c("V", "N", "D"))) %>%
    arrange(periods)
  
  cat("\n========================================\n")
  cat(" Équipe :", equipe, "\n")
  if (!is.null(periodes)) cat(" Périodes :", paste(periodes, collapse = ", "), "\n")
  cat("========================================\n\n")
  
  cat("--- Résumé par période ---\n")
  print(resume)
  
  cat("\n--- Adversaires par période ---\n")
  cat("  V = victoires | N = nuls | D = défaites\n\n")
  print(detail_adversaires, width = Inf)
  
  invisible(list(resume = resume, detail = detail_adversaires))
}

zoom_equipe_detail(analyse, "FRANCE", periodes = c(26, 27, 28, 29, 30))
zoom_equipe_detail(analyse, "SENEGAL", periodes = c(26, 27, 28, 29, 30))

zoom_equipe_detail(analyse, "BRAZIL", periodes = c(26, 27, 28, 29, 30))
zoom_equipe_detail(analyse, "MOROCCO", periodes = c(26, 27, 28, 29, 30))

zoom_equipe_detail(analyse, "GHANA", periodes = c(26, 27, 28, 29, 30))
zoom_equipe_detail(analyse, "PANAMA", periodes = c(26, 27, 28, 29, 30))

zoom_equipe_detail(analyse, "ARGENTINA", periodes = c(26, 27, 28, 29, 30))
zoom_equipe_detail(analyse, "ALGERIA", periodes = c(26, 27, 28, 29, 30))
