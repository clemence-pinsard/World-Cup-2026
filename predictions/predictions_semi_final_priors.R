library(dplyr)
library(tidyr)
library(footBayes)
library(cmdstanr)
library(posterior)

set.seed(123)

stan_informative <- "~/work/World-Cup-2026/stan/diag_infl_biv_pois_dynamic_informative.stan"

# --- Stage 1: fit on the non-World-Cup matches ---
n_future <- 72L
n_total  <- nrow(results_footBayes_ready)
n_train  <- n_total - n_future

data_nonwc <- results_footBayes_ready[seq_len(n_train), ]
data_nonwc <- data_nonwc[data_nonwc$home_team != "HONGKONG" & 
                           data_nonwc$away_team != "HONGKONG", ]
teams_m1   <- unique(data_nonwc$home_team)
ranking_m1 <- ranking_footBayes %>% filter(team %in% teams_m1) %>% 
  mutate(rank_points = if_else(periods == 27, rank_points / 100, rank_points)) %>% 
  filter(periods != 31)

fit_m1 <- stan_foot(
  data          = data_nonwc,
  model         = "diag_infl_biv_pois",
  predict       = 0,
  ranking       = ranking_m1,
  init = 0,
  dynamic_type  = "seasonal",
  home_effect   = FALSE,
  chains        = 4, parallel_chains = 4,
  iter_warmup   = 1000,
  iter_sampling = 1000,
  seed          = 123
)

fit_m1$fit$save_object(file = "~/work/World-Cup-2026/results/diag_infl_M1_nonWC.rds")

saveRDS(fit_m1, "~/work/World-Cup-2026/results/diag_infl_M1_nonWC.rds")

fit_m1 <- readRDS(file = "~/work/World-Cup-2026/results/diag_infl_M1_nonWC.rds")

# --- Posterior mean/sd of att and def per team (last period) ---
extract_team_prior <- function(cmdstan_fit, par, teams, period = NULL) {
  d    <- posterior::as_draws_df(cmdstan_fit$draws(par))
  vars <- setdiff(names(d), c(".chain", ".iteration", ".draw"))
  K    <- length(teams)
  
  if (any(grepl(",", vars))) {
    if (is.null(period)) {
      ts     <- as.integer(sub(paste0(par, "\\[(\\d+),.*"), "\\1", vars))
      period <- max(ts)
    }
    cols <- sprintf("%s[%d,%d]", par, period, seq_len(K))
  } else {
    cols <- sprintf("%s[%d]", par, seq_len(K))
  }
  
  data.frame(
    team = teams,
    mean = vapply(cols, function(cc) mean(d[[cc]]), numeric(1)),
    sd   = vapply(cols, function(cc) stats::sd(d[[cc]]), numeric(1)),
    row.names = NULL
  )
}

att_m1 <- extract_team_prior(fit_m1$fit, "att", teams_m1)
def_m1 <- extract_team_prior(fit_m1$fit, "def", teams_m1)

# --- Stage 2: World-Cup matches only ---
wc_played_groupstage1 <- data.frame(
  periods    = rep(1, 24),   
  home_team  = c("MEXICO","SOUTHKOREA","CANADA","UNITEDSTATES",
                 "QATAR","BRAZIL","HAITI","AUSTRALIA",
                 "GERMANY", "NETHERLANDS","IVORYCOAST","SWEDEN",
                 "SPAIN","BELGIUM","SAUDIARABIA","IRAN",
                 "FRANCE","IRAQ","ARGENTINA","AUSTRIA",
                 "PORTUGAL", "ENGLAND", "GHANA", "UZBEKISTAN"),
  home_goals = c(2,2,1,4,1,1,0,2,
                 5,2,1,5,0,1,1,2,
                 3,0,3,3,1,4,1,1), 
  away_team  = c("SOUTHAFRICA","CZECHREPUBLIC","BOSNIAANDHERZEGOVINA","PARAGUAY",
                 "SWITZERLAND","MOROCCO","SCOTLAND","TURKEY",
                 "CURACAO", "JAPAN","ECUADOR","TUNISIA",
                 "CAPEVERDE","EGYPT","URUGUAY","NEWZEALAND",
                 "SENEGAL","NORWAY","ALGERIA","JORDAN",
                 "DRCONGO", "CROATIA", "PANAMA","COLOMBIA"),
  away_goals = c(0,1,1,1,1,1,1,0,
                 1,2,0,1,0,1,1,2,
                 1,2,0,1,1,2,0,3)
)

wc_played_groupstage2 <- data.frame(
  periods    = rep(1, 24),   
  home_team  = c("CZECHREPUBLIC","SWITZERLAND","CANADA","MEXICO",
                 "UNITEDSTATES","SCOTLAND","BRAZIL","TURKEY",
                 "NETHERLANDS","GERMANY","ECUADOR","TUNISIA",
                 "SPAIN","BELGIUM","URUGUAY","NEWZEALAND",
                 "ARGENTINA","FRANCE","NORWAY","JORDAN",
                 "PORTUGAL","ENGLAND","PANAMA","COLOMBIA"),
  home_goals = c(1,4,4,1,2,0,3,0,
                 5,2,0,0,4,0,2,1,
                 2,3,3,1,4,0,0,1), 
  away_team  = c("SOUTHAFRICA","BOSNIAANDHERZEGOVINA","QATAR","SOUTHKOREA",
                 "AUSTRALIA","MOROCCO","HAITI","PARAGUAY",
                 "SWEDEN","IVORYCOAST","CURACAO","JAPAN",
                 "SAUDIARABIA","IRAN","CAPEVERDE","EGYPT",
                 "AUSTRIA","IRAQ","SENEGAL","ALGERIA",
                 "UZBEKISTAN","GHANA","CROATIA","DRCONGO"),
  away_goals = c(1,1,0,0,0,1,0,1,
                 1,1,0,4,0,0,2,3,
                 0,0,2,2,0,0,1,0)
)

wc_played_groupstage3 <- data.frame(
  periods = rep(1,12),
  home_team = c("SWITZERLAND","BOSNIAANDHERZEGOVINA","MOROCCO","SCOTLAND",
                "SOUTHAFRICA","CZECHREPUBLIC","CURACAO","ECUADOR",
                "TUNISIA","JAPAN","TURKEY","PARAGUAY",
                "NORWAY","SENEGAL","CAPEVERDE","URUGUAY",
                "NEWZEALAND","EGYPT","PANAMA","CROATIA",
                "COLOMBIA","DRCONGO","ALGERIA","JORDAN"),
  home_goals = c(2,3,4,0,1,0,0,2,
                 1,1,3,0,1,5,0,0,
                 1,1,0,2,0,3,3,1),
  away_team = c("CANADA","QATAR","HAITI","BRAZIL",
                "SOUTHKOREA","MEXICO","IVORYCOAST","GERMANY",
                "NETHERLANDS","SWEDEN","UNITEDSTATES","AUSTRALIA",
                "FRANCE","IRAQ","SAUDIARABIA","SPAIN",
                "BELGIUM","IRAN","ENGLAND","GHANA",
                "PORTUGAL","UZBEKISTAN","AUSTRIA","ARGENTINA"),
  away_goals = c(1,1,2,3,0,3,2,1,
                 3,1,2,0,4,0,0,1,
                 5,1,2,1,0,1,3,3)
)

wc_played_round32 <- data.frame(
  periods = rep(1,16),
  home_team = c("SOUTHAFRICA","BRAZIL","GERMANY","NETHERLANDS",
                "IVORYCOAST","FRANCE","MEXICO","ENGLAND",
                "BELGIUM","UNITEDSTATES","SPAIN","PORTUGAL",
                "SWITZERLAND","AUSTRALIA","ARGENTINA","COLOMBIA"),
  home_goals = c(0,2,1,1,1,3,2,2,
                 3,2,3,2,2,1,3,1),
  away_team = c("CANADA","JAPAN","PARAGUAY","MOROCCO",
                "NORWAY","SWEDEN","ECUADOR","DRCONGO",
                "SENEGAL","BOSNIAANDHERZEGOVINA","AUSTRIA","CROATIA",
                "ALGERIA","EGYPT","CAPEVERDE","GHANA"),
  away_goals = c(1,1,1,1,2,0,0,1,
                 2,0,0,1,0,1,2,0)
)

wc_played_round16 <- data.frame(
  periods = rep(1,8),
  home_team = c("CANADA","PARAGUAY","BRAZIL","MEXICO",
                "PORTUGAL","UNITEDSTATES","ARGENTINA","SWITZERLAND"),
  home_goals = c(0,0,1,2,0,1,3,0),
  away_team = c("MOROCCO","FRANCE","NORWAY","ENGLAND",
                "SPAIN","BELGIUM","EGYPT","COLOMBIA"),
  away_goals = c(3,1,2,3,0,4,2,0)
)

wc_played_quarter <- data.frame(
  periods = rep(1,4),
  home_team = c("FRANCE","SPAIN","NORWAY","ARGENTINA"),
  home_goals = c(2,2,1,1),
  away_team = c("MOROCCO","BELGIUM","ENGLAND","SWITZERLAND"),
  away_goals = c(0,1,1,1)
)

wc_future <- data.frame(
  periods = rep(1,2),
  home_team = c("FRANCE","ENGLAND"),
  home_goals = rep(NA_real_,2),
  away_team = c("SPAIN","ARGENTINA"),
  away_goals = rep(NA_real_,2)
)

wc_data <- rbind(wc_played_groupstage1, wc_played_groupstage2, wc_played_groupstage3, wc_played_round32, wc_played_round16, wc_played_quarter, wc_future)
n_pred  <- nrow(wc_future)

teams_m2   <- unique(wc_data$home_team)
ranking_m2 <- ranking_footBayes %>%
  filter(team %in% unique(c(wc_data$home_team, wc_data$away_team))) %>%
  group_by(team) %>% arrange(periods, .by_group = TRUE) %>%
  mutate(rank_points = rank_points[which.max(periods)]) %>%
  ungroup() %>% mutate(periods = 1L) %>% distinct()

# --- Align priors to the stage-2 team order ---
idx <- match(teams_m2, att_m1$team)

prior_att_mean <- ifelse(is.na(idx), 0, att_m1$mean[idx])
prior_att_sd   <- ifelse(is.na(idx), 1, att_m1$sd[idx])
prior_def_mean <- ifelse(is.na(idx), 0, def_m1$mean[idx])
prior_def_sd   <- ifelse(is.na(idx), 1, def_m1$sd[idx])

a0 <- 1
prior_att_sd <- prior_att_sd / sqrt(a0)
prior_def_sd <- prior_def_sd / sqrt(a0)

if (any(is.na(idx)))
  message("WC teams without a stage-1 posterior: ",
          paste(teams_m2[is.na(idx)], collapse = ", "))

# --- Baseline fit (no priors), also used to build the Stan data ---
fit_m2_base <- stan_foot(
  data          = wc_data,
  model         = "diag_infl_biv_pois",
  predict       = n_pred,
  ranking       = ranking_m2,
  dynamic_type  = "seasonal",
  init = 0,
  home_effect   = FALSE,
  chains        = 4, parallel_chains = 4,
  iter_warmup   = 1000,
  iter_sampling = 1000,
  seed          = 456
)

# --- Fit with informative priors ---
sdata <- fit_m2_base$stan_data
sdata$ind_inf_prior  <- 1L
sdata$prior_att_mean <- as.array(prior_att_mean)
sdata$prior_att_sd   <- as.array(prior_att_sd)
sdata$prior_def_mean <- as.array(prior_def_mean)
sdata$prior_def_sd   <- as.array(prior_def_sd)

mod_inf <- cmdstan_model(stan_informative)

fit_semi <- mod_inf$sample(
  data          = sdata,
  chains        = 4, parallel_chains = 4,
  init = 0,
  iter_warmup   = 1000,
  iter_sampling = 1000,
  adapt_delta   = 0.9,
  seed          = 456
)

# --- Match probabilities and difference of goals between the teams ---
out_inf_semi <- list(
  fit        = fit_semi,
  data       = wc_data,
  stan_data  = sdata,
  stan_code  = mod_inf$code(),
  stan_args  = list(),
  alg_method = "MCMC"
)
class(out_inf_semi) <- c("stanFoot", "footBayes")

yp     <- posterior::as_draws_matrix(fit_semi$draws("y_prev"))
th_all <- posterior::as_draws_matrix(fit_semi$draws("theta_home_prev"))
ta_all <- posterior::as_draws_matrix(fit_semi$draws("theta_away_prev"))
tc_all <- posterior::as_draws_matrix(fit_semi$draws("theta_corr_prev"))

goal_diff_stats <- function(n) {
  h    <- yp[, sprintf("y_prev[%d,1]", n)]
  a    <- yp[, sprintf("y_prev[%d,2]", n)]
  diff <- h - a
  
  xg_home <- th_all[, n] + tc_all[, n]
  xg_away <- ta_all[, n] + tc_all[, n]
  
  data.frame(
    goal_diff_mean     = mean(diff),
    goal_diff_median   = median(diff),
    goal_diff_sd       = sd(diff),
    goal_diff_q05      = quantile(diff, 0.05),
    goal_diff_q95      = quantile(diff, 0.95),
    most_likely_score  = {
      tab <- table(paste(h, a, sep = "-"))
      names(tab)[which.max(tab)]
    },
    xg_home_mean = mean(xg_home),
    xg_home_sd   = sd(xg_home),
    xg_away_mean = mean(xg_away),
    xg_away_sd   = sd(xg_away)
  )
}

prob_inf_semi <- tryCatch(
  {
    res <- foot_prob(object = out_inf_semi, data = wc_data)
    
    # on ajoute l'écart de buts au résultat de foot_prob
    gd <- do.call(rbind, lapply(seq_len(n_pred), goal_diff_stats))
    res$prob_table <- cbind(res$prob_table, gd)
    res
  },
  error = function(e) {
    res <- lapply(seq_len(n_pred), function(n) {
      h <- yp[, sprintf("y_prev[%d,1]", n)]
      a <- yp[, sprintf("y_prev[%d,2]", n)]
      
      cbind(
        data.frame(
          home     = wc_future$home_team[n],
          away     = wc_future$away_team[n],
          home_win = mean(h > a),
          draw     = mean(h == a),
          away_win = mean(h < a)
        ),
        goal_diff_stats(n)
      )
    })
    list(prob_table = do.call(rbind, res), prob_plot = NULL)
  }
)

print(prob_inf_semi$prob_table)

write.csv(prob_inf_semi$prob_table, 
          file = "~/work/World-Cup-2026/results/semi_diag_infl_biv_pois_priors.csv", 
          row.names = FALSE)

print(prob_inf)

prob_inf_semi$prob_table <- prob_inf_semi$prob_table %>% 
  dplyr::select(home_team, away_team, prob_h, prob_d, prob_a, mlo, goal_diff_mean, goal_diff_sd)

colnames(prob_inf_semi$prob_table) <- c(
  "home", "away", "home_win", "draw", "away_win", "mlo",
  "goal_diff_mean", "goal_diff_sd"
)

write.csv(prob_inf_semi$prob_table, 
          file = "~/work/World-Cup-2026/val/semi.csv", 
          row.names = FALSE)

p_4 <- prob_inf_semi$prob_plot

new_names <- c(
  "MEXICO"          = "Mexico",
  "SOUTHKOREA"      = "South Korea",
  "CANADA"            = "Canada",
  "UNITEDSTATES"      = "United States",
  "QATAR"      = "Qatar",
  "BRAZIL"           = "Brazil",
  "HAITI"            = "Haiti",
  "AUSTRALIA"    = "Australia",
  "GERMANY"      = "Germany",
  "IVORYCOAST"          = "Ivory Coast",
  "NETHERLANDS"            = "Netherlands",
  "SWEDEN"          = "Sweden",
  "BELGIUM"           = "Belgium",
  "IRAN"          = "Iran",
  "SPAIN"           = "Spain",
  "SAUDIARABIA"           = "Saudi Arabia",
  "FRANCE"          = "France",
  "IRAQ"          = "Iraq", 
  "ARGENTINA"          = "Argentina",
  "AUSTRIA"        = "Austria",
  "PORTUGAL"           = "Portugal",
  "UZBEKISTAN"      = "Uzbekistan",
  "ENGLAND"   = "England",
  "GHANA"         = "Ghana",
  "CZECHREPUBLIC"         = "Czech Republic",
  "SWITZERLAND"          = "Switzerland",
  "SCOTLAND"           = "Scotland",
  "TURKEY"         = "Turkey",
  "ECUADOR"            = "Ecuador",
  "TUNISIA"        = "Tunisia",
  "NEWZEALAND"          = "New Zealand",
  "URUGUAY"         = "Uruguay",
  "NORWAY"       = "Norway",
  "JORDAN"          = "Jordan",
  "COLOMBIA"     = "Colombia",
  "PANAMA"          = "Panama",
  "SOUTHAFRICA"     = "South Africa",
  "BOSNIAANDHERZEGOVINA"           = "Bosnia and Herzegovina",
  "MOROCCO"             = "Morocco",
  "PARAGUAY"       = "Paraguay",
  "CURACAO" = "Curacao",
  "JAPAN" = "Japan",
  "EGYPT" = "Egypt",
  "CAPEVERDE" = "Cape Verde",
  "SENEGAL" = "Senegal",
  "ALGERIA" = "Algeria",
  "DRCONGO" = "DR Congo",
  "CROATIA" = "Croatia"
)

for (old in names(new_names)) {
  p_4$data$new_matches <- gsub(old, new_names[old], p_4$data$new_matches, ignore.case = TRUE)
}

p_4$theme$axis.text.x$size <- 10
p_4$theme$axis.text.y$size <- 10
p_4$theme$axis.title$size  <- 10
p_4$theme$strip.text$size  <- 10 

p_4 <- p_4 + theme(
  strip.text = element_text(size = 10, face = "bold"),  
  axis.text  = element_text(size = 10),                 
  axis.title = element_text(size = 10, face = "bold"), 
  plot.title = element_text(size = 14, face = "bold") 
)

p_4 <- p_4 + guides(color = "none")

p_4

ggsave("~/work/World-Cup-2026/results/plot_diag_infl_semi_priors.png", plot = p_4, width = 20, height = 15, dpi = 300)

