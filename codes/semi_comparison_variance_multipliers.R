library(dplyr)
library(tidyr)
library(footBayes)
library(cmdstanr)
library(posterior)
library(ggplot2)

set.seed(123)

# Find the repository root whether the script is launched from the project root or from a subfolder such as codes/.
find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  
  repeat {
    if (dir.exists(file.path(current, "stan")) &&
        dir.exists(file.path(current, "results"))) {
      return(current)
    }
    
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop(
        "Project root not found. Run the script from inside the World-Cup-2026 repository."
      )
    }
    current <- parent
  }
}

project_dir <- find_project_root()
results_dir <- file.path(project_dir, "results")
plots_dir   <- file.path(results_dir, "variance_plots")

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

stan_informative <- file.path(
  project_dir,
  "stan",
  "diag_infl_biv_pois_dynamic_informative.stan"
)

required_objects <- c("results_footBayes_ready", "ranking_footBayes")
missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1), inherits = TRUE)
]

if (length(missing_objects) > 0) {
  stop(
    "Load the data-preparation code first. Missing object(s): ",
    paste(missing_objects, collapse = ", ")
  )
}

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

# Cache only the extracted numerical priors, not the CmdStan fit object.
# A serialized CmdStan fit often keeps paths to temporary CSV files; those
# paths become invalid after the original R session ends or on another PC.
stage1_prior_file <- file.path(
  results_dir,
  "diag_infl_M1_nonWC_team_priors.rds"
)

# Set this to TRUE only when the non-World-Cup data or model changes.
refit_stage1 <- FALSE

extract_team_prior <- function(cmdstan_fit, par, teams, period = NULL) {
  d    <- posterior::as_draws_df(cmdstan_fit$draws(par))
  vars <- setdiff(names(d), c(".chain", ".iteration", ".draw"))
  K    <- length(teams)
  
  if (any(grepl(",", vars))) {
    if (is.null(period)) {
      ts <- as.integer(
        sub(paste0(par, "\\[(\\d+),.*"), "\\1", vars)
      )
      period <- max(ts)
    }
    cols <- sprintf("%s[%d,%d]", par, period, seq_len(K))
  } else {
    cols <- sprintf("%s[%d]", par, seq_len(K))
  }
  
  missing_cols <- setdiff(cols, names(d))
  if (length(missing_cols) > 0) {
    stop(
      "Missing posterior column(s) for ", par, ": ",
      paste(head(missing_cols, 10), collapse = ", ")
    )
  }
  
  data.frame(
    team = teams,
    mean = vapply(cols, function(cc) mean(d[[cc]]), numeric(1)),
    sd   = vapply(
      cols,
      function(cc) stats::sd(d[[cc]]),
      numeric(1)
    ),
    row.names = NULL
  )
}

if (!refit_stage1 && file.exists(stage1_prior_file)) {
  message(
    "Loading cached stage-1 numerical priors: ",
    stage1_prior_file
  )
  
  cached_priors <- readRDS(stage1_prior_file)
  
  required_cache_names <- c("att_m1", "def_m1", "teams_m1")
  missing_cache_names <- setdiff(
    required_cache_names,
    names(cached_priors)
  )
  
  if (length(missing_cache_names) > 0) {
    stop(
      "Invalid stage-1 prior cache. Missing: ",
      paste(missing_cache_names, collapse = ", "),
      ". Delete ", stage1_prior_file,
      " and rerun the script."
    )
  }
  
  if (!identical(cached_priors$teams_m1, teams_m1)) {
    stop(
      "The cached stage-1 priors do not match the current team order. ",
      "Delete ", stage1_prior_file,
      " and rerun the script."
    )
  }
  
  att_m1 <- cached_priors$att_m1
  def_m1 <- cached_priors$def_m1
  
} else {
  message(
    "Estimating stage-1 model on non-World-Cup matches..."
  )
  
  fit_m1 <- stan_foot(
    data            = data_nonwc,
    model           = "diag_infl_biv_pois",
    predict         = 0,
    ranking         = ranking_m1,
    init            = 0,
    dynamic_type    = "seasonal",
    home_effect     = FALSE,
    chains          = 4,
    parallel_chains = 4,
    iter_warmup     = 1000,
    iter_sampling   = 1000,
    seed            = 123
  )
  
  att_m1 <- extract_team_prior(
    fit_m1$fit,
    "att",
    teams_m1
  )
  
  def_m1 <- extract_team_prior(
    fit_m1$fit,
    "def",
    teams_m1
  )
  
  saveRDS(
    list(
      att_m1   = att_m1,
      def_m1   = def_m1,
      teams_m1 = teams_m1
    ),
    stage1_prior_file
  )
  
  message(
    "Stage-1 numerical priors saved to: ",
    stage1_prior_file
  )
  
  rm(fit_m1)
  invisible(gc())
}


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
  periods = rep(1,24),
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

# --- Align stage-1 priors with the stage-2 team order ---
idx <- match(teams_m2, att_m1$team)

prior_att_mean    <- ifelse(is.na(idx), 0, att_m1$mean[idx])
prior_att_sd_base <- ifelse(is.na(idx), 1, att_m1$sd[idx])
prior_def_mean    <- ifelse(is.na(idx), 0, def_m1$mean[idx])
prior_def_sd_base <- ifelse(is.na(idx), 1, def_m1$sd[idx])

if (any(is.na(idx))) {
  message(
    "WC teams without a stage-1 posterior: ",
    paste(teams_m2[is.na(idx)], collapse = ", ")
  )
}

# a0 modifies prior precision:
#   prior SD       = original SD / sqrt(a0)
#   prior variance = original variance / a0
#
# Therefore:
#   a0 = 2    -> variance divided by 2, stronger priors
#   a0 = 1    -> original priors
#   a0 = 0.5  -> variance multiplied by 2, weaker priors
#   a0 = 0.25 -> variance multiplied by 4
#   a0 = 0.1  -> variance multiplied by 10
a0_values <- c(1, 2, 0.5, 0.25, 0.1)

# The baseline fit is needed only once to construct the Stan data.
message("Building the stage-2 Stan data...")

fit_m2_base <- stan_foot(
  data            = wc_data,
  model           = "diag_infl_biv_pois",
  predict         = n_pred,
  ranking         = ranking_m2,
  dynamic_type    = "seasonal",
  init            = 0,
  home_effect     = FALSE,
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 1000,
  iter_sampling   = 1000,
  seed            = 456
)

sdata_base <- fit_m2_base$stan_data
mod_inf    <- cmdstan_model(stan_informative)

goal_diff_stats <- function(
    n,
    yp,
    theta_home,
    theta_away,
    theta_corr
) {
  h <- yp[, sprintf("y_prev[%d,1]", n)]
  a <- yp[, sprintf("y_prev[%d,2]", n)]
  
  goal_diff <- h - a
  xg_home   <- theta_home[, n] + theta_corr[, n]
  xg_away   <- theta_away[, n] + theta_corr[, n]
  
  score_frequency <- table(paste(h, a, sep = "-"))
  
  data.frame(
    goal_diff_mean    = mean(goal_diff),
    goal_diff_median  = median(goal_diff),
    goal_diff_sd      = stats::sd(goal_diff),
    goal_diff_q05     = unname(stats::quantile(goal_diff, 0.05)),
    goal_diff_q95     = unname(stats::quantile(goal_diff, 0.95)),
    most_likely_score = names(score_frequency)[which.max(score_frequency)],
    xg_home_mean      = mean(xg_home),
    xg_home_sd        = stats::sd(xg_home),
    xg_away_mean      = mean(xg_away),
    xg_away_sd        = stats::sd(xg_away),
    row.names         = NULL
  )
}

extract_prediction_table <- function(fit, sdata, model, a0) {
  out_inf <- list(
    fit        = fit,
    data       = wc_data,
    stan_data  = sdata,
    stan_code  = model$code(),
    stan_args  = list(),
    alg_method = "MCMC"
  )
  class(out_inf) <- c("stanFoot", "footBayes")
  
  yp <- posterior::as_draws_matrix(fit$draws("y_prev"))
  theta_home <- posterior::as_draws_matrix(
    fit$draws("theta_home_prev")
  )
  theta_away <- posterior::as_draws_matrix(
    fit$draws("theta_away_prev")
  )
  theta_corr <- posterior::as_draws_matrix(
    fit$draws("theta_corr_prev")
  )
  
  gd <- dplyr::bind_rows(
    lapply(
      seq_len(n_pred),
      goal_diff_stats,
      yp = yp,
      theta_home = theta_home,
      theta_away = theta_away,
      theta_corr = theta_corr
    )
  )
  
  probability_result <- tryCatch(
    foot_prob(object = out_inf, data = wc_data),
    error = function(e) {
      message(
        "foot_prob failed for a0 = ", a0,
        ". Using posterior simulations directly. Error: ",
        conditionMessage(e)
      )
      NULL
    }
  )
  
  if (!is.null(probability_result) &&
      !is.null(probability_result$prob_table)) {
    prediction_table <- cbind(probability_result$prob_table, gd)
    prediction_plot  <- probability_result$prob_plot
  } else {
    probability_rows <- lapply(seq_len(n_pred), function(n) {
      h <- yp[, sprintf("y_prev[%d,1]", n)]
      a <- yp[, sprintf("y_prev[%d,2]", n)]
      
      data.frame(
        home     = wc_future$home_team[n],
        away     = wc_future$away_team[n],
        home_win = mean(h > a),
        draw     = mean(h == a),
        away_win = mean(h < a),
        row.names = NULL
      )
    })
    
    prediction_table <- cbind(
      dplyr::bind_rows(probability_rows),
      gd
    )
    prediction_plot <- NULL
  }
  
  prediction_table <- prediction_table %>%
    mutate(
      round = "Semi-finals",
      a0 = a0,
      prior_sd_multiplier = 1 / sqrt(a0),
      prior_variance_multiplier = 1 / a0,
      .before = 1
    )
  
  list(
    table = prediction_table,
    plot  = prediction_plot
  )
}

all_results <- vector("list", length(a0_values))

output_csv <- file.path(
  results_dir,
  "semi_variance_comparison.csv"
)

for (i in seq_along(a0_values)) {
  a0 <- a0_values[i]
  
  message(
    "\n========================================",
    "\nEstimating Semi-finals: a0 = ", a0,
    " | variance multiplier = ", 1 / a0,
    "\n========================================"
  )
  
  sdata <- sdata_base
  sdata$ind_inf_prior  <- 1L
  sdata$prior_att_mean <- as.array(prior_att_mean)
  sdata$prior_att_sd   <- as.array(
    prior_att_sd_base / sqrt(a0)
  )
  sdata$prior_def_mean <- as.array(prior_def_mean)
  sdata$prior_def_sd   <- as.array(
    prior_def_sd_base / sqrt(a0)
  )
  
  fit_m2_inf <- mod_inf$sample(
    data            = sdata,
    chains          = 4,
    parallel_chains = 4,
    init            = 0,
    iter_warmup     = 1000,
    iter_sampling   = 1000,
    adapt_delta     = 0.9,
    seed            = 456
  )
  
  extracted <- extract_prediction_table(
    fit   = fit_m2_inf,
    sdata = sdata,
    model = mod_inf,
    a0    = a0
  )
  
  all_results[[i]] <- extracted$table
  
  # Save progress after every value of a0. A long run is therefore not lost
  # if a later model fails.
  current_results <- dplyr::bind_rows(
    all_results[seq_len(i)]
  )
  
  write.csv(
    current_results,
    file = output_csv,
    row.names = FALSE
  )
  
  print(extracted$table)
  
  if (!is.null(extracted$plot)) {
    a0_tag <- gsub(
      "\\.",
      "_",
      format(a0, scientific = FALSE, trim = TRUE)
    )
    
    plot_file <- file.path(
      plots_dir,
      paste0(
        "semi_variance_a0_",
        a0_tag,
        ".png"
      )
    )
    
    prediction_plot <- extracted$plot +
      ggplot2::ggtitle(
        paste0(
          "Semi-finals: prior sensitivity, a0 = ",
          a0
        )
      ) +
      ggplot2::guides(color = "none")
    
    ggplot2::ggsave(
      filename = plot_file,
      plot     = prediction_plot,
      width    = 20,
      height   = 15,
      dpi      = 300
    )
  }
  
  rm(fit_m2_inf)
  invisible(gc())
}

variance_results <- dplyr::bind_rows(all_results)

write.csv(
  variance_results,
  file = output_csv,
  row.names = FALSE
)

message(
  "\nFinished. Consolidated results saved to:\n",
  output_csv
)

print(variance_results)SS