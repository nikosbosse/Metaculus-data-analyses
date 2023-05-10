library(data.table)
library(dplyr)
library(scoringutils)
library(stringr)
library(jsonlite)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(purrr)
library(RColorBrewer)
library(JuliaCall)
library(foreach)
library(zoo)

# ============================================================================ #
#                                 Load data                                    #
# ============================================================================ #

binary_pred_raw <- fromJSON("data/predictions-binary.json") |>
  as.data.table() 

binary_questions_raw <- fromJSON("data/questions-binary.json") |>
  as.data.table() |>
  select(publish_time, 
         question_id,
         resolution,
         close_time, 
         resolve_time)

combined <- inner_join(
  binary_pred_raw, binary_questions_raw
) |>
  filter(!is.na(resolution)) |>
  mutate(close_time = pmin(close_time, resolve_time)) |>
  filter(t <= close_time) |>
  rename(cp_original = cp)





# ============================================================================ #
# helper functions
# ============================================================================ #

# function to calculate a score over the whole question period
time_weighted_score <- function(pred, 
                                score = "Brier", 
                                resolution, 
                                t, close_time) {
  t_min <- min(t)
  t_max <- close_time
  t_total = t_max - t_min
  
  durations = diff(c(as.numeric(t), as.numeric(t_max)))
  
  if (score == "Brier") {
    score <- (pred - resolution)^2
  } else if (score == "Log") {
    score <- -log(1 - abs(resolution - pred))
  } else if (score == "average") {
    score <- pred
  }
  
  score <- ((score * durations) / t_total)
  score <- sum(score)
  
  return(score)
}

# helper function to compute score based on the data frame format
score_df <- function(data, pred, ...) {
  time_weighted_score(
    pred = pred, 
    resolution = unique(na.omit(data$resolution)), 
    t = data$t, 
    close_time = unique(na.omit(data$close_time)), 
    ...
  )
}

remove_clear_questions <- function(df) {
  discard <- scores |>
    filter(Brier_cp < 0.0025 | Brier_cp > 0.9025) |>
    pull(question_id) |>
    unique()
  
  filter(df, !(question_id %in% discard))
}


summary_function <- function(scores, var = NULL) {
  tibble(
    min = min(scores[[var]]),
    mean = mean(scores[[var]]), 
    median = median(scores[[var]]),
    max = max(scores[[var]]), 
    sd = sd(scores[[var]])
  )
}

average_by_question <- function(scores) {
  scores |> 
    group_by(question_id, n_user, n_pred, resolution, Brier_cp) |>
    summarise(
      # Brier_without_sd = sd(Brier_without), 
      # rel_Brier_sd = sd(rel_Brier), 
      # diff_Brier_sd = sd(diff_Brier),
      Brier_without = mean(Brier_without), 
      rel_Brier = mean(rel_Brier), 
      diff_Brier = mean(diff_Brier)
    ) 
}

average_by_user <- function(scores) {
  scores |>
    group_by(user_id) |>
    summarise(
      n_questions = length(unique(question_id)),
      # Brier_without_sd = sd(Brier_without), 
      # rel_Brier_sd = sd(rel_Brier), 
      # diff_Brier_sd = sd(diff_Brier),
      Brier_without = mean(Brier_without), 
      rel_Brier = mean(rel_Brier), 
      diff_Brier = mean(diff_Brier), 
      rel_pred_by_user = mean(rel_pred_by_user)
    )
}

## helper function to determine whether a forecast is more or less confident
## than another baseline forecast
more_confident <- function(pred, pred_baseline) {
  distance_pred <- pmin(1 - pred, pred)
  distance_pred_baseline <- pmin(1 - pred_baseline, pred_baseline)
  more_confident <- distance_pred_baseline - distance_pred
  return(more_confident)
}

## helper function to filter for a certain minimum number of forecasters
filter_no_forecasters <- function(data, min_number = 100) {
  numbers <- binary_pred_raw |>
    group_by(question_id) |>
    summarise(n_forecasters = length(unique(user_id)))
  
  numbers |>
    filter(n_forecasters >= min_number) |>
    inner_join(data)
}






# ============================================================================ #
# Computations
# ============================================================================ #

# ==================== recomputing the CP without a user ===================== #
question_ids <- combined$question_id |> unique() |> sort()
# Rerunning results takes several hours
if (FALSE) {
  library(doParallel)
  #Setup backend to use many processors
  totalCores = detectCores()
  cluster <- makeCluster(3) 
  registerDoParallel(cluster)
  
  res <- list()
  out <- list()
  out <- foreach(q = question_ids, 
                 .packages = c("dplyr", "JuliaCall", "data.table")) %dopar% {
                   julia_setup()
                   julia_library("DataFrames")
                   julia_library("StatsBase")
                   julia_source("julia/community-prediction.jl")
                   
                   df <- dplyr::filter(combined, question_id %in% q)
                   users <- unique(df$user_id)
                   
                   scores_without <- list()
                   for (user in users) {
                     df_without <- julia_call("remove_user_and_compute_cp", df, user)
                     brier_without <- score_df(df_without, pred = df_without$cp)
                     log_without <- score_df(df_without, pred = df_without$cp, score = "Log")
                     
                     scores_without[[paste(user)]] <- 
                       data.frame(
                         question_id = q, 
                         user_id = user,
                         "Brier_without" = brier_without, 
                         "Log_without" = log_without
                       )
                   }
                   print(paste("finished question", q))
                   res[[paste(q)]] <- rbindlist(scores_without)
                 }
  saveRDS(out, "forecasters-contributions/results.rds")
  res <- rbindlist(out)
  fwrite(res, "forecasters-contributions/results.csv")
} else {
  res <- fread("forecasters-contributions/results.csv")
}

# ======================= compute original scores ============================ #

## Compute scores for the original community prediction
scores_original_cp <- combined |>
  group_by(question_id) |>
  summarise(
    Brier_cp = time_weighted_score(
      pred = cp_original, 
      resolution = unique(resolution), 
      t = t, 
      close_time = unique(close_time)), 
    Log_cp = time_weighted_score(
      score = "Log",
      pred = cp_original, 
      resolution = unique(resolution), 
      t = t, 
      close_time = unique(close_time)), 
    n_user = length(unique(user_id)), 
    n_pred = length(t), 
    resolution = unique(resolution)
  )

## compute original scores for each user
scores_original_users <- combined |>
  group_by(user_id, question_id, resolve_time) |>
  summarise(brier_user = time_weighted_score(
    pred = prediction, 
    resolution = unique(resolution), 
    t = t, 
    close_time = unique(close_time))
  ) |>
  arrange(user_id, resolve_time) |>
  group_by(user_id) |>
  mutate(cum_brier_user = cummean(brier_user))

## compute number of forecasts by user and question
forecasts_by_user_and_question <- combined |>
  group_by(user_id, question_id) |>
  summarise(n_pred_by_user = length(t)) |>
  group_by(question_id) |>
  mutate(avg_n_pred_by_user = mean(n_pred_by_user)) |>
  ungroup() |>
  mutate(rel_pred_by_user = n_pred_by_user / avg_n_pred_by_user) 

## compute number of questions a user has forecasted on
num_questions_forecasted_by_user <- combined |> 
  select(user_id, question_id, resolve_time) |>
  unique() |>
  group_by(user_id) |>
  mutate(cum_num_questions_forecasted = 1:n())

## get resolve times for all questions
question_resolve_times <- combined |>
  select(question_id, resolve_time) |>
  unique()

## Calculate difference between CP and prediction 
## at the time the prediction was made
diff_prediction_to_cp <- combined |>
  group_by(question_id) |>
  arrange(question_id, t) |>
  mutate(cp_user_saw = lag(cp_original), 
         diff_to_cp = prediction - cp_user_saw, 
         confidence = more_confident(prediction, cp_user_saw)
         ) |>
  select(user_id, question_id, t, cp_user_saw, diff_to_cp, confidence)

diff_prediction_to_cp_mean <- diff_prediction_to_cp |>
  group_by(user_id, question_id) |>
  summarise(diff_to_cp = mean(diff_to_cp, na.rm = TRUE), 
            confidence = mean(confidence, na.rm = TRUE))

## get the final scores data.frame used throughout
# combine with scores for altered community predictions
scores <- full_join(scores_original_cp, res) |>
  # negative number means cp was better with that person included
  mutate(
    diff_Brier = Brier_cp - Brier_without, 
    diff_Log = Log_cp - Log_without) |>
  select(question_id, n_user, n_pred, resolution, user_id, Brier_cp, 
         Brier_without, diff_Brier) |>
  mutate(
    rel_Brier = Brier_cp / Brier_without
  ) 

# filter scores, but save an unfiltered version
scores_unfiltered <- copy(scores)
scores <- scores_unfiltered |>
  filter_no_forecasters(min_number = 5) |>
  remove_clear_questions()


## Calculate cumulative Brier differences
cumulative_brier_diff <- scores |> 
  inner_join(scores_original_users) |>
  select(question_id, user_id, resolve_time, diff_Brier, brier_user, cum_brier_user) |>
  arrange(user_id, resolve_time) |> 
  group_by(user_id) |>
  mutate(cum_avg_diff_Brier = cummean(diff_Brier), 
         cum_diff_Brier = cumsum(diff_Brier))









# ============================================================================ #
# Analysis - overall 
# ============================================================================ #

# ======================== Overall data visualisation ======================== #

# helper function to create a histogram
plot_hist <- function(scores, xvar = "rel_Brier", 
                      xlims = NULL, n_bin = 100, 
                      label_fn = scales::label_comma()) {
  scores |>
    ggplot(aes_string(x = xvar)) +
    geom_histogram(aes(y = after_stat(count / sum(count))), bins = n_bin, 
                   colour = "white", linewidth = 0.01) + 
    theme_scoringutils() + 
    scale_y_continuous(label = \(x) {paste0(100 * x, "%")}) + 
    scale_x_continuous(limits = xlims, labels = label_fn)
}  

# helper function to create a cumulative density plot
plot_ecdf <- function(scores, xvar = "rel_Brier", 
                      xlims = NULL,
                      label_fn = scales::label_comma()) {
  
  scores |>
    ggplot(aes_string(x = xvar)) +
    geom_hline(yintercept = c(0.05, 0.95), linetype = "dashed", color = "grey60") + 
    stat_ecdf(geom = "step") + 
    coord_cartesian(xlim = xlims) + 
    scale_y_continuous(breaks = c(0.05, 0.25, 0.5, 0.75, 0.95), 
                       labels = scales::percent_format()) + 
    theme_scoringutils()
}

# histogram of differences
(hist_abs_contributions <- scores |>
  remove_clear_questions() |>
  plot_hist(xvar = "diff_Brier", xlims = c(-0.01, 0.01), 
            n_bin = 130,
            label_fn = \(x) as.character(x)) + 
  labs(y = "Proportion", x = "Difference in Brier Score"))

# histogram of relative scores
(hist_rel_contributions <- scores |>
  plot_hist(x = "rel_Brier", xlims = c(0.9, 1.1), 
            label_fn = \(x) as.character(x)) + 
  labs(y = "Proportion", x = "Relative Bier Score"))

contributions_combined <- hist_abs_contributions + hist_rel_contributions &
  plot_annotation(tag_levels = "A")
ggsave("forecasters-contributions/plots/hist-contributions-combined.jpg", 
       plot = contributions_combined,
       width = 7, height = 3)


# CDF for differences
diff_cdf <- scores |> 
  plot_ecdf(xvar = "diff_Brier", xlims = c(-0.005, 0.005)) + 
  scale_x_continuous(labels = \(x) as.character(x), breaks = seq(-0.005, 0.005, 0.0025)) +
  labs(y = "Cumulative Proportion", x = "Difference in Brier Score")

# CDF for relative scores 
rel_score_cdf <- scores |> 
  plot_ecdf(xlims = c(0.97, 1.03)) + 
  scale_x_continuous(labels = \(x) as.character(x), breaks = seq(0.97, 1.03, 0.01)) + 
  labs(y = "Cumulative Proportion", x = "Relative Brier Score")

cdf_combined <- diff_cdf + rel_score_cdf &
  plot_annotation(tag_levels = "A")
ggsave("forecasters-contributions/plots/cdf-contributions-combined.jpg", 
       plot = cdf_combined,
       width = 8, height = 3)




# ======================== Table with numeric results ======================== #
table <- list()
# mean contribution is positive
(table[[1]] <- scores |>
  summary_function(var = "diff_Brier") |>
  mutate(var = "Absolute difference in Brier scores"))

# mean user makes a negative contribution
(table[[2]] <- scores |>
    inner_join(forecasts_by_user_and_question) |>
    average_by_user() |>
    summary_function(var = "diff_Brier") |>
    mutate(var = "Avg. abs. diff. per user"))

(table[[3]] <- scores |>
    average_by_question() |>
    summary_function(var = "diff_Brier") |>
    mutate(var = "Avg. abs. diff. per question"))

(table[[4]] <- scores |>
    summary_function(var = "rel_Brier") |>
    mutate(var = "Relative Brier scores"))

(table[[5]] <- scores |>
    inner_join(forecasts_by_user_and_question) |>
    average_by_user() |>
    summary_function(var = "rel_Brier") |>
    mutate(var = "Avg. rel. score per user"))

(table[[6]] <- scores |>
    remove_clear_questions() |>
    inner_join(forecasts_by_user_and_question) |>
    average_by_question() |>
    summary_function(var = "rel_Brier") |>
    mutate(var = "Avg. rel. score per question"))

table <- rbindlist(table) |>
  select(Description = var, min, mean, median, max, sd) 


# ================= Contributions and numbers of forecasters ================= #

## you would expect differences to be smaller for questions with 
## lots of forecasters and of course you have lots of forecasers there
(contrib_n_forecaster <- scores |>
    ggplot(aes(x = n_user, y = diff_Brier)) + 
    geom_point(size = 0.1, alpha = 0.5) + 
    stat_density_2d(geom = "polygon", alpha = 0.5,
                    aes(fill = after_stat(level)),
                    bins = 30) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    scale_y_continuous(limits = c(-0.1, 0.1), labels = function(x) {as.character(x)}) +
    theme_scoringutils() +
    theme(legend.position="none") + 
    labs(y = "Contribution (diff. in Brier scores)", x = "Number of forecasters"))

contrib_n_forecaster_log <- contrib_n_forecaster + 
  scale_x_log10()

(rel_contrib_n_forecaster <- scores |>
    ggplot(aes(x = n_user, y = rel_Brier)) + 
    geom_point(size = 0.1, alpha = 0.5) + 
    stat_density_2d(geom = "polygon", alpha = 0.5,
                    aes(fill = after_stat(level)),
                    bins = 30) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    scale_y_continuous(limits = c(0.8, 1.2), labels = function(x) {as.character(x)}) +
    theme_scoringutils() + 
    theme(legend.position = "none") +
    labs(y = "Relative Brier score", x = "Number of forecasters"))

rel_contrib_n_forecaster_log <- rel_contrib_n_forecaster + 
  scale_x_log10()

contrib_n_forecasters_combined <- 
  contrib_n_forecaster + rel_contrib_n_forecaster + 
  contrib_n_forecaster_log + rel_contrib_n_forecaster_log & 
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "none")

ggsave("forecasters-contributions/plots/contribusions-number-forecasters.jpg", 
       plot = contrib_n_forecasters_combined,
       width = 7, height = 6)



# ============================================================================ #
# Analysis - by user 
# ============================================================================ #

# ============================ Overview plot ================================= #

## Overview plot of average contributions by user
scores |>
  inner_join(forecasts_by_user_and_question) |>
  average_by_user() |>
  ggplot(aes(x = diff_Brier)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100, 
                 colour = "white", linewidth = 0.01) + 
  theme_scoringutils() + 
  scale_y_continuous(label = \(x) {paste0(100 * x, "%")}) +
  scale_x_continuous(limits = c(-0.005, 0.005), labels = function(x) {as.character(x)}) + 
  labs(y = "Proportion", x = "Average (per User) Difference in Brier score")
ggsave("forecasters-contributions/plots/avg-contribution-by-user.jpg", 
       width = 7, height = 3)


# ====================== relation to performance ============================== #
## Average contribution vs. average Brier score   
scores |>
  inner_join(cumulative_brier_diff) |>
  group_by(user_id) |>
  filter(resolve_time == max(resolve_time)) |>
  ggplot(aes(y = cum_avg_diff_Brier, x = cum_brier_user)) + 
  geom_point(size = 0.1, alpha = 0.7) +
  stat_density_2d(geom = "polygon", alpha = 0.5,
                  aes(fill = after_stat(level)),
                  bins = 30) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_scoringutils() +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(-0.025, 0.025)) + 
  labs(y = "Avg. contribution (diff. in Brier scores)", x = "Average user Brier score")
ggsave("forecasters-contributions/plots/avg-contribution-avg-brier-score.jpg", 
       width = 7, height = 3)


## individual contribution vs. performance on that question
(single_contributions_score <- scores |>
  inner_join(cumulative_brier_diff) |>
  group_by(user_id) |>
    ggplot(aes(y = diff_Brier, x = brier_user)) + 
    geom_point(size = 0.05, alpha = 0.5) + 
    stat_density_2d(geom = "polygon", alpha = 0.5,
                    aes(fill = after_stat(level)),
                    bins = 30) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_scoringutils() +
  scale_y_continuous(limits = c(-0.025, 0.025)) + 
  labs(y = "User contribution (diff. in score)", x = "Single user Brier score"))

(single_rel_contributions_score <- scores |>
    inner_join(cumulative_brier_diff) |>
    group_by(user_id) |>
    ggplot(aes(y = rel_Brier, x = brier_user)) + 
    geom_point(size = 0.05, alpha = 0.5) + 
    stat_density_2d(geom = "polygon", alpha = 0.5,
                    aes(fill = after_stat(level)),
                    bins = 30) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    theme_scoringutils() +
    scale_y_continuous(limits = c(0.9, 1.1)) + 
    labs(y = "Relative Brier score", x = "Single user Brier score"))

single_contributions_score + single_rel_contributions_score & 
  theme(legend.position = "none")
ggsave("forecasters-contributions/plots/individual-contribution-brier-score.jpg", 
       width = 8, height = 3)




# ================== Relationship number forecasts by user =================== #

## Average contributions (diff Brier score) vs. average number of questions forecasted on
(avg_contribution_num_q <- scores |>
  inner_join(cumulative_brier_diff) |>
  inner_join(num_questions_forecasted_by_user) |>
  group_by(user_id) |>
  filter(resolve_time == max(resolve_time)) |>
  ggplot(aes(y = cum_num_questions_forecasted, x = cum_avg_diff_Brier)) + 
    geom_point(size = 0.1, alpha = 0.5) + 
    stat_density_2d(geom = "polygon", alpha = 0.7,
                    aes(fill = after_stat(level)),
                    bins = 30) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") + 
  theme_scoringutils() +
  scale_x_continuous(limits = c(-0.01, 0.01)) +
  coord_flip() + 
  labs(y = "Number of questions forecasted on", x = "Avg. contribution (diff. in Brier score)") + 
  scale_y_log10())

## Average Brier score vs. average number of questions forecasted on
(avg_brier_num_q <- scores |>
  inner_join(cumulative_brier_diff) |>
  inner_join(num_questions_forecasted_by_user) |>
  group_by(user_id) |>
  filter(resolve_time == max(resolve_time)) |>
  ggplot(aes(y = cum_num_questions_forecasted, x = cum_brier_user)) + 
    geom_point(size = 0.1, alpha = 0.5) + 
    stat_density_2d(geom = "polygon", alpha = 0.7,
                    aes(fill = after_stat(level)),
                    bins = 30) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_scoringutils() +
  coord_flip() + 
  labs(y = "Number of questions forecasted on", x = "Average user Brier score") + 
  scale_y_log10())

# cumulative contributions (diff in Brier score) as a function of the number of forecasts made
(cum_avg_contribution_num_q <- scores |>
    inner_join(num_questions_forecasted_by_user) |>
    inner_join(cumulative_brier_diff) |>
    mutate(resolve_time = lubridate::as_date(resolve_time / 1e5)) |> 
    select(user_id, cum_num_questions_forecasted, cum_avg_diff_Brier, cum_brier_user) |>
    ggplot(aes(y = cum_avg_diff_Brier, x = cum_num_questions_forecasted, group = user_id)) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") + 
    geom_line(linewidth = 0.1, alpha = 0.7) +
    # scale_y_continuous(limits = c(-0.01, 0.01)) +
    coord_cartesian(ylim = c(-0.01, 0.01)) + 
    theme_scoringutils() + 
    labs(y = "Cum. avg. contribution", x = "Cum. number questions forecasted on")) 

## cumulative mean Brier score as a function of the number of forecasts made
(cum_avg_brier_num_q <- scores |>
  inner_join(num_questions_forecasted_by_user) |>
  inner_join(cumulative_brier_diff) |>
  mutate(resolve_time = lubridate::as_date(resolve_time / 1e5)) |> 
  select(user_id, cum_num_questions_forecasted, cum_avg_diff_Brier, cum_brier_user) |>
  ggplot(aes(y = cum_brier_user, x = cum_num_questions_forecasted, group = user_id)) + 
  # geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") + 
  geom_line(linewidth = 0.1, alpha = 0.7) +
  theme_scoringutils() + 
  labs(y = "Cum. avg. user Brier score", x = "Cum. number questions forecasted on")) 

(avg_contribution_num_q + avg_brier_num_q) /
(cum_avg_contribution_num_q + cum_avg_brier_num_q) & 
  theme(legend.position = "none")
ggsave("forecasters-contributions/plots/avg-brier-and-contribution-num-questions.jpg", 
       width = 8, height = 6)







# ========================= Looking at power user ============================ #
## filter for users who have forecasted on 100 questions. 
power_users <- scores |>
  inner_join(num_questions_forecasted_by_user) |>
  filter(cum_num_questions_forecasted >= 100) |> 
  pull(user_id) |>
  unique()

scores |>
  filter(user_id %in% power_users) |>
  inner_join(num_questions_forecasted_by_user) |>
  inner_join(cumulative_brier_diff) |>
  mutate(resolve_time = lubridate::as_date(resolve_time / 1e5)) |> 
  select(user_id, cum_num_questions_forecasted, cum_diff_Brier, cum_avg_diff_Brier, cum_brier_user) |>
  ggplot(aes(y = cum_diff_Brier, x = cum_num_questions_forecasted, group = user_id)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") + 
  geom_line(linewidth = 0.1, alpha = 0.7) +
  # scale_y_continuous(limits = c(-0.01, 0.01)) +
  # coord_cartesian(ylim = c(-0.01, 0.01)) +
  theme_scoringutils() + 
  labs(y = "Cum. avg. contribution", x = "Cum. number questions forecasted on")


scores |>
  filter(user_id %in% power_users) |>
  inner_join(forecasts_by_user_and_question) |>
  average_by_user() |>
  summary_function(var = "diff_Brier")

scores |>
  filter(user_id %in% power_users) |>
  inner_join(forecasts_by_user_and_question) |>
  average_by_user() |>
  mutate(cum_diff_Brier = diff_Brier * n_questions) |>
  summary_function(var = "cum_diff_Brier")
  
  
  
  
  



  




# =============== Contributions - difference prediction / CP ================= #

## check distance to CP and contribution
contribution_distance_cp <- scores |> 
  inner_join(diff_prediction_to_cp_mean) |>
  ggplot(aes(y = diff_Brier, x = diff_to_cp)) + 
  geom_point(size = 0.1, alpha = 0.5) + 
  stat_density_2d(geom = "polygon", alpha = 0.7,
                  aes(fill = after_stat(level)),
                  bins = 30) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  scale_y_continuous(limits = c(-0.1, 0.1)) +
  # coord_cartesian(ylim = c(-0.1, 0.1)) +
  theme_scoringutils() + 
  labs(y = "Contribution (diff. in Brier score)", x = "Difference prediction  - CP the user saw")
  
## check distance to CP and contribution
rel_contribution_distance_cp <- scores |> 
  inner_join(diff_prediction_to_cp_mean) |>
  ggplot(aes(y = rel_Brier, x = diff_to_cp)) + 
  geom_point(size = 0.1, alpha = 0.5) + 
  stat_density_2d(geom = "polygon", alpha = 0.7,
                  aes(fill = after_stat(level)),
                  bins = 30) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  scale_y_continuous(limits = c(0, 2)) +
  theme_scoringutils() + 
  labs(y = "Relative Brier score", x = "Difference prediction - CP the user saw")

contribution_distance_cp + rel_contribution_distance_cp & 
  theme(legend.position = "none")
ggsave("forecasters-contributions/plots/diff-prediction-cp-contribution.jpg",
       width = 7, height = 3)



## Contribution vs. whether or not forecasts are more or less confident
(diff_contribution_confidence <- scores |> 
  inner_join(diff_prediction_to_cp_mean) |>
  ggplot(aes(y = diff_Brier, x = confidence)) + 
    geom_point(size = 0.1, alpha = 1) + 
    stat_density_2d(geom = "polygon", alpha = 0.7,
                    aes(fill = after_stat(level)),
                    bins = 30) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    scale_y_continuous(limits = c(-0.1, 0.1)) +
    # coord_cartesian(ylim = c(-0.1, 0.1)) +
  theme_scoringutils() + 
  labs(y = "Contribution (diff. in Brier score)", x = "Confidence user prediction - CP"))

rel_contribution_confidence <- scores |> 
  inner_join(diff_prediction_to_cp_mean) |>
  ggplot(aes(y = rel_Brier, x = confidence)) + 
  geom_point(size = 0.1, alpha = 1) + 
  stat_density_2d(geom = "polygon", alpha = 0.7,
                  aes(fill = after_stat(level)),
                  bins = 30) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  scale_y_continuous(limits = c(0, 2)) +
  # coord_cartesian(ylim = c(0, 2)) +
  theme_scoringutils() + 
  labs(y = "Relative Brier score", x = "Confidence user prediction - CP")

diff_contribution_confidence + rel_contribution_confidence & 
  theme(legend.position = "none")
ggsave("forecasters-contributions/plots/contribution-confidence.jpg",
       width = 7, height = 3)


## Brier scores vs. whether or not predictions are more or less confident
combined |> 
  inner_join(diff_prediction_to_cp) |>
  filter(!is.na(confidence)) |>
  mutate(brier = (prediction - resolution)^2) |>
  ggplot(aes(y = brier, x = confidence)) + 
  geom_point(size = 0.1, alpha = 1) + 
  stat_density_2d(geom = "polygon", alpha = 0.7,
                  aes(fill = after_stat(level)),
                  bins = 30) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_scoringutils() + 
  labs(y = "Single forecast Brier score", x = "Confidence prediction vs. CP user saw") + 
  theme(legend.position = "none")
ggsave("forecasters-contributions/plots/brier-scores-confidence.jpg",
       width = 7, height = 3)


## check diff to CP as a function of the number of forecasts
(diff_cp_num_pred <- scores |> 
    inner_join(diff_prediction_to_cp_mean) |>
    inner_join(num_questions_forecasted_by_user) |>
    ggplot(aes(y = abs(diff_to_cp), x = cum_num_questions_forecasted, group = user_id)) + 
    # stat_summary_bin(fun= "mean", geom = "line", bins = 10) + 
    geom_line(size = 0.1, alpha = 0.7) +
    theme_scoringutils() + 
    labs(y = "Absolute difference prediction and CP", x = "Cum. number questions forecasted on"))

diff_num_pred_log <- diff_cp_num_pred + 
  scale_x_log10()

diff_cp_num_pred + diff_num_pred_log
ggsave("forecasters-contributions/plots/diff-prediction-cp-num-questions.jpg",
       width = 8, height = 3)



# ================== Contributions and frequent updating ===================== #

scores |>
  inner_join(forecasts_by_user_and_question) |>
  ggplot(aes(y = diff_Brier, x = rel_pred_by_user)) + 
  geom_point(size = 0.1) + 
  scale_x_log10() + 
  scale_y_continuous(limits = c(-0.1, 0.1)) + 
  theme_scoringutils() + 
  labs(y = "Difference in Brier score", x = "Update frequency relative to average update frequency")
ggsave("forecasters-contributions/plots/contribution-update-frequency.jpg",
       width = 7, height = 3)


scores |>
  inner_join(forecasts_by_user_and_question) |>
  inner_join(diff_prediction_to_cp_mean) |>
  ggplot(aes(y = diff_to_cp, x = rel_pred_by_user)) + 
  geom_point(size = 0.1) + 
  scale_x_log10() + 
  # scale_y_continuous(limits = c(-0.1, 0.1)) + 
  theme_scoringutils() + 
  labs(y = "Difference prediction - CP", x = "Update frequency relative to average update frequency")
ggsave("forecasters-contributions/plots/diff-cp-update-frequency.jpg",
       width = 7, height = 3)



## check whether the positive contribution is related to how often 
## someone updates, maye controlled by the average amount of 
## updating for a question, because you'd expect questions that require lots of
## updates to be harder
scores |>
  ggplot(aes(y = diff_Brier, x = n_pred_by_user)) + 
  geom_point(size = 0.1) + 
  scale_x_log10() + 
  scale_y_continuous(limits = c(-0.1, 0.1)) + 
  theme_scoringutils()
ggsave("forecasters-contributions/plots/diff-cp-update-frequency.jpg",
       width = 7, height = 3)



















# ============================================================================ #
## Code Parking lot and unused analyses


## relation to number of questions forecasted on
# plot_contrib_num_questions_forecasted <- function(scores) {
#   scores |> 
#     ggplot(aes_string(x = "n_questions", y = "diff_Brier")) + 
#     geom_point(size = 0.1) + 
#     scale_y_continuous(limits = c(-0.005, 0.005), labels = function(x) {as.character(x)}) +
#     theme_scoringutils() + 
#     geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", size = 0.5)
# }

# scores |>
#   inner_join(forecasts_by_user) |>
#   average_by_user() |>
#   plot_contrib_num_questions_forecasted() + 
#   scale_x_log10() +
#   labs(y = "Avg. abs. difference in Brier score", x = "Number of questions forecasted on")
# ggsave("forecasters-contributions/plots/contribution-vs-number-of-questions-forecasted.jpg", 
#        width = 7, height = 3)


## How much do people update as a function of the number of questions they 
# already forecasted on? 

# (updating_num_questions <- scores |> 
#     inner_join(forecasts_by_user_and_question) |>
#     inner_join(num_questions_forecasted_by_user) |>
#     ggplot(aes(y = rel_pred_by_user, x = cum_num_questions_forecasted)) + 
#     # stat_summary_bin(fun= "mean", geom = "line", bins = 10) + 
#     geom_line(size = 0.1, alpha = 0.7) +
#     theme_scoringutils() + 
#     labs(y = "Updating relative to others", x = "Cumulative number of questions forecasted on"))
# ggsave("forecasters-contributions/plots/diff-prediction-cp-num-questions.jpg",
#        width = 7, height = 3)



## contributions of users as a function of how many questions they 
# forecasted on. It seems like users tend to make less drastic cnotributions 
# as they forecast more
(contribution_num_q_forecasted <- scores |>
    inner_join(num_questions_forecasted_by_user) |>
    mutate(resolve_time = lubridate::as_date(resolve_time / 1e5)) |>
    ggplot(aes(y = diff_Brier, x = cum_num_questions_forecasted, group = user_id)) +
    geom_line(size = 0.1, alpha = 0.7) +
    coord_cartesian(ylim = c(-0.1, 0.1)) +
    theme_scoringutils() +
    labs(y = "Differece in Brier scores",
         x = "Cumulative number of questions forecasted on by a given user"))
ggsave("forecasters-contributions/plots/contribution-vs-number-of-questions-forecasted.jpg",
       width = 7, height = 3)

contribution_num_q_forecasted + 
  scale_x_log10()


# it would be really cool to get some kind of connection to the first analysis
# i.e. for different numbers of forecasts, what's the average contribution? 
# what do average contributions look like for different numbers of forecasters?
# not sure this works as is. 
# scores |>
#   average_by_question() |>
#   ggplot(aes(x = n_user, y = rel_Brier)) + 
#   stat_summary_bin(fun= "mean", geom = "line", bins = 10) + 
#   geom_hline(yintercept = 1, colour = "grey60", linetype = "dashed") + 
#   scale_x_continuous(trans = "log10") + 
#   scale_y_continuous(limits = c(0.99, 1.01), labels = function(x) {as.character(x)}) +
#   theme_scoringutils()







