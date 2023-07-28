library(data.table)
library(dplyr)
library(scoringutils)
library(arrow)
library(jsonlite)
library(ggplot2)
library(patchwork)
library(tidyr)
library(RColorBrewer)
library(zoo)
library(purrr)

# - implement scoring rules for binary and continuous
    # - talk to Lawrence to get the CP grid to 201
    # - do time averaging correctly
# - find a way to identify the names of the top forecasters
# - look at discrepancies in relative scores for binary and continuous



# ============================================================================ #
#                                 Load data                                    #
# ============================================================================ #

binary_pred_raw <- fromJSON("data/predictions_binary.json") |>
  as.data.table() 

binary_questions_raw <- fromJSON("data/questions_binary.json") |>
  as.data.table() |>
  filter(resolution %in% c(0, 1)) |>
  select(publish_time, 
         question_id,
         resolution,
         close_time, 
         resolve_time)

continuous_questions_raw <- fromJSON("data/questions_continuous.json")

continuous_predictions_raw <- read_parquet("data/predictions_continuous.parquet")

combine_data <- function(predictions, questions) {
  combined <- inner_join(
    predictions, questions
  ) |>
    filter(!is.na(resolution)) |>
    mutate(close_time = pmin(close_time, resolve_time)) |>
    filter(t <= close_time) 
  return(combined)
}

combined_binary <- combine_data(binary_pred_raw, binary_questions_raw)
combined_continuous <- combine_data(continuous_predictions_raw, continuous_questions_raw) |>
  mutate(open_time = publish_time)

names <- fromJSON(txt = "data/shared_anonymization_matrix.json")
names <- data.table(
  original = unlist(names[[1]]),
  anonymised = unlist(names[[2]])
)

users <- data.table::fread("data/metac_users.csv")
users <- names |>
  rename(id = original) |>
  inner_join(users, by = "id")

# ============================================================================ #
# helper functions
# ============================================================================ #

# Overview of scores that exist:
# non tournament scores: 
#   - Brier score
#   - Log score
#   - Metaculus points
#   
# old tournament scores: 
#   binary: 
#     relative log score (same as new tournament score)
#   continuous: 
#     relative (local) log score  
# 
# new tournament scores: 
#   binary: 
#     relative log score
#     absolute log score
#   continuous: 
#     relative (global) log score

# scoring for binary questions
# ------------------------------------------------------------------------------
# simple binary log score with no baseline
score_log_binary <- function(resolution, pred) {
  # this is log(pred) if resolution == 1, and log(1-pred) otherwise
  log(1 - abs(resolution - pred), base = 2)
}

# relative log score, usually compared to the CP (old and new tournament score)
score_rel_log_binary <- function(resolution, pred, pred_compare) {
  # compare log score against score of community prediction
  # if you're better than the CP than the score is positive, otherwise negative
  score_log_binary(resolution, pred) - score_log_binary(resolution, pred_compare)
}

# binary absolute log score (newly introduced, log score vs. naive baseline)
score_abs_log_binary <- function(resolution, pred) {
  # compare log score against a naive prior
  score_rel_log_binary(resolution, pred, 0.5)
}

# scoring for continuous questions
# ------------------------------------------------------------------------------

# missing: relative log score (forecast comparied against cp)

# relative log score (forecast compared against cdf of CP) and 
# absolute log score (forecast compared against naive prior) for continuous.
# score is normalised by score of a hypothetical best case foreast
score_log_continuous <- function(pred, 
                                 compare_against = "prior", 
                                 resolution, x_grid, 
                                 lower_bound_type, 
                                 upper_bound_type) {
  # pred is the cdf
  # compare against is either "prior" (absolute score) or the cdf of the CP
  # resolution is the actual observed x value in the real world
  # x_grid is the grid of values that the CDF maps to
  
  n_grid_points <- length(x_grid)
  # get resolution per grid point
  resolution_cdf <- (resolution <= x_grid) 
  
  if (is.character(compare_against) && compare_against[1] == "prior") {
    compare_against <- get_naive_cont_prior(
      n_grid_points, 
      lower_bound_type, 
      upper_bound_type
    )
  }
  
  # not entirely sure about the zero here. The problem is that the CDF, at least
  # for discrete values, could have probability mass at 0. Though I think that our
  # prior just assigns a flat 0 to that. Essentially this means that there is 
  # another bin from -Inf (exclusive) to the first x_grid point (inclusive)
  # that we assign 0 probability to 
  prob_mass_bin <- diff(c(0, compare_against))
  
  raw_score <- score_rel_log_binary(
    resolution = resolution_cdf, 
    pred = pred, 
    pred_compare = compare_against
  )
  
  normalised_score <- raw_score / get_weighting_factor(n_grid_points, lower_bound_type, upper_bound_type)
  weighted_avg <- sum(normalised_score * prob_mass_bin)
  
  return(weighted_avg)
}


# weighting factor of hypothetical best case forecast to weigh the continuous score with. 
get_weighting_factor <- function(n_grid_points = 201, lower_bound_type, upper_bound_type) {
  # The weighting factor is obtained by comparing a hypothetical perfect 
  # forecast (if forecast as well as resolution were ideal) against a naive prior
  prior <- get_naive_cont_prior(n_grid_points, lower_bound_type, upper_bound_type)
  
  # if the lower bound is closed, then the 
  # normalisation factor is determined from a hypothetical forecast that resolves
  # in the first bin, i.e. between the first and the second grid point
  # resolution for first grid point in CDF space is 0 and then 1 as the resolution
  # value must be higher than the first grid point, but lower than the second
  # if the lower bound is open, then the
  # hypothetical forecast assigns 100 percent to "resolves out of bounds" and is right. 
  
  if (lower_bound_type == "closed") {
    n_bins = n_grid_points - 1
    perfect_resolution <- c(0, rep(1, n_bins))
  } else {
    perfect_resolution <- 1
  }
  # the perfect log score is always 0, but the prior log score is different. 
  
  # scores are computed for bins, we can imagine every bin as a separate 
  # binary question and the resolution is always one
  # perfect score is 0
  perfect_rel_score <- 
    0 - score_log_binary(resolution = perfect_resolution, prior[1:n_grid_points])
  
  # compute weighted average as 
  # product of score * probability mass per bin assigned by naive prior
  prob_mass_bin <- diff(c(0, prior))
  weighted_avg <- (perfect_rel_score * prob_mass_bin) |>
    sum()
  
  return(weighted_avg)
}

# get naive continuous prior based on number of grid points
get_naive_cont_prior <- function(n_grid_points, lower_bound_type, upper_bound_type) {
  # the prior changes depending on whether we have an open or a closed bound
  # for a closed lower bound, the probabilty assigned to the lowest grid point 
  # is 0. In terms of bins, we can think of a bin from (-Inf, first_grid_point] 
  # that gets assigned probability 0. Not sure whether that's accurate if we 
  # think about discrete values, where it's actually possible that the resolution
  # can be equal to the lower bound. 
  # if the lower bound is open, then the probability for the first value is 
  # 0.05
  if (lower_bound_type == "closed") {
    lower <- 0
  } else if (lower_bound_type == "open") {
    lower <- 0.05
  }
  # if the upper bound is closed, then upper is 1. Else, upper is 0.95 and 5%
  # probability is out of bounds
  if (upper_bound_type == "closed") {
    upper <- 1
  } else if (upper_bound_type == "open") {
    upper <- 0.95
  }
  naive_prior <- seq(lower, upper, length.out = n_grid_points)
  return(naive_prior)
}



# time averaged scores
# ------------------------------------------------------------------------------

# time averaged version of the binary rel log score
# this scores all forecasts made by a single user for a single question
time_rel_log_score_binary <- function(
    pred_user, 
    t_user, 
    pred_cp, 
    t_cp, 
    resolution, 
    open_time, 
    close_time
) {
  joined_df <- create_combined_prediction_df(
    pred_user, t_user, pred_cp, t_cp
  )
  
  scores <- score_rel_log_binary(resolution, joined_df$prediction, joined_df$cp)
  
  score <- apply_time_weighting(t = joined_df$t, scores = scores, 
                                open_time = open_time, close_time = close_time)
  
  return(score)
}

# time averaged version of the abs binary log score
# this scores all forecasts made by a single user for a single question
time_abs_log_score_binary <- function(pred_user, t_user, resolution, open_time, close_time) {
  scores <- score_abs_log_binary(resolution, pred_user)
  
  score <- apply_time_weighting(t = t_user, scores = scores, 
                                open_time = open_time, close_time = close_time)
}

# helper function that creates a data.frame that has a user prediction for 
# every time point that there also is a cp for. The data.frame starts with the
# first time a user makes a prediction (previous cp values are discarded)
create_combined_prediction_df <- function(
    pred_user, 
    t_user, 
    pred_cp, 
    t_cp
) {
  a <- tibble(
    prediction = pred_user, 
    t = t_user
  )
  b <- tibble(
    cp = pred_cp, 
    t = t_cp
  )
  
  full_join(a, b, by = "t") |>
    arrange(t) %>%
    # as this can be a list I need to convert to character first and then convert back... 
    mutate(prediction = map_chr(prediction, ~ifelse(is.null(.x), NA, paste(.x, collapse = ",")))) |>
    mutate(prediction = zoo::na.locf(prediction, na.rm = FALSE)) %>%
    mutate(prediction = strsplit(prediction, ",") %>% map(~as.numeric(.x))) |>
    filter(!is.na(prediction))
}


# time averaged version of the abs continuous log score
# this scores all forecasts made by a single user for a single question
time_abs_log_score_continuous <- function(pred_user, t_user, x_grid, 
                                          resolution, open_time, close_time, 
                                          lower_bound_type, upper_bound_type) {
  # pred_user is a list of predictions of the same length as t
  # each entry is a predictive cdf that corresponds to a single forecast
  
  scores <- sapply(pred_user, function(x) {
    score_log_continuous(pred = x, compare_against = "prior", 
                         resolution = resolution, x_grid = x_grid, 
                         lower_bound_type = "closed", upper_bound_type = "closed")
  })
  
  
  score <- apply_time_weighting(t = t_user, scores = scores,
                                open_time = open_time, close_time = close_time)
  
  return(score)
}


time_rel_log_score_continuous <- function(pred_user, t_user, pred_cp, t_cp, x_grid,
                                          resolution, open_time, close_time,
                                          lower_bound_type, upper_bound_type) {
  # pred_user is a list of predictions of the same length as t
  # each entry is a predictive cdf that corresponds to a single forecast
  
  joined_df <- create_combined_prediction_df(
    pred_user, t_user, pred_cp, t_cp
  )
  
  scores <- purrr::map2(.x = joined_df$prediction, .y = joined_df$cp, .f = function(.x, .y) {
    score_log_continuous(pred = .x, compare_against = .y,
                         resolution = resolution, x_grid = x_grid,
                         lower_bound_type = "closed", upper_bound_type = "closed")
  }) |> unlist()

  score <- apply_time_weighting(t = joined_df$t, scores = scores,
                                open_time = open_time, close_time = close_time)
  
  return(score)
}



# helper function to weigh an existing set of scores by time
# time is the time of every prediction made. 
# if you haven't predicted anything, you get assigned a score of 0
# for absolute this is equivalent to predicting the prior, for relative it's 
# equivalent to predicting the CP
apply_time_weighting <- function(t, scores, open_time = NULL, close_time) {
  
  t_min <- min(t)
  t_max <- close_time
  t_total = t_max - t_min
  
  durations = diff(c(as.numeric(t), as.numeric(t_max)))
  
  scores <- ((scores * durations) / t_total)
  score <- sum(scores)
  
  if (!is.null(open_time)) {
    # assign a score of 0 for all the time that you haven't predicted
    score <- 0 + score * t_total / (close_time - open_time)
  }
  return(score)
}


# helper function to apply the time weighted scores across a data.frame
# takes the df for one question as input and iterates over all users to get the
# user scores for that question
get_scores_for_question_binary <- function(data) {
  pred_cp <- data$cp
  t_cp <- data$t
  
  data <- as.data.table(data)
  data[, time_rel_logs_binary := time_rel_log_score_binary(
    prediction,
    t, 
    pred_cp, 
    t_cp, 
    unique(resolution),
    unique(publish_time), 
    unique(close_time)
  ), by = "user_id"]
  
  data[, time_abs_logs_binary := time_abs_log_score_binary(
    prediction,
    t, 
    unique(resolution),
    unique(publish_time), 
    unique(close_time)
  ), by = "user_id"][]
  
  return(
    list(data$time_rel_logs_binary, data$time_abs_logs_binary)
  )
}

# helper function to apply the time weighted scores across a data.frame
# takes the df for one question as input and iterates over all users to get the
# user scores for that question
get_scores_for_question_continuous <- function(data) {
  # need to change: 
  pred_cp <- data$community_cdf
  # pred_cp <- data$cdf
  t_cp <- data$t
  
  data <- as.data.table(data)
  data[, time_rel_logs_continuous := time_rel_log_score_continuous(
    pred_user = cdf, 
    t_user = t, 
    pred_cp = pred_cp, 
    t_cp = t_cp, 
    x_grid = unlist(unique(x_grid)), 
    resolution = unique(resolution), 
    open_time = unique(open_time), 
    close_time = unique(close_time), 
    lower_bound_type = unique(lower_bound_type), 
    upper_bound_type = unique(upper_bound_type)
  ), by = "user_id"]
  
  data[, time_abs_logs_continuous := time_abs_log_score_continuous(
    pred_user = cdf, 
    t_user = t, 
    x_grid = unlist(unique(x_grid)), 
    resolution = unique(resolution), 
    open_time = unique(open_time), 
    close_time = unique(close_time), 
    lower_bound_type = unique(lower_bound_type), 
    upper_bound_type = unique(upper_bound_type)
  ), by = "user_id"]
  
  
  return(
    list(data$time_rel_logs_continuous, data$time_abs_logs_continuous)
  )
}




# ============================================================================ #
# Computations
# ============================================================================ #

# need to do: 
# - compute all scores
# - compute all scores in a ad hoc fashion (i.e. this time point) vs. a time-averaged fashion. 
# 
# - maybe just do a exploratory analysis of all the scores. 
#   - what does the cloud of scores look like
#   - consistency of users over time? 
#   - maybe something like agreement Brier and log score? 
# - can already do log vs. relative log for binary questions

# - talk to Lawrence to get the CP grid to 201
# - do time averaging correctly
# - find a way to identify the names of the top forecasters
# - look at discrepancies in relative scores for binary and continuous


# compute scores
# ------------------------------------------------------------------------------
recompute_scores <- FALSE

if (recompute_scores) {
  binary_scores <- combined_binary |> 
    mutate(
      logs_ab_cp_point = score_abs_log_binary(resolution, cp), 
      logs_abs_user_point = score_abs_log_binary(resolution, prediction), 
      logs_rel_user_point = score_rel_log_binary(resolution, prediction, cp)
    )
  setDT(binary_scores)
  
  # calculate relative scores
  binary_scores[, c("time_rel_logs_binary", "time_abs_logs_binary") :=
                  get_scores_for_question_binary(.SD), 
                by = "question_id"]
  fwrite(binary_scores, "scoring-rules/binary-scores.csv")
} else {
  binary_scores <- fread("scoring-rules/binary-scores.csv")
}

if (recompute_scores) {
  continuous_scores <- combined_continuous |> 
    rowwise() |>
    mutate(
      logs_abs_cp_point = score_log_continuous(
          pred = community_cdf, x_grid = x_grid, 
          resolution = resolution, 
          lower_bound_type = lower_bound_type, upper_bound_type = upper_bound_type),
      logs_abs_user_point = score_log_continuous(
        pred = cdf, x_grid = x_grid, 
        resolution = resolution, 
        lower_bound_type = lower_bound_type, upper_bound_type = upper_bound_type
      ), 
      logs_rel_user_point = score_log_continuous(
        pred = cdf, compare_against = community_cdf,
        x_grid = x_grid, 
        resolution = resolution, 
        lower_bound_type = lower_bound_type, upper_bound_type = upper_bound_type
      )
    )
  setDT(continuous_scores)
  
  # calculate relative scores
  continuous_scores[, c("time_rel_logs_continuous", "time_abs_logs_continuous") :=
                  get_scores_for_question_continuous(.SD), 
                by = "question_id"]
  
  continuous_scores |>
    select(user_id, question_id, t, logs_abs_cp_point, logs_abs_user_point, logs_rel_user_point, 
           time_rel_logs_continuous, time_abs_logs_continuous) |>
  fwrite("scoring-rules/continuous-scores.csv")
} else {
  continuous_scores <- fread("scoring-rules/continuous-scores.csv")
}



binary_scores <- binary_scores |>
  select(user_id, question_id, t, resolution, logs_ab_cp_point, 
         logs_abs_user_point, logs_rel_user_point, 
         time_abs_logs_binary, time_rel_logs_binary)




# ============================================================================ #
# Analyses
# ============================================================================ #

# Distribution of scores
# ------------------------------------------------------------------------------

label_names <- c(
  time_abs_logs_binary = "Absolute time avg. binary log score", 
  time_abs_logs_continuous = "Absolute time avg. cont. log score", 
  time_rel_logs_binary = "Relative time avg. binary log score", 
  time_rel_logs_continuous = "Relative time avg. cont. log score", 
  logs_abs_user_point = "Absolute (snapshot) log score", 
  logs_rel_user_point = "Relative (snapshot) log score"
)
# distribution of binary scores - time weighted
p1 <- binary_scores |>
  select(user_id, question_id, time_abs_logs_binary, time_rel_logs_binary) |>
  unique() |>
  pivot_longer(cols = c(time_abs_logs_binary, time_rel_logs_binary), 
               names_to = "score") |>
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100, 
                 colour = "white", linewidth = 0.01) + 
  facet_wrap(~score, ncol = 1, labeller = as_labeller(label_names)) + 
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15)) + 
  expand_limits(y = c(0, 0.18)) +  
  theme_scoringutils() + 
  lims(x = c(-3, 3)) + 
  labs(y = "Proportion", x = "Binary log scores\n(time-weighted)")

# interesting: People who predict near 50% tend to be right more often than wrong
# also for the absolute ones, scores get pulled towards zero by time weighting
# for the relative ones: people predict the CP quite a lot or maybe more plausible: 
# scores get pulled towards zero due to the time weighting. 

# distribution of binary scores - one point in time
p2 <- binary_scores |>
  select(user_id, question_id, logs_abs_user_point, logs_rel_user_point) |>
  unique() |>
  pivot_longer(cols = c(logs_abs_user_point, logs_rel_user_point), 
               names_to = "score") |>
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100, 
                 colour = "white", linewidth = 0.01) + 
  facet_wrap(~score, ncol = 1, labeller = as_labeller(label_names)) + 
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15)) + 
  expand_limits(y = c(0, 0.18)) + 
  theme_scoringutils() + 
  theme(axis.title.y = element_blank()) + 
  lims(x = c(-7, 3)) + # some people also predict 0.99 and are wrong + 
  labs(y = "Proportion", x = "Binary log scores\n(one point in time)")

# it seems like all of these is consistent with time weighting - relative log 
# scores look more or less symmetric without time weighting. 
# for abs: lots of people apparently predict 0.99 and are right

# distribution of continuous scores - time weighted
p3 <- continuous_scores |>
  select(user_id, question_id, time_abs_logs_continuous, time_rel_logs_continuous) |>
  unique() |>
  pivot_longer(cols = c(time_abs_logs_continuous, time_rel_logs_continuous), 
               names_to = "score") |>
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100, 
                 colour = "white", linewidth = 0.01) + 
  facet_wrap(~score, ncol = 1, labeller = as_labeller(label_names)) + 
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15)) + 
  expand_limits(y = c(0, 0.18)) + 
  theme_scoringutils() + 
  # theme(axis.title.y = element_blank()) + 
  lims(x = c(-3, 3)) + 
  labs(y = "Proportion", x = "Continuous (global) log scores \n(time weighted)")
  
# p1 + p3

# the shapes look overall quite similar

# distribution of continuous scores - one point in time
p4 <- continuous_scores |>
  select(user_id, question_id, logs_abs_user_point, logs_rel_user_point) |>
  unique() |>
  pivot_longer(cols = c(logs_abs_user_point, logs_rel_user_point), 
               names_to = "score") |>
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100, 
                 colour = "white", linewidth = 0.01) + 
  facet_wrap(~score, ncol = 1, labeller = as_labeller(label_names)) + 
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15)) + 
  expand_limits(y = c(0, 0.18)) + 
  theme_scoringutils() + 
  theme(axis.title.y = element_blank()) + 
  lims(x = c(-3, 3)) + 
labs(y = "Proportion", x = "Continuous (global) log scores\n(one point in time)")

# p2 + p4

(p1 + p2 + p3 + p4) + 
  plot_layout(nrow = 1) + 
  plot_annotation(tag_levels = "A")

ggsave("scoring-rules/output/comparison-scores.jpg", width = 11, height = 4.5)

get_var_name <- function(absolute, binary, time_weighted) {
  if (time_weighted) {
    if (absolute) {
      if (binary) {
        var <- "time_abs_logs_binary"
      } else { # if continuous
        var <- "time_abs_logs_continuous"
      }
    } else { # if relative
      if (binary) {
        var <- "time_rel_logs_binary"
      } else {
        var <- "time_rel_logs_continuous"
      }
    } 
  } else { # if not time_weighted
    if (absolute) {
      var <- "logs_abs_user_point"
    } else {
      var <- "logs_rel_user_point"
    }
  }
  return(var)
}

get_summary <- function(data, absolute = TRUE, binary = TRUE, time_weighted = TRUE) {
  if (time_weighted) {
    selection <- c("user_id", "question_id", 
                   "time_abs_logs_binary", "time_rel_logs_binary",
                   "time_abs_logs_continuous", "time_rel_logs_continuous")
  } else {
    selection <- c("user_id", "question_id", "t", 
                   "logs_abs_user_point", "logs_rel_user_point")
  }
  
  var <- get_var_name(absolute, binary, time_weighted)
  print(var)
  
  data <- data |> 
    select(any_of(selection)) |>
    unique() |>
    filter(is.finite(get(var))) |>
    pull(get(var)) |>
    summary()  |>
    t() |>
    rbind() |>
    as.data.table()
  
  data[, `:=`(
    "kind" = ifelse(absolute, "absolute", "relative"), 
    "binary" = ifelse(binary, "binary", "continuous"), 
    "time" = ifelse(time_weighted, "time weighted", "snapshot"))]
  return(data[])
}

l <- list(
  # Summary stats for time weighted relative scores
  get_summary(continuous_scores, absolute = FALSE, binary = FALSE, time_weighted = TRUE),
  get_summary(binary_scores, absolute = FALSE, binary = TRUE, time_weighted = TRUE), 
  # Summary stats for snapshot one point in time relative scores
  get_summary(continuous_scores, absolute = FALSE, binary = FALSE, time_weighted = FALSE), 
  get_summary(binary_scores, absolute = FALSE, binary = TRUE, time_weighted = FALSE), 
  # Summary stats for time weighted absolute scores
  get_summary(continuous_scores, absolute = TRUE, binary = FALSE, time_weighted = TRUE), 
  get_summary(binary_scores, absolute = TRUE, binary = TRUE, time_weighted = TRUE), 
  # Summary stats for snapshot one point in time absolute scores
  get_summary(continuous_scores, absolute = TRUE, binary = FALSE, time_weighted = FALSE),
  get_summary(binary_scores, absolute = TRUE, binary = TRUE, time_weighted = FALSE)
) |>
  rbindlist()

l |> 
  mutate(across(c(where(is.numeric)), \(x) (signif(x, digits = 3)))) |>
  mutate(across(c(where(is.numeric)), \(x) (round(x, digits = 3)))) |>
  select(Kind = kind, Type = binary, Weighting = time, everything())

# a lot more negative skew for the continuous questinos than for the binary ones

# rank correlation between absolute and relative binary scores - time weighted
cors_binary <- binary_scores |>
  select(user_id, question_id, 
         time_abs_logs_binary, time_rel_logs_binary, 
         logs_abs_user_point, logs_rel_user_point) |>
  unique() |>
  group_by(question_id) |>
  summarise(rank_cor = cor(time_abs_logs_binary, time_rel_logs_binary, 
                           method = "spearman"), 
            rank_cor_snap = cor(logs_abs_user_point, 
                                logs_rel_user_point, 
                                method = "spearman"))

# rank correlation between absolute and relative continuous scores - time weighted
cors_continuous <- continuous_scores |>
  select(user_id, question_id, 
         time_abs_logs_continuous, time_rel_logs_continuous, 
         logs_abs_user_point, logs_rel_user_point) |>
  unique() |>
  filter(is.finite(time_abs_logs_continuous)) |>
  group_by(question_id) |>
  summarise(rank_cor = cor(time_abs_logs_continuous, time_rel_logs_continuous, 
                           method = "spearman"), 
            rank_cor_snap = cor(logs_abs_user_point, 
                                logs_rel_user_point, 
                                method = "spearman"))

cor1 <- cors_binary |> 
  ggplot(aes(x = rank_cor)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100, 
                 colour = "white", linewidth = 0.01) + 
  geom_vline(xintercept = mean(cors$rank_cor), colour = "red", linewidth = 0.2) + 
  theme_scoringutils() + 
  expand_limits(x = c(-1, 1)) + 
  labs(y = "Proportion", x = "Rank correlation abs. vs. rel. binary scores\n(time weighted)")

cor2 <- cors_continuous |> 
  ggplot(aes(x = rank_cor)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100, 
                 colour = "white", linewidth = 0.01) + 
  geom_vline(xintercept = mean(cors_continuous$rank_cor, na.rm = TRUE), colour = "red", linewidth = 0.2) + 
  theme_scoringutils() + 
  expand_limits(x = c(-1, 1)) + 
  labs(y = "Proportion", x = "Rank correlation abs. vs. rel. continuous scores\n(time weighted)")

cor3 <- cors_binary |> 
  ggplot(aes(x = rank_cor_snap)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100, 
                 colour = "white", linewidth = 0.01) + 
  geom_vline(xintercept = mean(cors_binary$rank_cor_snap), colour = "red", linewidth = 0.2) + 
  theme_scoringutils() + 
  expand_limits(x = c(-1, 1)) + 
  labs(y = "Proportion", x = "Rank correlation abs. vs. rel. binary scores\n(snapshot)")

cor4 <- cors_continuous |> 
  ggplot(aes(x = rank_cor_snap)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100, 
                 colour = "white", linewidth = 0.01) + 
  geom_vline(xintercept = mean(cors_continuous$rank_cor_snap, na.rm = TRUE), colour = "red", linewidth = 0.2) + 
  theme_scoringutils() + 
  expand_limits(x = c(-1, 1)) + 
  labs(y = "Proportion", x = "Rank correlation abs. vs. rel. continuous scores\n(snapshot)")
  
(cor1 + cor2) / 
  (cor3 + cor4) + 
  plot_annotation(tag_levels = "A")
ggsave("scoring-rules/output/correlations.jpg", width = 8.5, height = 7)






# abs binary log scores vs. rel. binary log scores - time weighted
binary_time <- binary_scores |>
  select(user_id, question_id, time_abs_logs_binary, time_rel_logs_binary) |>
  unique() |>
  ggplot(aes(y = time_abs_logs_binary, x = time_rel_logs_binary)) + 
  geom_point(size = 0.1, alpha = 0.5) + 
  stat_density_2d(geom = "polygon", alpha = 0.5,
                  aes(fill = after_stat(level)),
                  bins = 30) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_scoringutils() + 
  theme(legend.position="none") + 
  lims(x = c(-2, 2), y = c(-2, 1)) + 
  labs(y = "Absolute binary log score (time averaged)", 
       x = "Relative binary log score (time averaged)")

# abs binary log scores vs. rel. binary log scores - one point in time, not time weighted
binary_snapshot <- binary_scores |>
  select(user_id, question_id, logs_abs_user_point, logs_rel_user_point) |>
  unique() |>
  ggplot(aes(y = logs_abs_user_point, x = logs_rel_user_point)) + 
  geom_point(size = 0.1, alpha = 0.5) + 
  stat_density_2d(geom = "polygon", alpha = 0.5,
                  aes(fill = after_stat(level)),
                  bins = 30) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_scoringutils() + 
  theme(legend.position="none") + 
  lims(x = c(-2, 2), y = c(-2, 1)) + 
  labs(y = "Absolute binary log score (snapshot)", 
       x = "Relative binary log score (snapshot)")


# abs continuous log scores vs. rel. binary log scores - time weighted
continuous_time <- continuous_scores |>
  select(user_id, question_id, time_abs_logs_continuous, time_rel_logs_continuous) |>
  unique() |>
  ggplot(aes(y = time_abs_logs_continuous, x = time_rel_logs_continuous)) + 
  geom_point(size = 0.1, alpha = 0.5) + 
  stat_density_2d(geom = "polygon", alpha = 0.5,
                  aes(fill = after_stat(level)),
                  bins = 30) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_scoringutils() + 
  theme(legend.position="none") + 
  lims(x = c(-2, 2), y = c(-2, 1)) + 
  labs(y = "Absolute continuous log score (time averaged)", 
       x = "Relative continuous log score (time averaged)")


# abs continuous log scores vs. rel. continuous log scores - one point in time, not time weighted
continuous_snapshot <- continuous_scores |>
  select(user_id, question_id, logs_abs_user_point, logs_rel_user_point) |>
  unique() |>
  ggplot(aes(y = logs_abs_user_point, x = logs_rel_user_point)) + 
  geom_point(size = 0.1, alpha = 0.5) + 
  stat_density_2d(geom = "polygon", alpha = 0.5,
                  aes(fill = after_stat(level)),
                  bins = 30) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_scoringutils() + 
  theme(legend.position="none") + 
  lims(x = c(-2, 2), y = c(-2, 1)) + 
  labs(y = "Absolute continuous log score (snapshot)", 
       x = "Relative continuous log score (snapshot)")

p <- binary_time + binary_snapshot + continuous_time + continuous_snapshot + 
  plot_layout(nrow = 2) 
ggsave("scoring-rules/output/score-density.jpg", width = 10, height = 10, plot = p)

# Look at top forecasters
# ------------------------------------------------------------------------------

# - best score
# - at least x resolved forecasts

find_top <- function(data, score = "time_abs_logs_binary", min_resolved = 20) {
  data <- as.data.table(data)
  
  data <- data[, .SD, .SDcols = c("user_id", "question_id", score)] |>
    group_by(user_id) |>
    filter(length(unique(question_id)) >= min_resolved) |>
    filter(is.finite(get(score))) |>
    summarise(score = mean(get(score), na.rm = TRUE)) |>
    arrange(-score) |>
    rename(anonymised = user_id) |>
    left_join(users, by = "anonymised")
  
  return(data)
}

find_top(binary_scores, min_resolved = 1000) |> print(n = 10)
find_top(binary_scores, score = "time_rel_logs_binary", min_resolved = 10)


find_top(continuous_scores, score = "time_rel_logs_continuous", min_resolved = 1000) |> print(n = 10)


























# ============================================================================ #
# Appendix
# ============================================================================ #

# Testing the new scoring rules on some of Tom's examples - seems to work
# ------------------------------------------------------------------------------

pred <- c(0, 0.02, 0.04, 0.06, 0.08, 1)
x_grid <- 0:5
resolution <- 4
score_log_continuous(pred = pred, compare_against = "prior", 
                     resolution = resolution, x_grid = x_grid, 
                     lower_bound_type = "closed", upper_bound_type = "closed")

pred <- c(0, seq(0.9, 0.999, length.out = 100))
x_grid <- 0:100
resolution <- 100
score_log_continuous(pred = pred, compare_against = "prior", 
                     resolution = resolution, x_grid = x_grid, 
                     lower_bound_type = "closed", upper_bound_type = "closed")

pred <- c(0, seq(0.1, 0.199, length.out = 199), 1)
x_grid <- 0:200
resolution <- 1
score_log_continuous(pred = pred, compare_against = "prior", 
                     resolution = resolution, x_grid = x_grid, 
                     lower_bound_type = "closed", upper_bound_type = "closed")

pred <- c(seq(0.002, 0.1, length.out = 50), seq(0.898, 0.998, length.out = 51))
x_grid <- 0:100
resolution <- 50
score_log_continuous(pred = pred, compare_against = "prior", 
                     resolution = resolution, x_grid = x_grid, 
                     lower_bound_type = "open", upper_bound_type = "open")

get_weighting_factor(201, "closed", "closed")
get_weighting_factor(101, "open", "open")


