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
  time_abs_logs_continuous = "Absolute time avg. continuous log score", 
  time_rel_logs_binary = "Relative time avg. binary log score", 
  time_rel_logs_continuous = "Relative time avg. continuous log score", 
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
  theme_scoringutils() + 
  theme(axis.title.y = element_blank()) + 
  lims(x = c(-3, 3)) + 
labs(y = "Proportion", x = "Continuous (global) log scores\n(one point in time)")

# p2 + p4

(p1 + p2 + p3 + p4) + 
  plot_layout(nrow = 1)

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

# a lot more negative skew for the continuous questinos than for the binary ones

# rank correlation between absolute and relative binary scores - time weighted
cors <- binary_scores |>
  select(user_id, question_id, time_abs_logs_binary, time_rel_logs_binary) |>
  unique() |>
  group_by(question_id) |>
  summarise(rank_cor = cor(time_abs_logs_binary, time_rel_logs_binary, 
                           method = "spearman"))

cors |> 
  ggplot(aes(x = rank_cor)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100, 
                 colour = "white", linewidth = 0.01) + 
  geom_vline(xintercept = mean(cors$rank_cor), colour = "red", linewidth = 0.2) + 
  theme_scoringutils() + 
  labs(y = "Proportion", x = "Rank correlation abs. vs. rel. scores")


# rank correlation between absolute and relative continuous scores - time weighted
cors_continuous <- continuous_scores |>
  select(user_id, question_id, time_abs_logs_continuous, time_rel_logs_continuous) |>
  unique() |>
  filter(is.finite(time_abs_logs_continuous)) |>
  group_by(question_id) |>
  summarise(rank_cor = cor(time_abs_logs_continuous, time_rel_logs_continuous, 
                           method = "spearman"))

cors_continuous |> 
  ggplot(aes(x = rank_cor)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100, 
                 colour = "white", linewidth = 0.01) + 
  geom_vline(xintercept = mean(cors_continuous$rank_cor, na.rm = TRUE), colour = "red", linewidth = 0.2) + 
  theme_scoringutils() + 
  labs(y = "Proportion", x = "Rank correlation abs. vs. rel. continuous log scores")
  

# abs binary log scores vs. rel. binary log scores - time weighted
binary_scores |>
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
  lims(x = c(-2, 2), y = c(-2, 1))

# abs binary log scores vs. rel. binary log scores - one point in time, not time weighted
binary_scores |>
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
  lims(x = c(-2, 2), y = c(-2, 1))


# abs continuous log scores vs. rel. binary log scores - time weighted
continuous_scores |>
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
  lims(x = c(-2, 2), y = c(-2, 1))


# abs continuous log scores vs. rel. continuous log scores - one point in time, not time weighted
continuous_scores |>
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
  lims(x = c(-2, 2), y = c(-2, 1))


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





# there is a small question here: should we compare against the CP after a user 
# made their prediction (which is the default in the data) or the CP the user
# "saw" when making their prediction (would need to take 1st differences here)










































test

test_whole <- combined_continuous |> 
  filter(question_id == 12432)
test2 <- combined_continuous |> 
  filter(question_id == 12432, 
         user_id == 117429)

pred_user <- test2$cdf
t_user <- test2$t
x_grid <- test2$x_grid |> unique() |> unlist()
resolution <- test2$resolution |> unique()
open_time <- test2$publish_time |> unique()
close_time <- test2$close_time |> unique()
lower_bound_type <- test2$lower_bound_type |> unique()
upper_bound_type <- test2$upper_bound_type |> unique()

pred_cp <- test_whole$cdf
t_cp <- test_whole$t

create_combined_prediction_df(
  pred_user = pred_user, 
  t_user = t_user, 
  pred_cp = pred_cp, 
  t_cp = t_cp
)










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







