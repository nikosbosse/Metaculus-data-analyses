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
  filter(!is.na(resolution))



# ============================================================================ #
# helper functions
# ============================================================================ #

# get reputations at a given time point
get_reputations_at_t <- function(start_t) {
  reps <- combined |> 
    select(user_id, t, question_id, reputation_at_t) |>
    arrange(user_id, t) |> 
    filter(t <= start_t) |>
    group_by(user_id) |>
    mutate(n_questions = length(unique(question_id))) |>
    slice(n()) |>
    ungroup() |>
    arrange(reputation_at_t)
  return(reps)
}

# function to calculate the latest community prediction for a vector of predictions
# function was tested and seems correct. 
calc_latest_cp <- function(preds) {
  n <- length(preds)
  index <- 1:n
  weight <- exp(sqrt(index) - sqrt(n))
  
  if(length(weight) != length(preds)) {
    print(n)
    print(weight)
    print(preds)
  }
  matrixStats::weightedMedian(x = preds, w = weight)
}

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
  } 
  score <- ((score * durations) / t_total)
  score <- sum(score)
  
  return(score)
}

# helper function to compute score based on the data frame format
score_df <- function(data, pred) {
  time_weighted_score(
    pred = pred, 
    resolution = data$resolution, 
    t = data$t, 
    close_time = unique(data$close_time)
  )
}

# calculate scores for the best and the rest
scores_best_rest <- function(qid, n_best = 10) {
  
  question_data <- combined |>
    filter(question_id %in% qid)
  
  start_t <- unique(question_data$publish_time)
  participating_users <- unique(question_data$user_id)
  
  # get all reputations for everyone
  reps <- get_reputations_at_t(start_t = start_t)
  
  participating_reps <- reps |>
    filter(user_id %in% participating_users)
  
  best_participating <- participating_reps |>
    slice_tail(n = n_best)
  
  worst_participating <- participating_reps |>
    slice_head(n = n_best)
  
  rep_best <- mean(best_participating$reputation_at_t)
  
  best_users <- participating_users[participating_users %in% best_participating$user_id]
  rest_users <- participating_users[!(participating_users %in% best_users)]
  worst_users <- participating_users[participating_users %in% worst_participating$user_id]
  not_worst_users <- participating_users[!(participating_users %in% worst_users)]
  
  if (length(best_users) < n_best | length(rest_users) < n_best) {
    out <- data.frame(
      "BrierBest" = numeric(), 
      "BrierRest" = numeric(),
      "BrierAll" = numeric(), 
      "BrierMP" = numeric(),
      "QuestionID" = numeric())
    return(out)
  }
  
  q_best <- question_data |>
    filter(user_id %in% best_users)
  
  q_rest <- question_data |>
    filter(user_id %in% rest_users)
  
  q_not_worst <- question_data |>
    filter(user_id %in% not_worst_users)
  
  setDT(q_best)
  for (i in 1:nrow(q_best)) {
    q_best[i, "cp_best" := calc_latest_cp(
      preds = q_best[1:i]$prediction)
    ]
  }
  
  setDT(q_rest)
  for (i in 1:nrow(q_rest)) {
    q_rest[i, "cp_rest" := calc_latest_cp(
      preds = q_rest[1:i]$prediction)
    ]
  }
  
  setDT(q_not_worst)
  for (i in 1:nrow(q_not_worst)) {
    q_not_worst[i, "cp_not_worst" := calc_latest_cp(
      preds = q_not_worst[1:i]$prediction)
    ]
  }
  
  brier_best <- score_df(q_best, pred = q_best$cp_best)
  brier_rest <- score_df(q_rest, pred = q_rest$cp_rest)
  brier_not_worst <- score_df(q_not_worst, pred = q_not_worst$cp_not_worst)
  brier_all <- score_df(question_data, pred = question_data$cp)
  brier_mp <- score_df(question_data, pred = question_data$mp)
  
  return(data.frame(
    "BrierBest" = brier_best, 
    "BrierRest" = brier_rest,
    "BrierNotWorst" = brier_not_worst,
    "BrierAll" = brier_all,
    "BrierMP" = brier_mp,
    "QuestionID" = qid, 
    "RepBest" = rep_best
  ))
}


# separate function to make a random draw of forecasters and score that
scores_cp_random <- function(qid, n_random = 10, draws = 10) {

  question_data <- combined |>
    filter(question_id %in% qid)

  start_t <- unique(question_data$publish_time)
  participating_users <- unique(question_data$user_id)
  
  random_scores <- numeric(length = draws)
  random_reps <- numeric(length = draws)
  
  reps <- get_reputations_at_t(start_t = start_t) |>
    filter(user_id %in% participating_users)
  
  if (nrow(reps) < n_random) {
    out <- data.frame(
      BrierRandom = numeric(), 
      QuestionId = numeric(), 
      sample = numeric(),
      RepRandom = numeric()
    )
    return(out)
  }
  
  for (j in 1:draws) {
    # take a random sample of those that already have a reputation, i.e. 
    # who existed before the question was published
    random_users <- sample(x = reps$user_id, size = n_random, replace = FALSE)
  
    q_random <- question_data |>
      filter(user_id %in% random_users)
    
    random_reps[j] <- reps |>
      filter(user_id %in% random_users) |>
      pull(reputation_at_t) |>
      mean()
    
    setDT(q_random)
    for (i in 1:nrow(q_random)) {
      q_random[i, "cp_random" := calc_latest_cp(
        preds = q_random[1:i]$prediction
      )]
    }
    
    random_scores[j] <- score_df(q_random, pred = q_random$cp_random)
  }

  return(data.frame(
    BrierRandom = random_scores, 
    QuestionId = qid, 
    sample = 1:draws,
    RepRandom = random_reps
  ))
}

## helper function to filter for a certain minimum number of forecasters
filter_no_forecasters <- function(data, min_number = 100) {
  numbers <- binary_pred_raw |>
    group_by(question_id) |>
    summarise(n_forecasters = length(unique(user_id)))
  
  numbers |>
    rename(QuestionID = question_id) |>
    filter(n_forecasters >= min_number) |>
    inner_join(data)
}






# ============================================================================ #
# actual analysis
# ============================================================================ #
all_qids <- unique(combined$question_id)

# run function. Apologies for this potato code...
if (FALSE) {
  scores_best_5 <- purrr::map_dfr(all_qids, .f = function(x) {
    scores_best_rest(qid = x, n_best = 5)
  })
  fwrite(scores_best_5, file = "forecaster-subsets/results/scores_best_5.csv")
  
  scores_random_5 <- purrr::map_dfr(all_qids, .f = function(x) {
    scores_cp_random(qid = x, n_random = 5, draws = 10)
  })
  fwrite(scores_random_5, file = "forecaster-subsets/results/scores_random_5.csv")
  
  
  scores_best_10 <- purrr::map_dfr(all_qids, .f = function(x) {
    scores_best_rest(qid = x, n_best = 10)
  })
  fwrite(scores_best_10, file = "forecaster-subsets/results/scores_best_10.csv")
  
  scores_random_10 <- purrr::map_dfr(all_qids, .f = function(x) {
    scores_cp_random(qid = x, n_random = 10, draws = 10)
  })
  fwrite(scores_random_10, file = "forecaster-subsets/results/scores_random_10.csv")
  
  
  scores_best_15 <- purrr::map_dfr(all_qids, .f = function(x) {
    scores_best_rest(qid = x, n_best = 15)
  })
  fwrite(scores_best_15, file = "forecaster-subsets/results/scores_best_15.csv")
  
  scores_random_15 <- purrr::map_dfr(all_qids, .f = function(x) {
    scores_cp_random(qid = x, n_random = 15, draws = 10)
  })
  fwrite(scores_random_15, file = "forecaster-subsets/results/scores_random_15.csv")
  
  
  scores_best_20 <- purrr::map_dfr(all_qids, .f = function(x) {
    scores_best_rest(qid = x, n_best = 20)
  })
  fwrite(scores_best_20, file = "forecaster-subsets/results/scores_best_20.csv")
  
  scores_random_20 <- purrr::map_dfr(all_qids, .f = function(x) {
    scores_cp_random(qid = x, n_random = 20, draws = 10)
  })
  fwrite(scores_random_20, file = "forecaster-subsets/results/scores_random_20.csv")
  
  
  scores_best_25 <- purrr::map_dfr(all_qids, .f = function(x) {
    scores_best_rest(qid = x, n_best = 25)
  })
  fwrite(scores_best_25, file = "forecaster-subsets/results/scores_best_25.csv")
  
  scores_random_25 <- purrr::map_dfr(all_qids, .f = function(x) {
    scores_cp_random(qid = x, n_random = 25, draws = 10)
  })
  fwrite(scores_random_25, file = "forecaster-subsets/results/scores_random_25.csv")
  
  
  scores_best_30 <- purrr::map_dfr(all_qids, .f = function(x) {
    scores_best_rest(qid = x, n_best = 30)
  })
  fwrite(scores_best_30, file = "forecaster-subsets/results/scores_best_30.csv")
  
  scores_random_30 <- purrr::map_dfr(all_qids, .f = function(x) {
    scores_cp_random(qid = x, n_random = 30, draws = 10)
  })
  fwrite(scores_random_30, file = "forecaster-subsets/results/scores_random_30.csv")
  
  best_forecasters <- rbind(
    mutate(scores_best_5, best = 5),
    mutate(scores_best_10, best = 10),
    mutate(scores_best_15, best = 15),
    mutate(scores_best_20, best = 20),
    mutate(scores_best_25, best = 25), 
    mutate(scores_best_30, best = 30) 
  )
  
  fwrite(best_forecasters, "forecaster-subsets/results/best-forecasters.csv")
  
  random_forecasters <- rbind(
    mutate(scores_random_5, best = 5),
    mutate(scores_random_10, best = 10),
    mutate(scores_random_15, best = 15),
    mutate(scores_random_20, best = 20),
    mutate(scores_random_25, best = 25), 
    mutate(scores_random_30, best = 30) 
  )
  fwrite(random_forecasters, "forecaster-subsets/results/random-forecasters.csv")
} {
  best_forecasters <- fread("forecaster-subsets/results/best-forecasters.csv")
  random_forecasters <- fread("forecaster-subsets/results/random-forecasters.csv")
}

## join random forecasters and best forecasters
random_summarised <- random_forecasters |>
  filter(QuestionId %in% unique(best_forecasters$QuestionID)) |>
  group_by(QuestionId, best) |>
  summarise(BrierRandom = mean(BrierRandom), 
            RepRandom = mean(RepRandom)) |>
  rename(QuestionID = QuestionId)

all <- random_summarised |>
  inner_join(best_forecasters)


# ============================================================================ #
# get all results together in a single data.frame
res <- list() 
grid <- c(1:12 * 50)
for (i in grid) {
  res[[i]] <- all |>
    group_by(QuestionID) |>
    # only keep questions if we have data for all different numbers of forecasters
    filter(length(unique(best)) == 6) |>
    filter_no_forecasters(min_number = i) |>
    group_by(best) |>
    summarise(across(everything(), mean)) |>
    mutate(min_forecasters = i)
}
res <- rbindlist(res)




# ============================================================================ #
# absolute scores
res |>
  rename("CP (all)" = BrierAll, 
         "CP (best)" = BrierBest, 
         "CP (rest)" = BrierRest, 
         "MP" = BrierMP) |>
  pivot_longer(cols = c(`CP (all)`, `CP (best)`, `CP (rest)`, MP), 
               names_to = "type", values_to = "score") |>
  ggplot(aes(y = score, x = min_forecasters, colour = type)) + 
  geom_line() + 
  facet_wrap(~ best, scales = "free") + 
  scale_color_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8", "grey50")) + 
  # scale_y_continuous(limits = c(0.06, 0.14)) + 
  theme_scoringutils() +
  labs(y = "Avg. Brier score", x = "Minimum number of forecasters", 
       color = "Ensemble method")

ggsave(filename = "forecaster-subsets/results/plots/scores.jpg", 
       height = 5, width = 7)


# ============================================================================ #
# relative scores
res |>
  mutate(BrierBest = BrierBest / BrierAll, 
         BrierRest = BrierRest / BrierAll, 
         BrierMP = BrierMP / BrierAll, 
         BrierAll = 1) |>
  rename("CP (all)" = BrierAll, 
         "CP (best)" = BrierBest, 
         "CP (rest)" = BrierRest, 
         "MP" = BrierMP) |>
  pivot_longer(cols = c(`CP (all)`, `CP (best)`, `CP (rest)`, MP), 
               names_to = "type", values_to = "score") |>
  ggplot(aes(y = score, x = min_forecasters, colour = type)) + 
  geom_line() + 
  expand_limits(y = c(0.9, 1.2)) + 
  facet_wrap(~ best, scales = "free") + 
  scale_color_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8", "grey50")) + 
  theme_scoringutils() + 
  labs(y = "Avg. relative Brier score", x = "Minimum number of forecasters", 
       color = "Ensemble method")

ggsave(filename = "forecaster-subsets/results/plots/relative-scores.jpg", 
       height = 5.8, width = 7)




# ============================================================================ #
# relative random and the best
res |>
  mutate(BrierBest = BrierRest / BrierAll, 
         BrierRandom = BrierRandom / BrierAll,
         BrierAll = 1) |>
  rename("CP (all)" = BrierAll, 
         "CP (best)" = BrierBest, 
         "CP (random)" = BrierRandom) |>
  pivot_longer(cols = c(`CP (all)`, `CP (best)`, `CP (random)`), 
               names_to = "type", values_to = "score") |>
  ggplot(aes(y = score, x = min_forecasters, colour = type)) + 
  geom_line() + 
  expand_limits(y = c(1, 1.6)) + 
  facet_wrap(~ best, scales = "free") + 
  scale_color_manual(values = c("#E41A1C", "#4DAF4A", "#984EA3")) + 
  theme_scoringutils() + 
  labs(y = "Avg. relative Brier score", x = "Minimum number of forecasters", 
       color = "Ensemble method")

ggsave(filename = "forecaster-subsets/results/plots/relative-scores-random.jpg", 
       height = 5.8, width = 7)





# ============================================================================ #
# relative not the worst vs. the rest
res |>
  mutate(BrierRest = BrierRest / BrierAll, 
         BrierNotWorst = BrierNotWorst / BrierAll,
         BrierAll = 1) |>
  rename("CP (all)" = BrierAll, 
         "CP (rest)" = BrierRest, 
         "CP (random)" = BrierRandom,
         "CP (not worst)" = BrierNotWorst) |>
  pivot_longer(cols = c(`CP (all)`, `CP (rest)`, `CP (not worst)`), 
               names_to = "type", values_to = "score") |>
  ggplot(aes(y = score, x = min_forecasters, colour = type)) + 
  geom_line() + 
  expand_limits(y = c(1, 1.05)) + 
  facet_wrap(~ best, scales = "free") + 
  scale_color_manual(values = c("#E41A1C", "#FF7F00", "#377EB8")) + 
  theme_scoringutils() + 
  labs(y = "Avg. relative Brier score", x = "Minimum number of forecasters", 
       color = "Ensemble method")

ggsave(filename = "forecaster-subsets/results/plots/relative-scores-not-worst.jpg", 
       height = 5.8, width = 7)




# ============================================================================ #
## average reputation of the best over time
reps <- list()
for (i in (1:12 * 50)) {
  reps[[i]] <- best_forecasters |>
    filter_no_forecasters(min_number = i) |>
    select(best, RepBest, QuestionID) |>
    group_by(best) |>
    summarise(RepBest = mean(RepBest)) |>
    mutate(min_number = i)
}
reps <- rbindlist(reps) 

reps |>
  ggplot(aes(x = min_number, y = RepBest, group = best, colour = factor(best))) +
  geom_line() + 
  theme_scoringutils() + 
  labs(y = "Avg. reputation score of the 'Best'", 
       x = "Minimum number of forecasters", 
       colour = "Size of the 'Best'")

ggsave("forecaster-subsets/results/plots/average-reputation.jpg", 
       width = 7, height = 3.5)
  


# ============================================================================ #
## Question numbers  
q_nums <- list()
for (i in (1:12 * 50)) {
  q_nums[[i]] <- best_forecasters |>
    filter_no_forecasters(min_number = i) |>
    select(QuestionID, n_forecasters) |>
    unique() |>
    nrow()
}
q_nums <- unlist(q_nums)







# ============================================================================ #
## Additional analyses and sense checks
# ============================================================================ #


## significance
best_forecasters |>
  group_by(QuestionID) |>
  # only keep questions if we have data for all different numbers of forecasters
  filter(length(unique(best)) == 6) |>
  filter_no_forecasters(min_number = 400) |>
  group_by(best) |>
  summarise(
    Best_vs_Rest = wilcox.test(BrierBest, BrierRest, paired = TRUE)$p.value, 
    Best_vs_All = wilcox.test(BrierBest, BrierAll, paired = TRUE)$p.value, 
    Best_vs_MP = wilcox.test(BrierBest, BrierMP, paired = TRUE)$p.value
  )


# this is a function that removes the recency weighting and just computes
# the median as the community forecast
scores_just_median <- function(qid, n_best = 10) {
  
  question_data <- combined |>
    filter(question_id %in% qid)
  
  start_t <- unique(question_data$publish_time)
  participating_users <- unique(question_data$user_id)
  
  reps <- get_reputations_at_t(start_t = start_t)
  
  participating_reps <- reps |>
    filter(user_id %in% participating_users)
  
  best_participating <- participating_reps |>
    slice_tail(n = n_best)
  
  worst_participating <- participating_reps |>
    slice_head(n = n_best)
  
  rep_best <- mean(best_participating$reputation_at_t)
  
  best_users <- participating_users[participating_users %in% best_participating$user_id]
  rest_users <- participating_users[!(participating_users %in% best_users)]
  worst_users <- participating_users[participating_users %in% worst_participating$user_id]
  not_worst_users <- participating_users[!(participating_users %in% worst_users)]
  
  if (length(best_users) < n_best | length(rest_users) < n_best) {
    out <- data.frame(
      "BrierBest" = numeric(), 
      "BrierRest" = numeric(),
      "BrierAll" = numeric(), 
      "BrierMP" = numeric(),
      "QuestionID" = numeric())
    return(out)
  }
  
  
  q_best <- question_data |>
    filter(user_id %in% best_users)
  
  q_rest <- question_data |>
    filter(user_id %in% rest_users)
  
  q_not_worst <- question_data |>
    filter(user_id %in% not_worst_users)
  
  setDT(q_best)
  for (i in 1:nrow(q_best)) {
    q_best[i, "cp_best" := median(q_best[1:i]$prediction)
    ]
  }
  
  setDT(q_rest)
  for (i in 1:nrow(q_rest)) {
    q_rest[i, "cp_rest" := median(q_rest[1:i]$prediction)
    ]
  }
  
  setDT(q_not_worst)
  for (i in 1:nrow(q_not_worst)) {
    q_not_worst[i, "cp_not_worst" := median(q_not_worst[1:i]$prediction)
    ]
  }
  
  brier_best <- score_df(q_best, pred = q_best$cp_best)
  brier_rest <- score_df(q_rest, pred = q_rest$cp_rest)
  brier_not_worst <- score_df(q_not_worst, pred = q_not_worst$cp_not_worst)
  brier_all <- score_df(question_data, pred = question_data$cp)
  brier_mp <- score_df(question_data, pred = question_data$mp)
  
  return(data.frame(
    "BrierBest" = brier_best, 
    "BrierRest" = brier_rest,
    "BrierNotWorst" = brier_not_worst,
    "BrierAll" = brier_all,
    "BrierMP" = brier_mp,
    "QuestionID" = qid, 
    "RepBest" = rep_best
  ))
}



