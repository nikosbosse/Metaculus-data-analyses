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

julia <- julia_setup()
julia_library("DataFrames")
julia_source("julia/community-prediction.jl")

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
  } 
  score <- ((score * durations) / t_total)
  score <- sum(score)
  
  return(score)
}

# helper function to compute score based on the data frame format
score_df <- function(data, pred, ...) {
  time_weighted_score(
    pred = pred, 
    resolution = data$resolution, 
    t = data$t, 
    close_time = unique(data$close_time), 
    ...
  )
}


# ============================================================================ #
# Analysis
# ============================================================================ #

julia_source("julia/community-prediction.jl")

# testing the function
df <- filter(combined, question_id %in% c(1, 4, 8, 5632)) |>
  rename(cp_old = cp)
test <- julia_call("apply_cp", df) |>
  as_tibble()



# **Research question(s)**
#   - Which users add most value to the CP? Do they add value consistently?
#   - How does value-adding behaviour correlate with overall performance?
#   - How does the value added depend on the number of forecasters?
#   - Potentially (but maybe not feasible in the time): Are there any patterns / characteristics of these users that help us identify them?
#   **Potential method(s)**
#   - For every user / question: compute the CP with that user and without that user
# - Determine users for which the difference between the CP with them and without them is biggest
# - This will be confounded by the overall number of predictors. One could in principle get around that by using the bootstrapping approach outlined above (i.e. setting ensemble sizes to e.g. 50 and then randomly drawing ensembles of size 50). However, that is computationally expensive. It's also a fact of life that new forecasters may just not be adding much after there are already a certain number of forecasters. Or we might find the opposite and are amazed.
# - Potentially plot the difference between CP with and without a user as a function of the number of other predictors for a given question. This seems like a cool complementary / alternative analysis to the previous suggestion about ensemble sizes and also tells us something about the question "How much does another user add when there are already are x forecasters"
# - Potentially try and identify characteristics of users who contribute much
# 	- they comment a lot
# 	- they forecast a lot
# 	- they have good performance
# 	- they predict far away from the CP


# per question
# per user
# get CP with and without them

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
}

nrow(res)


# currently unused version
for (q in question_ids) {
  
  julia_source("julia/community-prediction.jl")
  test = julia_call("remove_user_and_compute_cp", df, 125745)
  
  test2 = julia_call("iterate_users_and_compute_cp", filter(df))
  
  
  
  nrow(test)
  nrow(all_df_without$`125745`)
  
  all(all_df_without$`125745` == test)
  head(all_df_without[1])
  
  df <- filter(data, question_id %in% q)
  users <- unique(df$user_id)
  all_df_without <- list()
  for (user in users[1:2]) {
    df_without <- filter(df, user_id != user)
    
    df_without <- julia_call("apply_cp", df_without)
    
    all_df_without[[paste(user)]] <- df_without |>
      select(question_id, cp) |>
      mutate(without_user = user)
  }
  
  
  res[[paste(q)]] <- rbindlist(scores_without)
}





















