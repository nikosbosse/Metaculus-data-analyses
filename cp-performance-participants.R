# library(arrow)
library(data.table)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(scoringutils)
library("matrixStats")
library(future.apply)

# ============================================================================ #
#                                 Load data                                    #
# ============================================================================ #

binary_pred_raw <- fromJSON("data/predictions-binary-hackathon.json") |>
  as.data.table() 

binary_questions_raw <- fromJSON("data/questions-binary-hackathon.json") |>
  as.data.table()

continuous_pred <- read_parquet("data/predictions-continuous-hackathon-v2.parquet")
setDT(continuous_pred)
continuous_questions <- fromJSON("data/questions-continuous-hackathon.json") |>
  as.data.table()


# ============================================================================ #
# filter data
# ============================================================================ #

# only keep questions that have resolved, get rid of unnecessary cols ----------
binary_questions <- binary_questions_raw |>
  select(question_id, resolution) |>
  filter(resolution %in% c(0, 1))

binary_pred <- binary_pred_raw |>
  select(user_id, question_id, t, prediction, cp)

binary <- inner_join(binary_questions, binary_pred)

# only keep questions with >= 50 users -----------------------------------------
n_users <- 
  binary |> 
  group_by(question_id) |>
  dplyr::summarise(n_user = length(unique(user_id)))

n_users |>
  ggplot(aes(x = n_user)) +
  geom_histogram(color = "grey20", fill = "grey80") +
  theme_scoringutils() + 
  scale_x_continuous(trans = "log10")

binary <- binary |>
  inner_join(
    n_users |>
      filter(n_user >= 50)) |>
  arrange(question_id, t)

# ============================================================================ #
# Define functions 
# ============================================================================ #

# function to calculate the latest community prediction
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

test_questions <- binary$question_id |>
  unique() |>
  head(20)

binary |> 
  as_tibble() |>
  filter(question_id %in% test_questions) |>
  group_by(question_id) |>
  mutate(cp_test = calc_latest_cp(prediction)) |>
  dplyr::mutate(row_id = row_number()) |>
  dplyr::filter(row_id == max(row_id))


# function to compute the hypothetical cp for a given number of hypothetical 
# users
hypothetical_cp <- function(df, n_users, n_rep = 1000) {
  user_ids <- df$user_id |>
    unique()
  
  user_samples <- replicate(
    n_rep, 
    sample(x = user_ids, size = n_users, replace = FALSE)
  ) 
  
  score <- apply(user_samples, MARGIN = 2, function(users) {
    cp <- df[user_id %in% users, ]$prediction |>
      calc_latest_cp()
    
    # brier score
    (df$resolution[1] - cp)^2
  }) 
 
  mean <- mean(score)
  var <- var(score) 
  
  return(c("mean" = mean, "var" = var))
}


# function to apply the hypothetical cp to a data.frame and store results
simulate <- function(binary, n_users, write = TRUE, n_rep = 1e3) {
  sim <- binary[, .("score" = hypothetical_cp(.SD, n_users = n_users, 
                                              n_rep = n_rep)), 
                by = "question_id"]
  sim$desc <- rep(c("mean", "var"), times = nrow(sim) / 2)
  sim$n_users <- n_users
  
  sim <- sim |> 
    data.table::dcast(question_id + n_users ~ desc, value.var = "score")
  
  if (write) {
    filename <- paste0("output/data/simulate_cp_", n_users)
    fwrite(sim, file = filename)
  }
  return(sim)
}

# ============================================================================ #
# Apply functions to calculate hypothetical cp
# ============================================================================ #

library(foreach)
library(doParallel)

registerDoParallel(cores = 4)

grid <- c(5, 7, 10, 15, 20, 25, 30, 35, 40, 50)

out <- foreach (i = grid, .combine = rbind)  %dopar% {
  simulate(binary, n_users = i, n_rep = 5000)
}

# fwrite(out, "output/data/all_sims_cp.csv")

out <- fread("output/data/all_sims_cp.csv")

out |>
  group_by(n_users) |>
  summarise(mean = mean(mean), 
            sd = mean(sqrt(var))) |>
  mutate(lower = mean - sd, 
         upper = mean + sd) |>
  ggplot(aes(x = n_users)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4) + 
  geom_line(aes(y = mean)) +
  theme_scoringutils()





