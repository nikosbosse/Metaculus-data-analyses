library(data.table)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(scoringutils)
library("matrixStats")
library(future.apply)
library(ggpmisc)
library(patchwork)
library(foreach)
library(doParallel)

# ============================================================================ #
#                                 Load data                                    #
# ============================================================================ #

binary_pred_raw <- fromJSON("data/predictions-binary-hackathon.json") |>
  as.data.table() 

binary_questions_raw <- fromJSON("data/questions-binary-hackathon.json") |>
  as.data.table()

# ============================================================================ #
# filter data - helper functions
# ============================================================================ #

# function to filter questions that don't have at least n unique forecasters
filter_users <- function(binary, n = 150) {
  n_users <- 
    binary |> 
    group_by(question_id) |>
    dplyr::summarise(n_user = length(unique(user_id)))
  
  binary <- binary |>
    inner_join(
      n_users |>
        filter(n_user >= n)) |>
    arrange(question_id, t)
  return(binary)
}

# filter out question where the mean brier score is below a certain threshold
filter_low_bs <- function(result, bs_thresh = 0.01) {
  filtered_questions <- result |>
    group_by(question_id) |>
    filter(mean(mean) >= bs_thresh)
  
  return(filtered_questions)
}


# ============================================================================ #
# Calculate community prediction - helper functions
# ============================================================================ #

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

# function that does the actual bootstrapping, drawing a random set of users
hypothetical_cp <- function(df, n_users, n_rep = 1000, with_replacement = TRUE) {
  user_ids <- df$user_id 
  unique_ids <- unique(user_ids)
  
  user_samples <- replicate(
    n_rep, 
    sample(x = unique_ids, size = n_users, replace = with_replacement)
  ) 
    
  score <- apply(user_samples, MARGIN = 2, function(users) {
    indices <- sapply(users, function(user) {
      which(user_ids == user)
    }) |>
      unlist() |>
      sort()
    
    cp <- df[indices, ]$prediction |>
      calc_latest_cp()
    
    # brier score
    (df$resolution[1] - cp)^2
  })
  
  time <- apply(user_samples, MARGIN = 2, function(users) {
    max(df[user_id %in% users, ]$t)
  })
 
  mean <- mean(score)
  var <- var(score) 
  max_t = mean(time)
  
  return(c("mean" = mean, "var" = var, "max_t" = max_t))
}


# function to apply the hypothetical cp to a data.frame and store results
simulate <- function(binary, n_users, write = TRUE, n_rep = 1e3, 
                     with_replacement = TRUE,
                     folder = "output/data/") {
  sim <- binary[, .("score" = hypothetical_cp(.SD, n_users = n_users, 
                                              n_rep = n_rep, 
                                              with_replacement = with_replacement)), 
                by = "question_id"]
  sim$desc <- rep(c("mean", "var", "max_t"), times = nrow(sim) / 3)
  sim$n_users <- n_users
  
  sim <- sim |> 
    data.table::dcast(question_id + n_users ~ desc, value.var = "score")
  
  if (write) {
    filename <- paste0(folder, "simulate_cp_", n_users)
    fwrite(sim, file = filename)
  }
  return(sim)
}



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

# Additional filtering, "Charles method" ---------------------------------------
# - keep only first 25 percent of the question time. 
# - keep questions if the newest prediction is <1 year newer than the oldest
# https://forum.effectivealtruism.org/posts/xF8EWBouJRZpRgFgu/how-does-forecast-quantity-impact-forecast-quality-on-1

s_in_year <- 60 * 60 * 24 * 365

binary_charles <- binary |>
  group_by(question_id) |>
  filter(t <= (min(t) + 0.25 * (max(t) - min(t))), 
         max(t) - min(t) <= s_in_year)


# ============================================================================ #
# Apply functions to calculate hypothetical cp
# ============================================================================ #

registerDoParallel(cores = 4)

grid <- c(5, 7, 10, 15, 20, 25, 30, 35, 40, 50, 65, 80, 100, 120, 150)

# first version with data including all predictions until question closes
# including only questions with at least 150 forecasters
out <- foreach (i = grid, .combine = rbind)  %dopar% {
  simulate(binary, n_users = i, n_rep = 5000)
}
fwrite(out, "output/data/150/all_sims_cp.csv")


# "Charles filter", only using the first 25% of the question lifetime
# including only questions with at least 150 forecasters
out_charles <- foreach (i = grid, .combine = rbind)  %dopar% {
  data <- binary_charles |>
    filter_users(n = 150) |>
    as.data.table()
  
  simulate(binary = data, 
           n_users = i, 
           write = TRUE,
           with_replacement = TRUE,
           n_rep = 5000, folder = "output/data/charles/")
}

fwrite(out_charles, "output/data/charles/all_sims_cp.csv")

# "Charles filter", only using the first 25% of the question lifetime
# including only questions with at least 150 forecasters
# sampled without replacement
out_charles_without_r <- foreach (i = grid, .combine = rbind)  %dopar% {
  simulate(binary_charles |>
             filter_users(n = 150) |>
             as.data.table()
           , n_users = i, n_rep = 5000, 
           with_replacement = FALSE,
           folder = "output/data/charles-without-replacement/")
}

fwrite(out_charles_without_r, "output/data/charles-without-replacement/all_sims_cp.csv")




# ============================================================================ #
# Analysis - helper functions
# ============================================================================ #

regression <- function(binary) {
  binary |>
    group_by(question_id) |>
    filter(t == max(t)) |>
    mutate(score = (cp - resolution)^2) |>
    ungroup() |>
    ggplot(aes(x = n_user, y = score)) + 
    stat_poly_line() +
    stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                   after_stat(rr.label), sep = "*\", \"*"))) +
    # geom_smooth(formula = y ~ x, method = "lm") +
    geom_point() + 
    theme_scoringutils() + 
    scale_x_continuous(trans = "log10") +
    labs(y = "Brier Score", x = "Number of forecasters")
}


# ============================================================================ #
# Analysis
# ============================================================================ #

## Aggregate level analysis ----------------------------------------------------
binary |>
  filter_users(n = 0) |>
  regression() |>
  (\(x) {
    ggsave(filename = "output/figures/scatter-all-questions.png", plot = x, 
           height = 4, width = 7)
  })()


binary_charles |>
  filter_users(n = 0) |>
  regression() |>
  (\(x) {
    ggsave(filename = "output/figures/scatter-all-questions-charles.png", plot = x, 
           height = 4, width = 7)
  })()



## regression when you restrict everything to questions with at least 100 
## predictions
binary_charles_100plus <- binary_charles |>
  filter_users(n = 100) 

p1 <- regression(binary_charles_100plus)
p2 <- binary |>
  filter(question_id %in% unique(binary_charles_100plus$question_id)) |>
  filter_users(n = 0) |>
  regression()

(p1 + p2) |>
  (\(x) {
    ggsave(filename = "output/figures/scatter-100plus-questions.png", plot = x, 
           height = 3.5, width = 7)
  })()

p2 + 
  scale_y_log10()



## Individual level analysis ---------------------------------------------------

out_charles <- fread("output/data/charles/all_sims_cp.csv")


p1 <- out_charles |>
  group_by(question_id) |>
  mutate(first = mean[1]) |>
  mutate(mean = mean - first) |>
  ggplot(aes(x = n_users)) +
  geom_line(aes(y = mean, group = question_id), 
            alpha = 0.2, linewidth = 0.2) + 
  theme_scoringutils() + 
  labs(y = "Abs. change in Brier score", x = "Hypothetical users") 

label_perc <- function(x) {
  paste0(100 * x, "%")
}

p2 <- out_charles |>
  filter_low_bs(bs_thresh = 0.01) |>
  group_by(question_id) |>
  mutate(first = mean[1]) |>
  mutate(mean = mean / first) |>
  ggplot(aes(x = n_users)) +
  geom_line(aes(y = mean, group = question_id), 
            alpha = 0.2, linewidth = 0.2) + 
  theme_scoringutils() + 
  scale_x_continuous(trans = "log10", breaks = c(10, 30, 50, 100, 150)) +  
  labs(y = "Rel. change in Brier score", x = "Hypothetical users (log scale)")


(p1 / p2) |>
  (\(x) {
    ggsave(filename = "output/figures/scatter-questions-spaghetti-charles.png", plot = x, 
           height = 5, width = 7)
  })()



p1 <- out_charles |>
  group_by(question_id) |>
  mutate(normalised_bs = mean) |>
  ungroup() |>
  group_by(n_users) |>
  summarise(improvement = mean(normalised_bs)) |>
  ggplot(aes(x = n_users)) +
  geom_line(aes(y = improvement)) +
  geom_point(aes(y = improvement)) +
  theme_scoringutils() + 
  labs(y = "Average Brier Score", x = "Hypothetical users") 

p2 <- out_charles |>
  group_by(question_id) |>
  filter_low_bs(bs_thresh = 0.01) |>
  mutate(first = mean[1]) |>
  mutate(normalised_bs = mean / first) |>
  ungroup() |>
  group_by(n_users) |>
  summarise(improvement = mean(normalised_bs)) |>
  ggplot(aes(x = n_users)) +
  geom_line(aes(y = improvement)) +
  geom_point(aes(y = improvement)) +
  theme_scoringutils() + 
  scale_x_continuous(trans = "log10", breaks = c(10, 30, 50, 100, 150)) +  
  scale_y_continuous(breaks = c(0.7, 0.8, 0.9, 1), labels = label_perc) + 
  expand_limits(y = c(0.75, 1)) + 
  labs(y = "Average change in Brier score", x = "Hypothetical users (log scale)") 

(p1 / p2) |>
  (\(x) {
    ggsave(filename = "output/figures/average-bs-charles.png", plot = x, 
           height = 5, width = 7)
  })()



p <- out_charles |>
  group_by(question_id) |>
  filter_low_bs(bs_thresh = 0.01) |>
  mutate(first = var[1]) |>
  mutate(var = var / first) |>
  ggplot(aes(x = n_users)) +
  geom_line(aes(y = var, group = question_id), 
            alpha = 0.2, linewidth = 0.2) + 
  theme_scoringutils() + 
  scale_x_log10() + 
  labs(y = "Rel. change in variance of Brier score", x = "Hypothetical users (log scale)")

ggsave(filename = "output/figures/variance-bs-charles.png", plot = p, 
       height = 3.5, width = 7)
  

p1 <- binary_charles |>
  filter_users(n = 150) |>
  group_by(question_id) |>
  mutate(bs = (cp - resolution)^2) |>
  mutate(forecast_id = 1:n()) |>
  # filter(forecast_id > 50) |>
  mutate(bs = bs - bs[1]) |>
  mutate(t_total = (max(t) - min(t)) / (60 * 60 * 24), 
         t_rel = (t - min(t)) / (60 * 60 * 24), 
         t_perc = t_rel / t_total) |>
  # mutate(mean_cp)
  ggplot(aes(x = t_perc)) +
  geom_line(aes(y = bs, group = question_id), 
            alpha = 0.2, linewidth = 0.2) + 
  theme_scoringutils() + 
  scale_x_continuous(labels = label_perc) + 
  labs(y = "Abs. change in Brier score", x = "Percentage of time passed")


get_n_forecasters <- function(ids) {
  n <- length(ids)
  out <- numeric(n)
  for (i in 1:length(ids)) {
    out[i] <- length(unique(ids[1:i]))
  }
  return(out)
}

p2 <- binary_charles |>
  filter_users(n = 150) |>
  group_by(question_id) |>
  mutate(n_forecasters = get_n_forecasters(user_id)) |>
  mutate(bs = (cp - resolution)^2) |>
  mutate(bs = bs - bs[1]) |>
  ggplot(aes(x = n_forecasters)) +
  geom_line(aes(y = bs, group = question_id), 
            alpha = 0.2, linewidth = 0.2) + 
  theme_scoringutils() + 
  labs(y = "Abs. change in Brier score", x = "Number of forecasters")

p <- p1 / p2

ggsave(filename = "output/figures/bs-time-and-numbers-charles.png", plot = p, 
       height = 5, width = 7)

## Time analysis
times <- binary_charles |>
  group_by(question_id) |>
  mutate(min_t = min(t), 
         end_t = max(t)) |>
  select(question_id, min_t, end_t) |>
  unique() |>
  ungroup()

p <- out_charles |> 
  inner_join(times) |>
  mutate(total_t = end_t - min_t, 
         rel_t = max_t - min_t, 
         perc_t = rel_t / total_t) |>
  group_by(n_users) |>
  mutate(perc_t_mean = mean(perc_t)) |>
  ggplot(aes(y = perc_t, x = n_users)) + 
  geom_line(aes(y = perc_t, group = question_id), 
            alpha = 0.2, linewidth = 0.2) + 
  geom_line(aes(y = perc_t_mean)) + 
  scale_x_log10() + 
  scale_y_continuous(labels = label_perc) +
  theme_scoringutils() + 
  labs(y = "Latest included forecast as % of overall time", x = "Hypothetical users")

ggsave(filename = "output/figures/time-sims-charles.png", plot = p, 
       height = 3.5, width = 7)


# # Follow up: 
# - what features make ensembles better or worse? 
# - Are they better if they are very different? 


  
