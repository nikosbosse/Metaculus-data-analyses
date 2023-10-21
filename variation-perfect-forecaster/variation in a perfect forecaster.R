library(dplyr)
library(ggplot2)
library(data.table)

set.seed(123)

theme_set(
  theme_minimal() %+replace%
    theme(axis.line = element_line(colour = "grey80"),
          axis.ticks = element_line(colour = "grey80"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom")
)
forecast_with_noise <- function(true_prob, noise) {
  # add noise in log-odds space
  odds <- true_prob / (1 - true_prob)
  log_odds <- log(odds)
  noisy_log_odds <- log_odds + rnorm(n = length(log_odds), mean = 0, sd = noise)
  noisy_odds <- exp(noisy_log_odds)
  noisy_prob <- noisy_odds / (1 + noisy_odds)
  return(noisy_prob)
}
brier_score <- function(obs, pred) {(pred - obs)^2}
log_score <- function(obs, pred) {ifelse(obs == 1, -log(pred), -log(1 - pred))}





############################################
# multiple true probabilities, many outcomes
############################################
n_obs_grid <- c(5, 10, 20, 50, 100, 200, 500, 1000)
rep_many_df <- readRDS(file = "comparing-two-forecasters/replications_df.rds") |>
  mutate(expected_bs_h0 = prediction - prediction^2)
rep_many_list <- split(rep_many_df, rep_many_df$id)

get_pvals <- function(rep_many_df, score = "brier_score", test_fun = t.test) {
  lapply(n_obs_grid, function(n_obs, test_fun) {
    rep_many_df |>
      group_by(id, noise) |>
      filter(obs_id <= n_obs) |>
      summarise(p_value = test_fun(brier_score, 
                                   expected_bs_h0, paired = TRUE)$p.value, 
                n_obs = n_obs, 
                .groups = "drop_last")
  }, test_fun = test_fun) |>
    rbindlist()
}

pvals_bs <- get_pvals(rep_many_df)

pvals_bs |> 
  group_by(n_obs, noise) |>
  summarise(proportion = mean(`p_value` < 0.05)) |>
  ggplot(aes(x = n_obs, y = proportion, group = noise, colour = noise)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = c(10, 50, 100, 200, 500, 1000)) +
  labs(y = "Proportion of replications with p <= 0.05", x = "Number of available independent predictions (sample size)")



rep_many_df |>
  group_by(noise) |>
  summarise(mean_exp = mean(expected_bs_h0), 
            mean_bs = mean(brier_score))




#####################################
# one true probability, many outcomes
#####################################
# similar to above, but with the main difference that the true probability 
# is fixed across replications

n_obs <- 1000
reps <- 1:1000
noises <- c(0, 0.1, 0.3, 0.5, 1)
true_prob <- runif(n = n_obs, min = 0, max = 1)
f0 <- forecast_with_noise(true_prob, noise = 0)
f0.1 <- forecast_with_noise(true_prob, noise = 0.1)
f0.3 <- forecast_with_noise(true_prob, noise = 0.3)
f0.5 <- forecast_with_noise(true_prob, noise = 0.5)
f1 <- forecast_with_noise(true_prob, noise = 1)

replications <- lapply(reps, function(x) {
  obs <- rbinom(n = n_obs, size = 1, p = true_prob)
  
  df <- data.frame(
    prediction = c(f0, f0.1, f0.3, f0.5, f1),
    model = rep(c("f0", "f0.1", "f0.3", "f0.5", "f1"), each = n_obs),
    obs = obs
    ) |>
    mutate(
      brier_score = brier_score(obs, prediction), 
      log_score = log_score(obs, prediction)
    ) 
  return(df)
})

replications_df <- rbindlist(replications, idcol = "id") |>
  mutate(expected_bs_h0 = prediction - prediction^2)

# plot of the distribution of the perfect forecaster
replications_df |>
  filter(model == "f0") |>
  group_by(id) |>
  summarise(mean_logs = mean(log_score), 
            mean_bs = mean(brier_score)) |>
  ggplot(aes(x = mean_bs)) + 
  geom_histogram(fill = "grey40", colour = "white") 


# compute bins of probability
bin_width = 1/6
binned <- replications_df |> 
  group_by(model) |>
  mutate(bin = cut(prediction, breaks = seq(0, 1, by = bin_width), 
                   include.lowest = TRUE)) 

# plot distribution of brier scores for bins
binned |> 
  filter(model == "f1") |>
  group_by(id) |>
  slice_head(n = 1000) |>
  group_by(id, bin) |>
  summarise(mean_logs = mean(log_score), 
            mean_bs = mean(brier_score)) |>
  ggplot(aes(x = mean_bs)) + 
  
  expand_limits(x = c(0, 0.25)) + 
  facet_wrap(~ bin, scales = "free", nrow = 2)


replications_df |>
  filter(model == "f0", id == 1) |>
  ggplot(aes(x = expected_bs_h0)) + 
  geom_histogram(fill = "grey40", colour = "white")



