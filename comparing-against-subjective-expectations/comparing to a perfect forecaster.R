library(dplyr)
library(ggplot2)
library(data.table)
library(scales)

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


# illustration of the expected score of a naive and ideal forecaster
grid <- seq(0, 1, 0.01)
n <- length(grid)
exp_bs_ideal <- grid - grid^2
exp_bs_naive <- rep(0.25, n)

data.frame(
  p = grid, 
  brier = c(exp_bs_ideal, exp_bs_naive), 
  forecaster = rep(c("ideal", "naive"), each = n)
) |>
  ggplot(aes(y = brier, x = p)) + 
  geom_line() + 
  facet_wrap(~forecaster) + 
  labs(y = "Brier score", x = "True probability of the predicted event")

odds <- function(p) {
  p / (1 - p)
}

forecast_with_noise <- function(true_prob, noise, underconfidence = 0) {
  # add noise in log-odds space
  odds <- true_prob / (1 - true_prob)
  log_odds <- log(odds)
  noisy_log_odds <- log_odds + rnorm(n = length(log_odds), mean = 0, sd = noise)
  noisy_log_odds <- underconfidence * 0 + (1 - underconfidence) * noisy_log_odds
  noisy_odds <- exp(noisy_log_odds)
  noisy_prob <- noisy_odds / (1 + noisy_odds)
  return(noisy_prob)
}
brier_score <- function(obs, pred) {(pred - obs)^2}
log_score <- function(obs, pred) {ifelse(obs == 1, -log(pred), -log(1 - pred))}

#####################################
# rainfall example in Ithaca
#####################################

monthly_rain <- c(14, 15, 7, 6, 3, 3, 1, 1, 3, 8, 12, 15)
days_per_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
p1 <- rep(88/365, 365)
p2 <- rep(monthly_rain / days_per_month, times = days_per_month)
reps <- 1:1000

replications <- lapply(reps, function(x) {
  obs <- rbinom(n = 365, size = 1, p = p2)
  
  df <- data.frame(
    prediction = c(p1, p2),
    model = rep(c("Baseline", "More sophisticated"), each = 365),
    obs = obs
  ) |>
    mutate(
      brier_score = brier_score(obs, prediction), 
      log_score = log_score(obs, prediction)
    ) 
  return(df)
})

rain_df <- rbindlist(replications, idcol = "id")

rain_df |>
  group_by(model, id) |>
  summarise(mean_brier = mean(brier_score), 
            mean_logs = mean(log_score)) |>
  ggplot(aes(y = mean_brier)) + 
  geom_histogram(aes(x = after_stat(width * density)), fill = "grey40", colour = "white") + 
  facet_wrap(~ model, scales = "fixed") + 
  labs(y = "Average Brier score", x = "Proportion")


###############################################
# behaviour of the "idealised expected score"
###############################################

# - true probability 
# a) adding over- or underconfidence
# b) adding different levels of noise

actual_expected_score <- function(true_prob, predicted_prob) {
  true_prob * (1 - predicted_prob)^2 + (1 - true_prob) * predicted_prob^2
}

create_behaviour_plot <- function(true_prob = 0.8) {
  predicted <- c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
  actual <- actual_expected_score(true_prob, predicted)
  levels <- factor(c("Idealised expected score", "Actual expected score", "Difference"), 
                   levels = c(c("Idealised expected score", "Actual expected score", "Difference")))
  
  df <- data.frame(
    true_prob = true_prob, 
    predicted = predicted, 
    value = c(predicted - predicted^2, actual, actual - (predicted - predicted^2)),
    score = rep(levels, 
                each = length(predicted))
  )
  
  p <- df |> 
    ggplot(aes(y = value, x = predicted)) + 
    geom_point() + 
    geom_line() + 
    facet_wrap(~ score) + 
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    geom_hline(yintercept = 0.25, color = "grey70", linetype = "dashed") +
    geom_vline(xintercept = true_prob, linewidth = 0.5) + 
    labs(y = "Brier score", x = "Predicted probability") + 
    expand_limits(y = c(-0.2, 1))
  
  ggsave(filename = paste0("variation-perfect-forecaster/behaviour-gif/", true_prob, ".jpg"), 
         width = 7, height = 3.5)
}

grid <- seq(0.05, 0.95, 0.05)
lapply(grid, function(x) {create_behaviour_plot(x)})
library(magick)
imgs <- list.files("variation-perfect-forecaster/behaviour-gif/", full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 2)
## save to disk
image_write(image = img_animated,
            path = "variation-perfect-forecaster/behaviour.gif")



############################################
# multiple true probabilities, many outcomes
############################################
n_obs_grid <- c(5, 10, 20, 50, 100, 200, 500, 1000)
rep_many_df <- readRDS(file = "comparing-two-forecasters/replications_df.rds") |>
  mutate(expected_bs_h0 = prediction - prediction^2)
rep_many_list <- split(rep_many_df, rep_many_df$id)

# expected vs. actual scores
rep_long <- rep_many_df |>
  rename("Actual Brier score" = brier_score, "Idealised expected Brier score" = expected_bs_h0) |>
  pivot_longer(cols = c(`Actual Brier score`, `Idealised expected Brier score`), 
               values_to = "value", names_to = "score") 
plot_df <- rep_long |> 
  group_by(score, id, noise) |>
  summarise(value = mean(value))

mean_scores <- replications_df |> 
  filter(noise %in% c(0, 0.1, 0.3, 0.5, 0.75, 1)) |>
  group_by(noise) |>
  summarise(brier_score = mean(brier_score), 
            idealised = mean(prediction - prediction^2))

plot_df |> 
  ggplot(aes(y = value, fill = score)) + 
  geom_histogram(aes(x = after_stat(width * density)), 
                 alpha=0.7, position="identity", colour = "white") + 
  facet_wrap(~ noise) + 
  labs(y = "Average Brier score", x = "Proportion") + 
  geom_text(aes(x = 0.4, y = 0.218, 
                label = paste("Avg. actual BS:", round(brier_score, 3))),
            size = 3, 
            data = mean_scores, inherit.aes = FALSE, hjust = 1) + 
  geom_text(aes(x = 0.4, y = 0.21, 
                label = paste("Avg. idealised BS:", round(idealised, 3))),
            size = 3, 
            data = mean_scores, inherit.aes = FALSE, hjust = 1)

ggsave(filename = "variation-perfect-forecaster/score-distr-with-noise.jpg", 
       width = 7, height = 5)

rep_long |>
  filter(id == 1, noise %in% c(0, 0.5, 1)) |>
  ggplot(aes(y = prediction)) + 
  geom_histogram(aes(x = after_stat(width * density)), 
                 fill = "grey40", colour = "white", bins = 50) + 
  facet_wrap(~ noise) + 
  labs(y = "Prediction", x = "Proportion")

rep_long |>
  group_by(noise) |>
  summarise(mean_prediction = mean(prediction), 
            var_prediction = var(prediction))

# do a binned version of the behaviour plot
binned <- rep_long |> 
  group_by(model) |>
  mutate(bin = cut(prediction, breaks = c(0, 0.25, 0.75, 1), 
                   include.lowest = TRUE)) |>
  mutate(bin = ifelse(bin %in% c("[0,0.25]", "[0.75,0.1]"), "outer", "inner"))

plot_df <- binned |> 
  group_by(score, id, noise, bin) |>
  summarise(value = mean(value))

plot_df |> 
  mutate(score = paste(score, "-", bin)) |>
  ggplot(aes(y = value, fill = score)) + 
  geom_histogram(aes(x = after_stat(width * density)), 
                 alpha=0.7, position="identity", colour = "white") + 
  facet_wrap(~ noise) + 
  labs(y = "Brier score", x = "Proportion") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

ggsave(filename = "variation-perfect-forecaster/score-distr-with-noise-binned.jpg", 
       width = 7, height = 5)


# check calibration plots for noisy forecasters
num_bins <- 10

bin_data <- function(df, noises = c(0, 0.1, 0.3, 0.5, 0.6, 0.75, 1)) {
  df |>
    filter(noise %in% noises) |>
    group_by(noise) |>
    mutate(bin = cut(prediction, breaks = seq(0, 1, length.out = num_bins + 1), include.lowest = TRUE, labels = seq(0, 1 - 1/num_bins, by = 1/num_bins))) %>%
    group_by(bin, noise) %>%
    summarise(
      avg_pred_prob = mean(prediction),  # Average predicted probability
      actual_success_rate = mean(obs),   # Actual success rate
      count = n(),                        # Count per bin
    ) %>%
    ungroup()
}
calibration_plot <- function(data_binned) {
  ggplot(data_binned, aes(x = avg_pred_prob, y = actual_success_rate)) +
    geom_point(aes(size = count), alpha = 0.6) +  # Plot points, size by count
    geom_line(aes(group = 1), color = "blue") +   # Connect points with lines
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # 45-degree line
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    labs(
      title = "Calibration Plot",
      x = "Average Predicted Probability",
      y = "Actual Success Rate",
      size = "Count per Bin"
    ) + 
    facet_wrap(~ noise)
}

binned_data_all <- replications_df |> bin_data()
binned_data_1 <- replications_df |> filter(id == 1) |> bin_data()

calibration_plot(binned_data_all)
calibration_plot(binned_data_1)







# check the combination of underconfidence and noise
n_obs <- 1000
reps <- 1:1000
noises <- c(0, 0.3, 0.6, 1)

replications_bias <- lapply(reps, function(x) {
  underconfidence <- 0.2
  true_prob <- runif(n = n_obs, min = 0, max = 1)
  obs <- rbinom(n = n_obs, size = 1, p = true_prob)
  
  f0 <- forecast_with_noise(true_prob, noise = 0, underconfidence = underconfidence)
  f0.3 <- forecast_with_noise(true_prob, noise = 0.3, underconfidence = underconfidence)
  f0.6 <- forecast_with_noise(true_prob, noise = 0.6, underconfidence = underconfidence)
  f1 <- forecast_with_noise(true_prob, noise = 1, underconfidence = underconfidence)
  
  df <- data.frame(
    obs_id = 1:n_obs,
    prediction = c(f0, f0.3, f0.6, f1), 
    obs = obs, 
    true_prob = true_prob, 
    model = c(rep(c("f0", "f0.3", "f0.6", "f1"), each = n_obs)), 
    noise = c(rep(c("0", "0.3", "0.6", "1"), each = n_obs))
  ) |>
    mutate(
      brier_score = brier_score(obs, prediction), 
      log_score = log_score(obs, prediction),
      true_log_odds = log(true_prob / (1 - true_prob)), 
      log_odds = log(prediction / (1 - prediction))
    ) 
  return(df)
})

replications_bias_df <- rbindlist(replications_bias, idcol = "id") |>
  mutate(expected_bs_h0 = prediction - prediction^2)

rep_bias_long <- replications_bias_df |>
  rename("Actual Brier score" = brier_score, "Idealised expected Brier score" = expected_bs_h0) |>
  pivot_longer(cols = c(`Actual Brier score`, `Idealised expected Brier score`), 
               values_to = "value", names_to = "score") 
plot_df <- rep_bias_long |> 
  group_by(score, id, noise) |>
  summarise(value = mean(value))


mean_scores_bias <- replications_bias_df |> 
  group_by(noise) |>
  summarise(brier_score = mean(brier_score), 
            idealised = mean(expected_bs_h0))

plot_df |> 
  ggplot(aes(y = value, fill = score)) + 
  geom_histogram(aes(x = after_stat(width * density)), 
                 alpha=0.7, position="identity", colour = "white") + 
  facet_wrap(~ noise) + 
  labs(y = "Average Brier score", x = "Proportion") + 
  geom_text(aes(x = 0.4, y = 0.218, 
                label = paste("Avg. actual BS:", round(brier_score, 3))),
            size = 3, 
            data = mean_scores_bias, inherit.aes = FALSE, hjust = 1) + 
  geom_text(aes(x = 0.4, y = 0.21, 
                label = paste("Avg. idealised BS:", round(idealised, 3))),
            size = 3, 
            data = mean_scores_bias, inherit.aes = FALSE, hjust = 1)

ggsave(filename = "variation-perfect-forecaster/score-distr-with-underconfidence.jpg", 
       width = 7, height = 5)

replications_bias_df |>
  group_by(noise) |>
  summarise(mean = mean(prediction), 
            mean_score = mean(brier_score), 
            var = var(prediction), 
            mean_ideal = mean(expected_bs_h0))

rep_bias_long |>
  filter(id == 1, noise %in% c(0, 0.3, 0.6, 1)) |>
  ggplot(aes(y = prediction)) + 
  geom_histogram(aes(x = after_stat(width * density)), 
                 fill = "grey40", colour = "white", bins = 10) + 
  facet_wrap(~ noise) + 
  labs(y = "Prediction", x = "Proportion")



# calibration for biased data
biased_binned <- bin_data(replications_bias_df)
biased_binned_1 <- replications_bias_df |> filter(id == 1) |> bin_data()
calibration_plot(biased_binned) 



# significance tests
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
  scale_y_continuous(labels = percent_format()) +
  geom_histogram(aes(y = after_stat(width * density)), fill = "grey40", colour = "white") + 
  labs(y = "Proportion", x = "Average Brier score")
  







# compute bins of probability
bin_width = 1/6
binned <- replications_df |> 
  group_by(model) |>
  mutate(bin = cut(prediction, breaks = seq(0, 1, by = bin_width), 
                   include.lowest = TRUE)) 

# plot distribution of brier scores for bins
binned |> 
  filter(model == "f0") |>
  group_by(id, bin) |>
  summarise(mean_logs = mean(log_score), 
            mean_bs = mean(brier_score)) |>
  ggplot(aes(x = mean_bs)) + 
  geom_histogram(aes(y = after_stat(width * density)), fill = "grey40", colour = "white") + 
  expand_limits(x = c(0, 0.25)) +
  facet_wrap(~ bin, scales = "free", nrow = 2) +
  labs(y = "Proportion", x = "Average Brier score across 1000 questions")

# 
# replications_df |>
#   filter(model == "f0", id == 1) |>
#   ggplot(aes(x = expected_bs_h0)) + 
#   geom_histogram(fill = "grey40", colour = "white")

