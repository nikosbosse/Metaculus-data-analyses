# script with experiments

library(dplyr)
library(ggplot2)
library(data.table)
library(scales)
library(patchwork)
library(tidyr)

set.seed(123)

label_double <- function(x) {
  paste((x * 100)/100)
}

theme_set(
  theme_minimal() %+replace%
    theme(legend.position = "bottom", 
          axis.line = element_line(colour = "grey80"),
          axis.ticks = element_line(colour = "grey80"), 
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey80", linewidth = 0.08), #changed this
          panel.border = element_rect(colour = "grey80", fill = NA), # changed this
          panel.background = element_blank())
)

prob_to_odds <- function(p) {
  p / (1 - p)
}
odds_to_prob <- function(odds) {
  odds / (1 + odds)
}

forecast_with_noise <- function(true_prob, noise, confidence_bias = 1) {
  # add noise in log-odds space
  odds <- true_prob / (1 - true_prob)
  log_odds <- log(odds)
  noisy_log_odds <- log_odds + rnorm(n = length(log_odds), mean = 0, sd = noise)
  noisy_log_odds <- noisy_log_odds * confidence_bias
  noisy_odds <- exp(noisy_log_odds)
  noisy_prob <- noisy_odds / (1 + noisy_odds)
  return(noisy_prob)
}
brier_score <- function(obs, pred) {(pred - obs)^2}
log_score <- function(obs, pred) {ifelse(obs == 1, -log(pred), -log(1 - pred))}
actual_expected_score <- function(true_prob, predicted_prob) {
  true_prob * (1 - predicted_prob)^2 + (1 - true_prob) * predicted_prob^2
}
subjective_expected <- function(predicted_prob) {
  predicted_prob - predicted_prob^2
}

# bin data to create a calibration plot 
bin_data <- function(df, group_by) {
  df |>
    group_by(.data[[group_by]]) |>
    mutate(bin = cut(prediction, breaks = seq(0, 1, length.out = num_bins + 1), include.lowest = TRUE, labels = seq(0, 1 - 1/num_bins, by = 1/num_bins))) %>%
    group_by(bin, .data[[group_by]]) %>%
    summarise(
      avg_pred_prob = mean(prediction),  # Average predicted probability
      actual_success_rate = mean(obs),   # Actual success rate
      count = n(),                        # Count per bin
      mean_brier = mean(brier_score), 
      mean_subjective = mean(subjective_bs)
    ) %>%
    ungroup()
}



# calibration plot
round_sig <- function(x, round_fun = ceiling, digits = 1) {
  rounding_factor <- 10^(digits - ceiling(log10(abs(x))))
  rounded_value <- round_fun(x * rounding_factor) / rounding_factor
  return(rounded_value)
}

calibration_plot <- function(data_binned) {
  size_limits <- c(
    2e4,
    18e4
  )
  ggplot(data_binned, aes(x = avg_pred_prob, y = actual_success_rate)) +
    geom_point(aes(size = count), alpha = 0.6) +  # Plot points, size by count
    geom_line(aes(group = 1), color = "blue") +   # Connect points with lines
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # 45-degree line
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + 
    scale_size_continuous(name = "size", breaks = c(4e4, 6e4, 8e4, 10e4, 12e4, 14e4, 16e4, 18e4), limits = size_limits) +
    labs(
      x = "Average Predicted Probability",
      y = "Actual Success Rate",
      size = "Count per Bin"
    ) + 
    guides(size = guide_legend(nrow = 1)) + 
    theme(panel.grid.major = element_line(colour = "grey", linewidth = 0.04))
}

# calculate mean scores for use in plotting
calc_mean_scores <- function(df) {
  df |> 
    summarise(brier_score = mean(brier_score, na.rm = TRUE), 
              subjective = mean(subjective_bs, na.rm = TRUE))
}

# plot the histogram of actual and subjective scores
plot_behaviour_hist <- function(plot_df, mean_scores) {
  plot_df |> 
    ggplot(aes(y = value, fill = score)) + 
    geom_histogram(aes(x = after_stat(width * density)), 
                   alpha=0.7, position="identity", colour = "white") + 
    labs(y = "Average Brier score", x = "Proportion") + 
    geom_text(aes(x = 0.45, y = 0.218, 
                  label = paste("Avg. actual BS:", round(brier_score, 3))),
              size = 3, 
              data = mean_scores, inherit.aes = FALSE, hjust = 1) + 
    geom_text(aes(x = 0.45, y = 0.213, 
                  label = paste("Avg. subjective BS:", round(subjective, 3))),
              size = 3, 
              data = mean_scores, inherit.aes = FALSE, hjust = 1) + 
    geom_hline(aes(yintercept = brier_score, colour = "Actual Brier score"), 
               data = mean_scores) +
    geom_hline(aes(yintercept = subjective, colour = "Subjective Brier score"), 
               data = mean_scores)
}

# create the plotting data.frame needed for the above function
create_long_df <- function(df) {
  df |>
    rename("Actual Brier score" = brier_score, "Subjective expected Brier score" = subjective_bs) |>
    pivot_longer(cols = c(`Actual Brier score`, `Subjective expected Brier score`), 
                 values_to = "value", names_to = "score") 
}
create_hist_plot_df <- function(df, group_by) {
  long <- create_long_df(df)
  plot_df <- long |> 
    group_by(score, id, .data[[group_by]]) |>
    summarise(value = mean(value))
}


add_difficulty <- function(df) {
  df |>
    mutate(
      difficulty = case_when(
        true_prob < 1/6 | true_prob > 5/6 ~ "easy",
        true_prob < 2/6 | true_prob > 4/6 ~ "medium", 
        .default = "hard"
      ), 
      difficulty = factor(difficulty, levels = c("easy", "medium", "hard"))
    )
}


plot_histogram_predictions <- function(df) {
  df |>
    ggplot(aes(x = prediction)) + 
    geom_histogram(aes(y = after_stat(density * width)), 
                   alpha=0.7, position="identity", colour = "white", 
                   breaks = seq(0, 1, by = 1/10)) + 
    scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) + 
    expand_limits(y = c(0, 0.2)) + 
    labs(y = "Proportion", x = "Predicted probabilities") + 
    theme(panel.grid.major = element_line(colour = "grey", linewidth = 0.04))
}








###############################################
# behaviour of the "subjective expected score" - simulation
###############################################
n_obs <- 1000
reps <- 1:1000
confidences <- c(0.6, 0.85, 1, 1.15, 1.4)

rep_confidence <- lapply(reps, function(x) {
  true_prob <- runif(n = n_obs, min = 0, max = 1)
  obs <- rbinom(n = n_obs, size = 1, p = true_prob)
  
  forecasts <- sapply(confidences, function(x) {
    forecast_with_noise(true_prob, noise = 0, confidence_bias = x)
  }) |>
    as.numeric()
  
  df <- data.frame(
    obs_id = 1:n_obs,
    prediction = forecasts,
    obs = obs, 
    true_prob = true_prob, 
    confidence = rep(confidences, each = n_obs)
  ) |>
    mutate(
      brier_score = brier_score(obs, prediction), 
      log_score = log_score(obs, prediction),
      true_log_odds = log(true_prob / (1 - true_prob)), 
      log_odds = log(prediction / (1 - prediction)), 
      subjective_bs = prediction - prediction^2
    ) 
  return(df)
})
rep_confidence_df <- rbindlist(rep_confidence, idcol = "id") |>
  add_difficulty() |>
  mutate(confidence = paste("Confidence:", confidence))

# create plotting data.frames
plot_df_confidence <- create_hist_plot_df(rep_confidence_df, group_by = "confidence")
  
mean_scores_confidence <- rep_confidence_df |> 
  group_by(confidence) |>
  calc_mean_scores()

plot_df_confidence |>
  plot_behaviour_hist(mean_scores_confidence) + 
  facet_wrap(~ confidence, nrow = 1) 
ggsave("variation-perfect-forecaster/behaviour-confidence.jpg", 
       width = 10, height = 5)

num_bins <- 10
binned_data_confidence <- rep_confidence_df |>
  bin_data(group_by = "confidence")

calibration_plot(binned_data_confidence) + 
  facet_wrap(~ confidence, nrow = 1) + 
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) 
ggsave("variation-perfect-forecaster/calibration-confidence.jpg", 
       width = 10, height = 3)

binned_data_confidence |>
  rename("Actual Brier score" = mean_brier, "Subjective expected Brier score" = mean_subjective) |>
  pivot_longer(cols = c(`Actual Brier score`, `Subjective expected Brier score`), 
               values_to = "value", names_to = "score") |>
  ggplot(aes(y = value, x = avg_pred_prob, colour = score)) + 
  geom_point(aes(size = count), alpha = 0.6) +  # Plot points, size by count 
  geom_line() + 
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) + 
  facet_wrap(~ confidence, nrow = 1)
ggsave("variation-perfect-forecaster/comparison-confidence.jpg", width = 8, height = 3)

rep_confidence_df |>
  plot_histogram_predictions() + 
  facet_wrap(~ confidence, nrow = 1) 
ggsave("variation-perfect-forecaster/hist-predictions-confidence.jpg", 
       width = 10, height = 2)


## make version of the first plot with really extreme over/underconfidence
confidences_extreme <- c(0.02, 0.1, 1, 10, 50)

rep_conf_extr <- lapply(reps, function(x) {
  true_prob <- runif(n = n_obs, min = 0, max = 1)
  obs <- rbinom(n = n_obs, size = 1, prob = true_prob)
  
  forecasts <- sapply(confidences_extreme, function(x) {
    forecast_with_noise(true_prob, noise = 0, confidence_bias = x)
  }) |>
    as.numeric()
  
  df <- data.frame(
    obs_id = 1:n_obs,
    prediction = forecasts,
    obs = obs, 
    true_prob = true_prob, 
    confidence = rep(confidences_extreme, each = n_obs)
  ) |>
    mutate(
      brier_score = brier_score(obs, prediction), 
      log_score = log_score(obs, prediction),
      true_log_odds = log(true_prob / (1 - true_prob)), 
      log_odds = log(prediction / (1 - prediction)), 
      subjective_bs = prediction - prediction^2
    ) 
  return(df)
})
rep_conf_extreme_df <- rbindlist(rep_conf_extr, idcol = "id") |>
  add_difficulty() |>
  mutate(confidence = paste("Confidence:", confidence))

plot_df_conf_extreme <- create_hist_plot_df(rep_conf_extreme_df, group_by = "confidence")

mean_scores_conf_extreme <- rep_conf_extreme_df |> 
  mutate(brier_score = ifelse(is.finite(brier_score), brier_score, 0)) |>
  group_by(confidence) |>
  calc_mean_scores()

plot_df_conf_extreme |>
  ggplot(aes(y = value, fill = score)) + 
  geom_histogram(aes(x = after_stat(width * density)), 
                 alpha=0.7, position="identity", colour = "white") + 
  labs(y = "Average Brier score", x = "Proportion") + 
  geom_text(aes(x = 0.85, y = 0.33, 
                label = paste("Avg. actual BS:", round(brier_score, 3))),
            size = 3, 
            data = mean_scores_conf_extreme, inherit.aes = FALSE, hjust = 1) + 
  geom_text(aes(x = 0.85, y = 0.29, 
                label = paste("Avg. subjective BS:", round(subjective, 3))),
            size = 3, 
            data = mean_scores_conf_extreme, inherit.aes = FALSE, hjust = 1) + 
  geom_hline(aes(yintercept = brier_score, colour = "Actual Brier score"), 
             data = mean_scores_conf_extreme) +
  geom_hline(aes(yintercept = subjective, colour = "Subjective Brier score"), 
             data = mean_scores_conf_extreme) +
  facet_wrap(~ confidence, nrow = 1) 
ggsave("variation-perfect-forecaster/behaviour-conf-extreme.jpg", 
       width = 10, height = 4)

############################################
# Now we add noise instead of over/ underconfidence
############################################
n_obs <- 1000
reps <- 1:1000
noises <- c(0, 0.25, 0.5, 0.75, 1)

rep_noise <- lapply(reps, function(x) {
  true_prob <- runif(n = n_obs, min = 0, max = 1)
  obs <- rbinom(n = n_obs, size = 1, p = true_prob)
  
  forecasts <- sapply(noises, function(x) {
    forecast_with_noise(true_prob, noise = x)
  }) |>
    as.numeric()
  
  df <- data.frame(
    obs_id = 1:n_obs,
    prediction = forecasts,
    obs = obs, 
    true_prob = true_prob, 
    noise = rep(noises, each = n_obs)
  ) |>
    mutate(
      brier_score = brier_score(obs, prediction), 
      log_score = log_score(obs, prediction),
      true_log_odds = log(true_prob / (1 - true_prob)), 
      log_odds = log(prediction / (1 - prediction)), 
      subjective_bs = prediction - prediction^2
    ) 
  return(df)
})
rep_noise_df <- rbindlist(rep_noise, idcol = "id") |>
  add_difficulty() |>
  mutate(noise = paste("Noise:", noise))

plot_df_noise <- create_hist_plot_df(rep_noise_df, group_by = "noise")

mean_scores_noise <- rep_noise_df |> 
  group_by(noise) |>
  calc_mean_scores()

plot_df_noise |>
  plot_behaviour_hist(mean_scores_noise) + 
  facet_wrap(~ noise, nrow = 1)
ggsave("variation-perfect-forecaster/behaviour-noise.jpg", 
       width = 10, height = 5)

num_bins <- 10
binned_data_noise <- rep_noise_df |>
  bin_data(group_by = "noise")

calibration_plot(binned_data_noise) + 
  facet_wrap(~ noise, nrow = 1) + 
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2))
ggsave("variation-perfect-forecaster/calibration-noise.jpg", 
       width = 10, height = 3)

binned_data_noise |>
  rename("Actual Brier score" = mean_brier, "Subjective expected Brier score" = mean_subjective) |>
  pivot_longer(cols = c(`Actual Brier score`, `Subjective expected Brier score`), 
               values_to = "value", names_to = "score") |>
  ggplot(aes(y = value, x = avg_pred_prob, colour = score)) + 
  geom_point(aes(size = count), alpha = 0.6) +  # Plot points, size by count 
  geom_line() + 
  facet_wrap(~ noise, nrow = 1)

rep_noise_df |>
  plot_histogram_predictions() + 
  facet_wrap(~ noise, nrow = 1) 
ggsave("variation-perfect-forecaster/hist-predictions-noise.jpg", 
       width = 10, height = 2)



# small test to check what happens if you just add / subtract on the 
# actual probabilities
test <- rep_noise_df[noise == 0]
test[, factor := sample(c(-0.05, 0.05), size = nrow(test), replace = TRUE)]
test[prediction > 0.56 & prediction < 0.9, prediction := prediction + factor]

test[, .(mean_old = mean(subjective_bs), 
         mean_new = mean(prediction - prediction^2))]

calibration_plot(bin_data(test, group_by = "noise"))


##################################################
# combine underconfidence and noise
##################################################

n_obs <- 1000
reps <- 1:1000
noises <- c(0, 0.25, 0.5, 0.75, 1)

rep_underconfident_noise <- lapply(reps, function(x) {
  true_prob <- runif(n = n_obs, min = 0, max = 1)
  obs <- rbinom(n = n_obs, size = 1, p = true_prob)
  
  forecasts <- sapply(noises, function(x) {
    forecast_with_noise(true_prob, noise = x, confidence_bias = 1 - x^2 * 0.15)
  }) |>
    as.numeric()
  
  df <- data.frame(
    obs_id = 1:n_obs,
    prediction = forecasts,
    obs = obs, 
    true_prob = true_prob, 
    noise = rep(noises, each = n_obs),
    confidence = rep(1 - noises^2 * 0.15, each = n_obs)
  ) |>
    mutate(
      brier_score = brier_score(obs, prediction), 
      log_score = log_score(obs, prediction),
      true_log_odds = log(true_prob / (1 - true_prob)), 
      log_odds = log(prediction / (1 - prediction)), 
      subjective_bs = prediction - prediction^2
    ) 
  return(df)
})

rep_underconfident_noise_df <- rbindlist(rep_underconfident_noise, idcol = "id") |>
  add_difficulty() |>
  mutate(confidence = paste("Confidence:", confidence)) |>
  mutate(noise = paste("Noise:", noise))

plot_df_underconfident_noise <- create_hist_plot_df(rep_underconfident_noise_df, group_by = "noise")

mean_scores_underconfident_noise <- rep_underconfident_noise_df |>
  group_by(noise) |>
  calc_mean_scores()

plot_df_underconfident_noise |>
  plot_behaviour_hist(mean_scores_underconfident_noise) + 
  facet_wrap(~ noise, nrow = 1)
ggsave("variation-perfect-forecaster/behaviour-underconfident-noise.jpg", 
       width = 10, height = 5)

num_bins <- 10
binned_data_underconfident_noise <- rep_underconfident_noise_df |>
  bin_data(group_by = "noise")

calibration_plot(binned_data_underconfident_noise) + 
  facet_wrap(~ noise, nrow = 1) + 
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2))
ggsave("variation-perfect-forecaster/calibration-underconfident-noise.jpg", 
       width = 10, height = 3)


rep_underconfident_noise_df |>
  plot_histogram_predictions() + 
  facet_wrap(~ noise, nrow = 1) 
ggsave("variation-perfect-forecaster/hist-predictions-underconfident-noise.jpg", 
       width = 10, height = 5)






##################################################
# now let's stratify this by true probability
##################################################

plot_df_confidence_strat <- rep_confidence_df |>
  create_long_df() |>
  group_by(score, id, confidence, difficulty) |>
  summarise(value = mean(value))

mean_scores_confidence_strat <- rep_confidence_df |>
  group_by(confidence, difficulty) |>
  calc_mean_scores()

plot_df_confidence_strat |>
  ggplot(aes(y = value, fill = score)) + 
  geom_histogram(aes(x = after_stat(width * density)), 
                 alpha=0.7, position="identity", colour = "white", 
                 linewidth = 0.02,
                 binwidth = 0.008) + 
  labs(y = "Average Brier score", x = "Proportion") + 
  geom_text(aes(x = 1, y = 0.3, 
                label = paste("Avg. actual BS:", round(brier_score, 3))),
            size = 2.5, 
            data = mean_scores_confidence_strat, inherit.aes = FALSE, hjust = 1) + 
  geom_text(aes(x = 1, y = 0.27, 
                label = paste("Avg. subjective BS:", round(subjective, 3))),
            size = 2.5, 
            data = mean_scores_confidence_strat, inherit.aes = FALSE, hjust = 1) +
  scale_x_continuous(labels = label_double) + 
  facet_grid(difficulty ~ confidence)

ggsave("variation-perfect-forecaster/behaviour-confidence-strat2.jpg", 
       width = 8.5, height = 7)



plot_df_noise_strat <- rep_noise_df |>
  create_long_df() |>
  group_by(score, id, noise, difficulty) |>
  summarise(value = mean(value))

mean_scores_noise_strat <- rep_noise_df |>
  group_by(noise, difficulty) |>
  calc_mean_scores()

plot_df_noise_strat |>
  ggplot(aes(y = value, fill = score)) + 
  geom_histogram(aes(x = after_stat(width * density)), 
                 alpha=0.7, position="identity", colour = "white", 
                 linewidth = 0.02,
                 binwidth = 0.008) + 
  labs(y = "Average Brier score", x = "Proportion") + 
  geom_text(aes(x = 1, y = 0.36, 
                label = paste("Avg. actual BS:", round(brier_score, 3))),
            size = 2.5, 
            data = mean_scores_noise_strat, inherit.aes = FALSE, hjust = 1) + 
  geom_text(aes(x = 1, y = 0.33, 
                label = paste("Avg. subjective BS:", round(subjective, 3))),
            size = 2.5, 
            data = mean_scores_noise_strat, inherit.aes = FALSE, hjust = 1) +
  facet_grid(difficulty ~ noise) + 
  geom_hline(aes(yintercept = brier_score, colour = "Actual Brier score"), 
             data = mean_scores_noise_strat) +
  geom_hline(aes(yintercept = subjective, colour = "Subjective Brier score"), 
             data = mean_scores_noise_strat) +
  scale_x_continuous(labels = label_double)

ggsave("variation-perfect-forecaster/behaviour-noise-strat2.jpg", 
       width = 8.5, height = 7)



plot_df_underconfident_noise_strat <- rep_underconfident_noise_df |>
  create_long_df() |>
  group_by(score, id, noise, difficulty) |>
  summarise(value = mean(value))

mean_scores_underconfident_noise_strat <- rep_underconfident_noise_df |>
  group_by(noise, difficulty) |>
  calc_mean_scores()

plot_df_underconfident_noise_strat |>
  ggplot(aes(y = value, fill = score)) + 
  geom_histogram(aes(x = after_stat(width * density)), 
                 alpha=0.7, position="identity", colour = "white", 
                 linewidth = 0.02,
                 binwidth = 0.008) + 
  labs(y = "Average Brier score", x = "Proportion") + 
  geom_text(aes(x = 1, y = 0.34, 
                label = paste("Avg. actual BS:", round(brier_score, 3))),
            size = 2.5, 
            data = mean_scores_underconfident_noise_strat, inherit.aes = FALSE, hjust = 1) + 
  geom_text(aes(x = 1, y = 0.31, 
                label = paste("Avg. subjective BS:", round(subjective, 3))),
            size = 2.5, 
            data = mean_scores_underconfident_noise_strat, inherit.aes = FALSE, hjust = 1) +
  geom_hline(aes(yintercept = brier_score, colour = "Actual Brier score"), 
             data = mean_scores_underconfident_noise_strat) +
  geom_hline(aes(yintercept = subjective, colour = "Subjective Brier score"), 
             data = mean_scores_underconfident_noise_strat) +
  facet_grid(difficulty ~ noise) +
  scale_x_continuous(labels = label_double)

ggsave("variation-perfect-forecaster/behaviour-underconfident-noise-strat2.jpg", 
       width = 8.5, height = 7)





















############################################
# multiple true probabilities, many outcomes
############################################
n_obs_grid <- c(5, 10, 20, 50, 100, 200, 500, 1000)
rep_many_df <- readRDS(file = "comparing-two-forecasters/replications_df.rds") |>
  mutate(expected_bs_h0 = prediction - prediction^2)
rep_many_list <- split(rep_many_df, rep_many_df$id)

# expected vs. actual scores
rep_long <- rep_many_df |>
  rename("Actual Brier score" = brier_score, "Subjective expected Brier score" = expected_bs_h0) |>
  pivot_longer(cols = c(`Actual Brier score`, `Subjective expected Brier score`), 
               values_to = "value", names_to = "score") 
plot_df <- rep_long |> 
  group_by(score, id, noise) |>
  summarise(value = mean(value))

mean_scores <- rep_many_df |> 
  filter(noise %in% c(0, 0.1, 0.3, 0.5, 0.75, 1)) |>
  group_by(noise) |>
  summarise(brier_score = mean(brier_score), 
            subjective = mean(prediction - prediction^2))

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
                label = paste("Avg. subjective BS:", round(subjective, 3))),
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
  rename("Actual Brier score" = brier_score, "Subjective expected Brier score" = expected_bs_h0) |>
  pivot_longer(cols = c(`Actual Brier score`, `Subjective expected Brier score`), 
               values_to = "value", names_to = "score") 
plot_df <- rep_bias_long |> 
  group_by(score, id, noise) |>
  summarise(value = mean(value))


mean_scores_bias <- replications_bias_df |> 
  group_by(noise) |>
  summarise(brier_score = mean(brier_score), 
            subjective = mean(expected_bs_h0))

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
                label = paste("Avg. subjective BS:", round(subjective, 3))),
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

