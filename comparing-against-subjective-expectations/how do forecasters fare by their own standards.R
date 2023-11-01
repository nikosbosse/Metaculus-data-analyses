# script related to the actual written piece

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
# behaviour of the "subjective expected score" in isolation
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





