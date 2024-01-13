## Example local vs. global scoring rule ---------------------------------------

library(ggplot2)
library(scoringutils)
library(data.table)
library(patchwork)

quantiles <- seq(0.1, 1, 0.1)
forecast_a <- c(0.01, 0.01, 0.01, 0.02, 0.03, 0.1, 0.13, 0.3, 0.22, 0.17)
forecast_b <- c(0.14, 0.22, 0.1, 0.02, 0.01, 0.01, 0.05, 0.3, 0.1, 0.05)
true_value <- 8

df <- data.table(
  forecaster = rep(c("Forecaster A", "Forecaster B"), each = 10),
  outcome = rep(1:10, 2),
  prob = c(forecast_a, forecast_b),
  true_value = true_value
)

df[, crps := sum((cumsum(prob) - (outcome >= true_value))^2),
   by = c("forecaster")]
df[, log_score := -log(prob[outcome == true_value]),
   by = c("forecaster")]
df[, mean_pred := sum(prob * outcome) / sum(prob),
   by = c("forecaster")]
df[, sd_pred := sqrt(sum((prob * outcome - mean_pred)^2)),
   by = c("forecaster")]
df[, log_score := -log(prob[outcome == true_value]),
   by = c("forecaster")]

# sense-check: compute crps using samples
sample_a <- sample(x=1:10, size = 1e5, replace = TRUE, prob = forecast_a)
sample_b <- sample(x=1:10, size = 1e5, replace = TRUE, prob = forecast_b)

crps_a <- scoringutils::crps_sample(2, t(as.matrix(sample_a)))
crps_b <- scoringutils::crps_sample(2, t(as.matrix(sample_b)))

annotation <- df[, .(forecaster, crps, log_score)] |> unique()


ggplot(df, aes(x = factor(outcome), y = prob)) +
  geom_col() +
  geom_text(data = annotation, x = 2, y = 0.28, hjust = "left", size = 3,
            aes(label = paste("CRPS: ", round(crps, 2)))) +
  geom_text(data = annotation,x = 2, y = 0.26, hjust = "left", size = 3,
            aes(label = paste("Log score: ", round(log_score, 2)))) +
  facet_wrap(~ forecaster) +
  geom_vline(aes(xintercept = true_value), linetype = "dashed") +
  theme_scoringutils() +
  labs(y = "Probability assigned", x = "Possible outcomes") + 
  theme(panel.background = element_rect())

ggsave("trade-offs-different-scoring-rules/score-locality.jpg", height = 3, width = 8)




## Example scores overconfidence --------------------------------------

label_double <- function(x) {
  paste((x * 100) / 100)
}

predicted <- seq(0.01, 0.99, 0.001)
log_score_1 <- -log(predicted)
log_score_2 <- -log(1 - predicted)
brier_score_1 <- (1 - predicted)^2
brier_score_2 <- (predicted)^2

df <- data.table(
  predicted = predicted,
  value = c(log_score_1, log_score_2, brier_score_1, brier_score_2),
  score = rep(c("Log score", "Brier score"), each = 2 * length(predicted)),
  outcome = rep(c(1, 0), each = length(predicted))
)

ggplot(df, aes(x = predicted, y = value)) +
  geom_line(aes(color = factor(outcome))) +
  theme_scoringutils() +
  labs(y = "Score", x = "Predicted probability") + 
  scale_color_discrete(name = "Outcome") +
  scale_x_continuous(labels = label_double) + 
  theme(panel.background = element_rect()) + 
  facet_wrap(~ score)

ggsave("trade-offs-different-scoring-rules/scores-overconfidence.jpg", height = 3, width = 8)

prob_to_odds <- function(x) {
  x / (1 - x)
}
predicted_odds <- prob_to_odds(predicted)

df <- data.table(
  odds = predicted_odds,
  value = c(log_score_1, log_score_2, brier_score_1, brier_score_2),
  score = rep(c("Log score", "Brier score"), each = 2 * length(predicted)),
  outcome = rep(c(1, 0), each = length(predicted))
)

ggplot(df, aes(x = odds, y = value)) +
  geom_line(aes(color = factor(outcome))) +
  theme_scoringutils() +
  labs(y = "Score", x = "Predicted odds") + 
  scale_color_discrete(name = "Outcome") +
  # scale_x_continuous(labels = label_double, trans = "log10") + 
  theme(panel.background = element_rect()) + 
  facet_wrap(~ score) 

ggsave("trade-offs-different-scoring-rules/scores-overconfidence-odds.jpg", height = 3, width = 8)
