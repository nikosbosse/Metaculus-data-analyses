library(dplyr)
library(ggplot2)
library(data.table)

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


# generate synthetic data
n_obs <- 1000
reps <- 1:1000
noises <- c(0, 0.1, 0.3, 0.5, 1)

replications <- lapply(reps, function(x) {
  true_prob <- runif(n = n_obs, min = 0, max = 1)
  obs <- rbinom(n = n_obs, size = 1, p = true_prob)
  
  f0 <- forecast_with_noise(true_prob, noise = 0)
  f0.1 <- forecast_with_noise(true_prob, noise = 0.1)
  f0.1a <- forecast_with_noise(true_prob, noise = 0.1)
  f0.3 <- forecast_with_noise(true_prob, noise = 0.3)
  f0.3a <- forecast_with_noise(true_prob, noise = 0.3)
  f0.5 <- forecast_with_noise(true_prob, noise = 0.5)
  f0.5a <- forecast_with_noise(true_prob, noise = 0.5)
  f1 <- forecast_with_noise(true_prob, noise = 1)
  f1a <- forecast_with_noise(true_prob, noise = 1)
  
  df <- data.frame(
    obs_id = 1:n_obs,
    prediction = c(f0, f0.1, f0.1a, f0.3, f0.3a, f0.5, f0.5a, f1, f1a), 
    obs = obs, 
    true_prob = true_prob, 
    model = c(rep(c("f0", "f0.1", "f0.1a", "f0.3", "f0.3a", "f0.5", "f0.5a", "f1", "f1a"), each = n_obs)), 
    noise = c(rep(c("0", "0.1", "0.1a", "0.3", "0.3a", "0.5", "0.5a", "1", "1a"), each = n_obs))
  ) |>
    mutate(
      brier_score = brier_score(obs, prediction), 
      log_score = log_score(obs, prediction),
      true_log_odds = log(true_prob / (1 - true_prob)), 
      log_odds = log(prediction / (1 - prediction))
    ) 
  return(df)
})

replications_df <- rbindlist(replications, idcol = "id")

replications_df |> 
  group_by(noise) |>
  filter(noise %in% noises) |>
  summarise(`Mean Log score` = mean(log_score), 
            `Mean Brier score` = mean(brier_score), 
             `Mean abs. diff pred - true`= round(mean(abs(true_prob - prediction)), 3), 
            `Mean abs. diff log odds` = round(mean(abs(true_log_odds - log_odds)), 3))


# plot the difference between predictions between noisy and perfect forecaster
bin_width <- 0.01
binned <- replications_df |>
  filter(noise %in% noises[-1]) |>
  mutate(diff = abs(prediction - true_prob)) |>
  mutate(bin = cut(true_prob, breaks = seq(0, 1, by = bin_width), include.lowest = TRUE, labels = FALSE)) |>
  group_by(bin, noise) %>%
  summarize(avg_diff = mean(diff), 
            true_prob_midpoint = (unique(bin) - 0.5) * bin_width)

binned |> 
  ggplot(aes(x = true_prob_midpoint, y = avg_diff, colour = noise)) +
  geom_point(size = 0.9) +
  geom_line() +
  labs(x = "True Probability", y = "Average Difference")


# look at percentage of 'tournaments' won.
n_obs_grid <- c(5, 10, 20, 50, 100, 200, 500, 1000)

percentage_wins <- lapply(n_obs_grid, function(n_obs) {
  replications_df |>
    filter(noise %in% noises) |>
    filter(obs_id <= n_obs) |>
    mutate(n_obs = n_obs) |>
    group_by(id, noise, n_obs) |> 
    summarise(mean_logs = mean(log_score)) |>
    pivot_wider(names_from = noise, values_from = mean_logs) |>
    pivot_longer(cols = c(`0.1`, `0.3`, `0.5`, `1`), 
                 values_to = "log_score", names_to = "noise") |>
    mutate(perfect_wins = `0` < log_score) |>
    group_by(noise, n_obs) |>
    summarise(perc_perfect_wins = mean(perfect_wins))
})

perc_win_df <- rbindlist(percentage_wins)

perc_win_df |>
  ggplot(aes(y = perc_perfect_wins, x = n_obs, colour = noise)) + 
  geom_point(size = 0.9) + 
  geom_line() +
  scale_x_continuous(breaks = c(10, 50, 100, 200, 500, 1000)) +
  labs(y = "Proportion of replications in which perfect wins", x = "Number of available independent predictions (sample size)")



# check significance
get_pvals <- function(replications, score = "brier_score", test_fun = t.test) {
  pvals <- lapply(replications, function(replication) {
    d <- replication |> 
      filter(noise %in% noises) |>
      pivot_wider(names_from = noise, values_from = all_of(score), id_cols = c(obs_id, obs))
    
    out <- lapply(n_obs_grid, function(i) {
      pvals <- sapply(noises[-1], function(noise) {
        test_fun(
          d[[paste(0)]][1:i], 
          d[[paste(noise)]][1:i], 
          paired = TRUE
        )$p.value
      })
    })
    
    names(out) <- n_obs_grid
    out <- t(as_tibble(out)) |>
      as.data.frame() 
    names(out) <- noises[-1]
    out <- out |>
      mutate(
        n_obs = n_obs_grid
      )
    
    return(out)
  })
  pvals <- rbindlist(pvals, idcol = "id")
  return(pvals)
}


pvals_bs <- get_pvals(replications, score = "brier_score")  
pvals_bs <- get_pvals(replications, score = "brier_score")  
pvals_bs_wmw <- get_pvals(replications, score = "brier_score", test_fun = wilcox.test)
pvals_logs_wmw <- get_pvals(replications, score = "log_score", test_fun = wilcox.test)


pvals_bs |> 
  pivot_longer(cols = c(`0.1`, `0.3`, `0.5`, `1`), names_to = "noise", values_to = "p-value") |>
  group_by(n_obs, noise) |>
  summarise(`p-value` = mean(`p-value`)) |>
  ggplot(aes(x = n_obs, y = `p-value`, group = noise, colour = noise)) + 
  geom_line() + 
  labs(y = "Average p-value", x = "Number of available independent predictions (sample size)")

pvals_logs_wmw |> 
  pivot_longer(cols = c(`0.1`, `0.3`, `0.5`, `1`), names_to = "noise", values_to = "p-value") |>
  group_by(n_obs, noise) |>
  summarise(proportion = mean(`p-value` < 0.05)) |>
  ggplot(aes(x = n_obs, y = proportion, group = noise, colour = noise)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = c(10, 50, 100, 200, 500, 1000)) +
  labs(y = "Proportion of replications with p <= 0.05", x = "Number of available independent predictions (sample size)")

pvals_logs |> 
  mutate(score = "logs") |> 
  rbind(pvals_bs |> mutate(score = "brier")) |>
  pivot_longer(cols = c(`0.1`, `0.3`, `0.5`, `1`), names_to = "noise", values_to = "p-value") |>
  group_by(noise, score, n_obs) |>
  summarise(`Proportion significant` = mean(`p-value` <= 0.05)) |> 
  pivot_wider(names_from = score, values_from = `Proportion significant`) |> 
  select(-brier) |>
  mutate(logs = round(logs, 2)) |>
  pivot_wider(names_from = noise, values_from = logs)


# comparing between two identical forecasters
get_pvals_equals <- function(replications, score = "brier_score") {
  
  # for 0.1, 0.3, 0.5, 1: 
    # 
  pvals <- lapply(replications, function(replication) {
    d <- replication |> 
      pivot_wider(names_from = noise, values_from = all_of(score), id_cols = c(obs_id, obs))
    
    out <- lapply(n_obs_grid, function(i) {
      pvals <- sapply(noises[-1], function(noise) {
        t.test(
          d[[paste(noise)]][1:i], 
          d[[paste0(noise, "a")]][1:i], 
          paired = TRUE
        )$p.value
      })
    })
    
    names(out) <- n_obs_grid
    out <- t(as_tibble(out)) |>
      as.data.frame() 
    names(out) <- noises[-1]
    out <- out |>
      mutate(
        n_obs = n_obs_grid
      )
    
    return(out)
  })
  pvals <- rbindlist(pvals, idcol = "id")
  return(pvals)
}


pvals_equals_bs <- get_pvals_equals(replications, score = "brier_score")  
pvals_equals_logs <- get_pvals_equals(replications, score = "log_score")

pvals_equals_bs |> 
  pivot_longer(cols = c(`0.1`, `0.3`, `0.5`, `1`), names_to = "noise", values_to = "p-value") |>
  group_by(n_obs, noise) |>
  summarise(proportion = mean(`p-value` < 0.05)) |>
  ggplot(aes(x = n_obs, y = proportion, group = noise, colour = noise)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = c(10, 50, 100, 200, 500, 1000)) +
  labs(y = "Proportion of replications with p <= 0.05", x = "Number of available independent predictions (sample size)")














# plot scores
df |>
  ggplot(aes(y = brier_score, x = model)) + 
  geom_violin(fill = "grey70", colour = NA, alpha = 0.6) + 
  geom_jitter(alpha = 0.5) + 
  geom_boxplot(alpha = 0)

# plot distance in log_odds space
df |>
  ggplot(aes(y = log_odds / true_log_odds, x = id, grop = model, colour = model)) + 
  geom_line()


# check significance
t.test(brier_score(obs, f0), brier_score(obs, f0.3), paired = TRUE)



t.test(brier_score ~ model, data = d, paired = TRUE)


  
  
