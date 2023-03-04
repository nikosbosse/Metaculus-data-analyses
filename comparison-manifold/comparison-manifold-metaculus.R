library(data.table)
library(dplyr)
library(scoringutils)
library(stringr)
library(jsonlite)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)

# --------------------------- Get Manifold Data ----------------------------- ##
# helper functions
extract_metac_question <- function(string) {
  string1 <- "https://www.metaculus.com/questions/"
  string2 <- "/"
  regex <- paste0(string1, "(.*?)", string2)
  
  out <- str_match(string, pattern = regex)[, 2]
  return(out)
}

# read data 
bets <- fread("comparison-manifold/manifold-data/metaculus-binary-bets.csv") |>
  select(userId, probAfter, probBefore, contractId, createdTime) |>
  rename(t = createdTime) |>
  arrange(contractId, t) |>
  unique()

markets <- fread("comparison-manifold/manifold-data/metaculus-binary-markets.csv") |>
  as_tibble() |>
  rename(contractId = id) |>
  mutate(question_id = extract_metac_question(description)) |>
  select(-description) |>
  mutate(resolution = ifelse(resolution == "NO", 0, 1))

manifold_data <- markets |>
  inner_join(bets) |>
  select(question_id, createdTime, closeTime, resolution, resolutionTime, t, 
         probBefore, probAfter, userId) |>
  arrange(question_id, t) |>
  # duplicate the first row to get the right starting prob
  group_by(question_id) |>
  mutate(n = 1:n()) |>
  mutate(dupl = ifelse(n==1, 2, 1)) |>
  uncount(dupl) |>
  mutate(n = 1:n()) 

# for the duplicated role, assign probabilities and created time to the first one
setDT(manifold_data)
manifold_data[n == 1, `:=` (probAfter = probBefore, t = createdTime)]

manifold_data <- manifold_data |>
  select(question_id, publishTime = createdTime, closeTime, resolution, 
         resolutionTime, t, p = probAfter, userId) |>
  mutate(platform = "Manifold")


manifold_data <- manifold_data |> 
  mutate(closeTime = closeTime / 1000, 
         publishTime = publishTime / 1000, 
         resolutionTime = resolutionTime / 1000, 
         t = t / 1000)

# --------------------------- Get Metaculus Data ---------------------------- ##

ids <- manifold_data$question_id |> unique()

binary_pred_raw <- fromJSON("data/predictions-binary.json") |>
  as.data.table() |>
  filter(question_id %in% ids) |>
  select(userId = user_id, question_id, t, p = cp)

binary_questions_raw <- fromJSON("data/questions-binary.json") |>
  as.data.table() |>
  filter(question_id %in% ids) |>
  select(publishTime = publish_time, 
         question_id,
         closeTime = close_time, resolutionTime = resolve_time) |>
  mutate(platform = "Metaculus")

metaculus_data <- binary_pred_raw |>
  inner_join(binary_questions_raw)

metaculus_data |>
  filter(question_id == 9743) |>
  mutate(t = as_datetime(t), 
         closeTime = as_datetime(closeTime)) |>
  tail()

# --------------------------- Combine data ---------------------------- ##

resolutions <- manifold_data |>
  select(question_id, resolution) |>
  unique()


data <- rbind(manifold_data, metaculus_data, fill = TRUE) |>
  select(-resolution) |>
  mutate(t = as_datetime(t), 
         publishTime = as_datetime(publishTime), 
         closeTime = as_datetime(closeTime), 
         resolutionTime = as_datetime(resolutionTime)) |>
  inner_join(resolutions)


# filter data such that we have comparable time horizons for both.  ------------

# set closeTime to minimum of closeTime and resolutionTime
data <- data |>
  mutate(closeTime = pmin(closeTime, resolutionTime)) |>
  select(-resolutionTime)

# set publish time to maximum of both and close time to minimum of both
# to make sure we're looking at the same time line
data <- data |>
  group_by(question_id) |>
  mutate(publishTime = max(publishTime), 
         closeTime = min(closeTime))

# filter out data that is not in the shared time frame. 
# the better solution would be to have the last forecast that is out 
# before the first foreast that is in count at least partly, such that the 
# Manifold time series and Metaculus time series start at the exact same time
# but it doesn't seem to make much of a difference
filtered_data <- data |>
  filter(t >= publishTime, 
         t <= closeTime) 

# only keep questions that have at least 5 forecasts from each platform
filtered_data <- filtered_data |>
  # manuallt filer out question that's not in the Metaculus data
  filter(question_id != 10054) |>
  group_by(question_id, platform) |>
  mutate(n = n()) |>
  group_by(question_id) |>
  filter(min(n) > 5) |>
  select(-n) |>
  arrange(question_id, platform, t)


# --------------------------- Analysis ---------------------------- ##

time_weighted_score <- function(pred, 
                                pred2 = NULL,
                                score = "Brier", 
                                resolution, 
                                t, close_time, 
                                debug = FALSE) {
  t_min <- min(t)
  t_max <- close_time
  t_total = difftime(t_max, t_min, units = "secs") |>
    as.numeric()
  
  durations = diff(c(as.numeric(t), as.numeric(t_max)))

  if (debug) {
    cat("\n",
        paste("min time:", t_min, "\n",
              "max time:", t_max, "\n",
              "overall duration:", sum(durations), "\n",
              "overall time:", t_total), "\n"
    )
  }  
  
  if (score == "Brier") {
    score <- (pred - resolution)^2
  } else if (score == "Log") {
    score <- -log(1 - abs(resolution - pred))
  } else if (score == "rel-log") {
    if (resolution == 1) {
      score <- log(pred / pred2)
    } else {
      score <- log((1 - pred) / (1 - pred2))
    }
  }
  score <- ((score * durations) / t_total)
  score <- sum(score)
  
  return(score)
}


scores <- 
  filtered_data |>
  group_by(question_id, platform) |>
  summarise(
    score = time_weighted_score(
      pred = p, 
      close_time = unique(closeTime), 
      t = t,
      resolution = unique(resolution), 
      debug = FALSE
    )) |>
  pivot_wider(names_from = platform, values_from = score) |>
  mutate(manifold_better = Metaculus > Manifold) |>
  mutate(Difference = Manifold - Metaculus)
  

scores_log <- 
  filtered_data |>
  group_by(question_id, platform) |>
  summarise(
    score = time_weighted_score(
      pred = p, 
      score = "Log",
      close_time = unique(closeTime), 
      t = t,
      resolution = unique(resolution), 
      debug = FALSE
    )) |>
  pivot_wider(names_from = platform, values_from = score) |>
  mutate(manifold_better = Metaculus > Manifold) |>
  mutate(Difference = Manifold - Metaculus)

scores_rel_log <- 
  filtered_data |>
  pivot_wider(names_from = platform, values_from = p)
  group_by(question_id, platform) |>
  summarise(
    score = time_weighted_score(
      pred = p, 
      score = "Log",
      close_time = unique(closeTime), 
      t = t,
      resolution = unique(resolution), 
      debug = FALSE
    )) |>
  pivot_wider(names_from = platform, values_from = score) |>
  mutate(manifold_better = Metaculus > Manifold) |>
  mutate(Difference = Manifold - Metaculus)




s# Giant plot with predictions over time
p_timelines <- filtered_data |>
  # filter(question_id == 10046) |>
  # duplicate every forecast with the next points to get straight lines on the 
  # plot
  uncount(2) |>
  group_by(question_id, platform) |>
  mutate(n = 1:n(), 
         time_shifted = c(t[-1], max(t))) |>
  mutate(t = ifelse(n%%2 == 0, time_shifted, t)) |>
  mutate(t = as_datetime(t)) |>
  select(-time_shifted) |>
  ggplot(aes(y = p, x = t, color = platform)) +
  geom_line() +
  geom_text(data = scores |>
              # filter(question_id == 10046) |>
              mutate(platform = "Metaculus"), 
            inherit.aes = FALSE,
            aes(label = paste(
              "Brier:",
              signif(Metaculus, 2), "(Met.)", "vs", 
              signif(Manifold, 2), "(Man.)"
            ), y = -Inf, x = as_datetime(-Inf)), 
            vjust = -1, hjust = -0.2) + 
  facet_wrap(~ type_and_scale, scale = "free_y") + 
  scale_y_continuous(expand = expansion(mult = c(0.20, 0.05))) + 
  geom_hline(aes(yintercept = resolution), 
             linetype = "dashed", color = "grey60") + 
  theme_scoringutils() + 
  facet_wrap(~ question_id, scale = "free_x", ncol = 5) +
  labs(y = "Forecast", x = "Time", colour = "Platform")

ggsave("comparison-manifold/output/prediction_timelines.jpg", 
       plot = p_timelines, width = 16, height = 33)


# summarised comparison between the two ----------------------------------------
scores |>
  ungroup() |>
  summarise(Metaculus = mean(Metaculus), 
            Manifold = mean(Manifold))

scores_log |>
  ungroup() |>
  summarise(Metaculus = mean(Metaculus), 
            Manifold = mean(Manifold))

wilcox.test(scores$Metaculus, scores$Manifold)

# count how often one or the other ones
comparison <- scores |>
  group_by(manifold_better) |>
  summarise(n = n()) |>
  pull(n)
sum(scores$manifold_better)
sum(scores_log$manifold_better)


n_predictions <- filtered_data |>
  group_by(question_id, platform) |>
  summarise(n = n()) |>
  inner_join(scores |>
               pivot_longer(cols = c(Manifold, Metaculus), 
                            values_to = "score", 
                            names_to = "platform")) |>
  mutate(winner = case_when(
    (platform == "Manifold" & manifold_better) ~ TRUE,
    (platform == "Metaculus" & !manifold_better) ~ TRUE,
    TRUE ~ FALSE
  )) |>
  ggplot(aes(x = platform, y = n, 
             group = question_id, color = winner)) +
  geom_point() +
  geom_line(
    # aes(color = manifold_better), 
    color = "grey80",
    linewidth = 0.1) + 
  scale_y_log10() +
  annotate(geom = "text", 
           size = 2.4,
           x = 1, y = 1,
           hjust = -0.06, vjust = -5,
           label = paste(
             "Better Brier Score:", comparison[1], "(Metaculus)", 
             comparison[2], "(Manifold)")) +
  scale_color_manual(values = c("black", "red", "black", "red")) +
  theme_scoringutils() + 
  labs(color = "Better Brier Score", y = "Number of forecasts", x = "Platform")


ggsave("comparison-manifold/output/number-of-forecasts.jpg", width = 5.5, height = 4.5)  








## -------------------------- bootstrap analysis ---------------------------- ##

# Analysis on the difference between a single questions ------------------------
bootstrap_single_mean <- 
  replicate(
    1e2, 
    expr = sample(scores$Difference, size = 1e2)
  )


plot_df <- data.frame (
  diff = sample(scores$Difference, size = 1e5, replace = TRUE)
)


p_differences <- scores |> 
  ggplot(aes(x = Difference)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), 
                 bins = 30, 
                 color = "white") + 
  theme_scoringutils() + 
  labs(y = "Proportion", x = "Difference in Brier score (Manifold - Metaculus)")

ggsave("comparison-manifold/output/score-differences.jpg", plot = p_differences,
       width = 7.5, height = 3.5)



# Analysis on the mean across 64 questions -------------------------------------

bootstrap_mean <- function(scores, index) {
  scores <- scores[index, ]
  mani <- mean(scores$Manifold)
  meta <- mean(scores$Metaculus)
  diff <- mani - meta
  return(c(
    "Manifold" = mani, 
    "Metaculus" = meta, 
    "Difference" = diff
  ))
}

if (FALSE) {
  n_bootstrap <- 1e5
  bootstrap_means <- replicate(
    n_bootstrap, 
    expr = 
      bootstrap_mean(
        scores, 
        sample(1:64, size = 64, replace = TRUE)
      )
  ) 
  saveRDS(bootstrap_means, file = "comparison-manifold/output/bootstrap_means.RDS")
} else {
  bootstrap_means <- readRDS(file = "comparison-manifold/output/bootstrap_means.RDS")
}

if (FALSE) {
  n_bootstrap <- 1e4
  bootstrap_means_log <- replicate(
    n_bootstrap, 
    expr = 
      bootstrap_mean(
        scores_log, 
        sample(1:64, size = 64, replace = TRUE)
      )
  ) 
  saveRDS(bootstrap_means_log, file = "comparison-manifold/output/bootstrap_means_log.RDS")
} else {
  bootstrap_means_log <- readRDS(file = "comparison-manifold/output/bootstrap_means_log.RDS")
}


plot_bootstrap <- function(bootstrap_means, scores) {
  plot_df <- bootstrap_means |>
    t() |>
    as.data.frame() |>
    pivot_longer(cols = c(Manifold, Metaculus, Difference), 
                 values_to = "score", names_to = "Platform")
  
  observations <- data.frame(
    "Platform" = c("Manifold", "Metaculus"), 
    "score" = c(mean(scores$Manifold), mean(scores$Metaculus))
  )
  
  p_means <- plot_df |>
    filter(Platform %in% c("Metaculus", "Manifold")) |>
    ggplot(aes(x = score, 
               fill = Platform, color = Platform)) +
    geom_histogram(aes(y = after_stat(count/sum(count))),
                   position = "identity",
                   bins = 60,
                   alpha = 0.5,
                   # fill = "grey60", 
                   color = "white") + 
    geom_segment(inherit.aes = FALSE, 
                 data = observations,
                 aes(x = score, y = 0, xend = score, yend = 0.044),
                 color = "grey20",
                 linetype = "dashed", linewidth = 1.05) +
    # geom_vline(data = observations, aes(xintercept = score), 
    #            linetype = "dashed", linewidth = 1.05, color = "grey20") +
    theme_scoringutils() + 
    # geom_vline(xintercept = observed, color = "grey20", linetype = "dashed") + 
    expand_limits(x = 0) + 
    labs(y = str_wrap("Proportion of bootstrap samples", width = 20), 
         x = "Mean Brier score") 
  
  observed <- mean(scores$Manifold - scores$Metaculus)
  
  p_diff <- plot_df |>
    filter(Platform %in% c("Difference")) |>
    ggplot(aes(x = score)) +
    geom_histogram(aes(y = after_stat(count/sum(count))),
                   bins = 60,
                   fill = "grey70", 
                   color = "white") + 
    theme_scoringutils() + 
    geom_segment(inherit.aes = FALSE, 
                 aes(x = observed, y = 0, xend = observed, yend = 0.07, 
                     color = "Observed"),
                 linetype = "dashed", linewidth = 1.05) +
    expand_limits(x = 0) + 
    scale_color_manual(values = "grey20") + 
    labs(y = str_wrap("Proportion of bootstrap samples", width = 20), 
         x = "Mean difference in Brier score", 
         color = "") 
  
  p_diff <- p_diff + 
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom", 
          axis.title.y = element_text(size = 10)) 
  
  return(p_diff)
}


p_combined <- plot_bootstrap(bootstrap_means, scores)

ggsave("comparison-manifold/output/bootstrap-means.jpg", plot = p_combined,
       width = 7.5, height = 3.5)

p_combined_log <- plot_bootstrap(bootstrap_means_log, scores_log)


t.test(scores$Manifold, scores$Metaculus, paired = TRUE)
wilcox.test(scores$Manifold, scores$Metaculus, paired = TRUE)
diffs <- bootstrap_means[3, ]
diffs[diffs <= 0]



t.test(scores_log$Manifold, scores_log$Metaculus, paired = TRUE)
wilcox.test(scores_log$Manifold, scores_log$Metaculus, paired = TRUE)

