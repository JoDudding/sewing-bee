#-------------------------------------------------------------------------------
#' predicting-winner.r
cli::cli_h1("predicting-winner.r")
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' predicting the winner after four episodes
#-------------------------------------------------------------------------------
#' notes
#' filter to series with 8+ episodes
#' filter to sewers that have survived to episode 4
#' pattern challenge average rank
#' alteration challenge average rank
#' garment of the week count and recency
#-------------------------------------------------------------------------------

source("scripts/_setup.r")

#--- read data ---

gbsb_eliminations <- readRDS("data/gbsb-eliminations.rds")

gbsb_episodes <- readRDS("data/gbsb-episodes.rds")

#--- parameters ---

num_episodes <- 8
predict_after_ep <- 4

cli::cli_text("Parameters:")
cli::cli_dl(list(
  min_num_episodes = num_episodes,
  predict_after_ep = predict_after_ep
))

#--- included sewers and target ---

included_sewers <- gbsb_eliminations |>
  group_by(series) |>
  filter(n() >= num_episodes) |>
  ungroup() |>
  filter(episode > predict_after_ep) |>
  group_by(series, sewer) |>
  summarise(
    target = max(coalesce(result, "x") == "Winner")
  )

log_obj("included_sewers")

included_sewers |>
  group_by(series) |>
  summarise(
    n = n_distinct(sewer),
    n_target = sum(target)
  ) |>
  print()

#--- get the episodes before the cut off ---

in_running <- gbsb_episodes |>
  filter(episode <= predict_after_ep) |>
  group_by(series, sewer) |>
  summarise(
    mean_pattern = mean(pattern_rank),
    best_pattern = min(pattern_rank),
    win_pattern = sum(pattern_rank == 1),
    mean_transformation = mean(transformation_rank),
    best_transformation = min(transformation_rank),
    win_transformation = sum(transformation_rank == 1),
    win_garmet_of_week = sum(garmet_of_week_win)
  ) |>
  ungroup() |>
  inner_join(included_sewers, by = c("series", "sewer"))

log_obj("in_running")

#--- functions for proportion test ---

prop_test <- function(
    data,
    group,
    event,
    confidence_level = 0.95,
    direction = "two.sided", #' less') {
  # get z to use in confidence intervals
  signif_z <- qnorm(

    (1 - confidence_level) / if_else(direction == "two.sided", 2, 1),
    lower.tail = FALSE
  )

  # summarise by the groups you are comparing
  data_summary <- data |>
    group_by({{ group }}) |>
    summarise(
      tot_n = n(),
      event_n = sum({{ event }}),
      event_pct = mean({{ event }})
    ) |>
    ungroup() |>
    mutate(
      event_sd = sqrt((1 / tot_n) * event_pct * (1 - event_pct)),
      lci = pmax(event_pct - (signif_z * event_sd), 0),
      uci = pmin(event_pct + (signif_z * event_sd), 1)
    )

  # apply the proportion test and add interpretation
  data_test <- prop.test(
    x = data_summary$event_n,
    n = data_summary$tot_n,
    conf.level = confidence_level,
    alternative = direction
  ) |>
    broom::tidy() |>
    rename(
      p_value = p.value,
      diff_lci = conf.low,
      diff_uci = conf.high
    ) |>
    mutate(
      signif = p_value <= (1 - confidence_level),
      signif_tick = if_else(signif, "✔", "✖"),
      interpret = case_when(
        !signif ~ glue::glue(
          "Insufficient evidence of difference at {percent(confidence_level)} confidence"
        ),
        TRUE ~ glue::glue(
          "Evidence of difference at {percent(confidence_level)} confidence"
        )
      ),
      confidence_level = confidence_level
    )

  # print summary to the log
  data_summary |>
    transmute(

      {{ group }},
      print = glue("{percent(event_pct, 0.1)} ({percent(lci, 0.1)} - {percent(uci, 0.1)})")
    ) |>
    spread(key = {{ group }}, value = print) |>
    bind_cols(
      data_test |>
        transmute(

          method,
          alternative,
          confidence_level = percent(confidence_level),
          interpret,
          p_value = percent(p_value, 0.001)
        )
    ) |>
    cli_dl()

  # return summary with prop test results appended

  return(
    data_summary |>
      bind_cols(
        data_test |>
          gather(starts_with("estimate"), key = "xx", value = "estimate") |>
          select(-xx)
      ) |>
      invisible()
  )
}

prop_test_chart <- function(data, compare_group, within_group = NULL) {
  data <- data |>
    mutate(
      facet_name = coalesce({{ within_group }}, "Total"),
      facet_name = paste(facet_name, signif_tick)
    )

  confidence_level = percent(data$confidence_level[1])
  
  p <- data |>
    mutate(compare_group = {{ compare_group }}) |>
    ggplot(aes(compare_group, event_pct,
      ymax = uci, ymin = lci,
      colour = compare_group
    )) +
    geom_pointrange() +
    geom_point(aes(size = signif), shape = 21, fill = NA) +
    scale_y_continuous(label = percent) +
    scale_size_manual(values = c("TRUE" = 4, "FALSE" = 0)) +
    labs(
      x = NULL, y = NULL, colour = NULL,
      subtitle = glue("Differences significant at {confidence_level} confidence are circled")
    ) +
    guides(colour = "none", size = "none")

  if (length(unique(data$facet_name)) > 1) {
    p <- p + facet_wrap(~facet_name, nrow = 1)
  }

  return(p)
}

in_running |> 
  mutate(
    best_pattern_1_2 = if_else(best_pattern <= 2, 'First or second', 'Third plus')
  ) |> 
  prop_test(group = best_pattern_1_2, event = target, confidence_level = 0.8) |> 
  prop_test_chart(best_pattern_1_2) +
  scale_colour_manual(values = c(gbsb_col$blue, gbsb_col$red)) +
  labs(
    title = 'Probablilty of winning with a first or second in the first four\npattern challenges'
  )

gg_save("winner-best-pattern-1-2-prop-test")

#--- pattern challenge ---

in_running |>
  group_by(best_pattern) |>
  summarise(
    n = n(),
    pct_winner = mean(target)
  ) |>
  ggplot(aes(factor(best_pattern), pct_winner)) +
  geom_col() +
  scale_y_continuous(label = percent) +
  labs(
    x = "Best pattern challenge rank",
    y = "Overall winner rate",
    title = "The best pattern challenge rank is a good predictor",
    subtitle = glue("Winner rate by best pattern challenge rank after week {
      predict_after_ep
    }")
  )

gg_save("winner-best-pattern")

in_running |>
  group_by(win_pattern) |>
  summarise(
    n = n(),
    pct_winner = mean(target)
  ) |>
  ggplot(aes(factor(win_pattern), pct_winner)) +
  geom_col(fill = gbsb_col$blue) +
  scale_y_continuous(label = percent) +
  labs(
    x = "Number of pattern challenge wins",
    y = "Overall winner rate",
    title = "The number of pattern challenge wins is a good predictor",
    subtitle = glue("Winner rate by number of pattern challenge wins after week {
      predict_after_ep
    }")
  )

gg_save("winner-num-pattern")

#--- transformation challenge ---

in_running |>
  group_by(best_transformation) |>
  summarise(
    n = n(),
    pct_winner = mean(target)
  ) |>
  ggplot(aes(factor(best_transformation), pct_winner)) +
  geom_col() +
  scale_y_continuous(label = percent) +
  labs(
    x = "Best transformation challenge rank",
    y = "Overall winner rate",
    title = "Sewers with best transformation challenge rank over 4 do not win",
    subtitle = glue("Winner rate by best transformation challenge rank after week {
      predict_after_ep
    }")
  )

gg_save("winner-best-transformation")

in_running |>
  group_by(win_transformation) |>
  summarise(
    n = n(),
    pct_winner = mean(target)
  ) |>
  ggplot(aes(factor(win_transformation), pct_winner)) +
  geom_col(fill = gbsb_col$blue) +
  scale_y_continuous(label = percent) +
  labs(
    x = "Number of transformation challenge wins",
    y = "Overall winner rate",
    title = "The number transformation challenge wins is not a good predictor",
    subtitle = glue("
    Winner rate by number of transformation challenge wins after week {
      predict_after_ep
    }
    ")
  )

gg_save("winner-num-transformation")

#--- garment of week ---

in_running |>
  group_by(win_garmet_of_week) |>
  summarise(
    n = n(),
    pct_winner = mean(target)
  ) |>
  ggplot(aes(factor(win_garmet_of_week), pct_winner)) +
  geom_col() +
  scale_y_continuous(label = percent) +
  labs(
    x = "Number of garment of week wins",
    y = "Overall winner rate",
    title = "Sewers with no garments of the week are more likely to win overall",
    subtitle = glue("Winner rate by number of garment of the week wins after week {
      predict_after_ep
    }")
  )

gg_save("winner-num-gow")
