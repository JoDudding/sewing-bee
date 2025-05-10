#-------------------------------------------------------------------------------
#' predicting-winner.r
cli::cli_h1('predicting-winner.r')
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

source('scripts/_setup.r')

#--- read data ---

gbsb_eliminations <- readRDS('data/gbsb-eliminations.rds')

gbsb_episodes <- readRDS('data/gbsb-episodes.rds')

#--- parameters ---

num_episodes <- 8
predict_after_ep <- 4

#--- included sewers and target ---

included_sewers <- gbsb_eliminations |> 
  group_by(series) |> 
  filter(n() >= num_episodes) |> 
  ungroup() |> 
  filter(episode > predict_after_ep) |> 
  group_by(series, sewer) |> 
  summarise(
    target = max(coalesce(result, 'x') == 'Winner')
  )

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
  inner_join(included_sewers, by = c('series', 'sewer'))

#--- pattern challenge ---

in_running |> 
  group_by(best_pattern) |> 
  summarise(
    n = n(),
    pct_winner = mean(target)
  ) |> 
  ggplot(aes(factor(best_pattern), pct_winner)) +
  geom_col() +
  scale_y_continuous(label= percent) +
  labs(
    x = 'Best pattern challenge rank',
    y = 'Overall winner rate',
    title = 'The best pattern challenge rank is a good predictor',
    subtitle = glue('Winner rate by best pattern challenge rank after week {predict_after_ep}')
  )
gg_save('winner-best-pattern')

in_running |> 
  group_by(win_pattern) |> 
  summarise(
    n = n(),
    pct_winner = mean(target)
  ) |> 
  ggplot(aes(factor(win_pattern), pct_winner)) +
  geom_col(fill = gbsb_col$blue) +
  scale_y_continuous(label= percent) +
  labs(
    x = 'Number of pattern challenge wins',
    y = 'Overall winner rate',
    title = 'The number of pattern challenge wins is a good predictor',
    subtitle = glue('Winner rate by number of pattern challenge wins after week {predict_after_ep}')
  )

gg_save('winner-num-pattern')

#--- transformation challenge ---

in_running |> 
  group_by(best_transformation) |> 
  summarise(
    n = n(),
    pct_winner = mean(target)
  ) |> 
  ggplot(aes(factor(best_transformation), pct_winner)) +
  geom_col() +
  scale_y_continuous(label= percent) +
  labs(
    x = 'Best transformation challenge rank',
    y = 'Overall winner rate',
    title = 'Sewers with best transformation challenge rank over 4 do not win',
    subtitle = glue('Winner rate by best transformation challenge rank after week {predict_after_ep}')
  )

gg_save('winner-best-transformation')

in_running |> 
  group_by(win_transformation) |> 
  summarise(
    n = n(),
    pct_winner = mean(target)
  ) |> 
  ggplot(aes(factor(win_transformation), pct_winner)) +
  geom_col(fill = gbsb_col$blue) +
  scale_y_continuous(label= percent) +
  labs(
    x = 'Number of transformation challenge wins',
    y = 'Overall winner rate',
    title = 'The number transformation challenge wins is not a good predictor',
    subtitle = glue('Winner rate by number of transformation challenge wins after week {predict_after_ep}')
  )

gg_save('winner-num-transformation')

#--- garment of week ---

in_running |> 
  group_by(win_garmet_of_week) |> 
  summarise(
    n = n(),
    pct_winner = mean(target)
  ) |> 
  ggplot(aes(factor(win_garmet_of_week), pct_winner)) +
  geom_col() +
  scale_y_continuous(label= percent) +
  labs(
    x = 'Number of garment of week wins',
    y = 'Overall winner rate',
    title = 'Sewers with no garments of the week are more likely to win overall',
    subtitle = glue('Winner rate by number of garment of the week wins after week {predict_after_ep}')
  )

gg_save('winner-num-gow')
