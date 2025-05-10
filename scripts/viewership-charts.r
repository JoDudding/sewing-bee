#-------------------------------------------------------------------------------
#' viewership-charts.r
cli::cli_h1('viewership-charts.r')
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' replicate the charts in bakeoff recipes
#-------------------------------------------------------------------------------

source('scripts/_setup.r')

#--- load data ---

gbsb_ratings <- readRDS('data/gbsb-ratings.rds')

#--- set colour palettes for series and episode type ---

# create series colour palette

set.seed(19991)

series_pal <- sample(gbsb_palette[2:12], length(unique(gbsb_ratings$series)))

names(series_pal) <- unique(gbsb_ratings$series)

# series_pal |> show_col()

# and episode type palette

type_pal <- c(Premiere = '#39566F', Middle = '#A29395', Finale = '#9C291B')

# type_pal |> show_col()

#--- data prep ---

ratings <- gbsb_ratings |> 
  select(series, episode, total_viewers_m) |> 
  arrange(series, episode) |> 
  mutate(
    series_ep = row_number(),
    series = as.factor(series)
  ) |> 
  group_by(series) |> 
  mutate(
    episode_type = case_when(
      episode == min(episode) ~ 'Premiere',
      episode == max(episode) ~ 'Finale',
      TRUE ~ 'Middle'
    ),
    premier = case_when(episode == min(episode) ~ total_viewers_m),
    finale = case_when(episode == max(episode) ~ total_viewers_m),
    series_label = case_when(episode == floor(mean(episode)) ~ series),
    series_y = case_when(
      episode == floor(mean(episode)) ~ 
        coalesce(total_viewers_m, mean(total_viewers_m, na.rm = TRUE))
    )
  ) |> 
  ungroup() 

ratings_first_last <- ratings |> 
  filter(episode_type != 'Middle') |> 
  select(episode_type, series, total_viewers_m) |> 
  spread(key = episode_type, value = total_viewers_m) |> 
  mutate(
    num_change = Finale - Premiere,
    pct_change = num_change / Premiere,
    label = percent(pct_change, 0.1)
  )

#--- bar chart ---

ratings |> 
  ggplot(aes(series_ep, total_viewers_m, fill = series)) +
  geom_col() +
  geom_text(aes(y = series_y, label = series_label), nudge_y = 0.5) +
  scale_y_comma() +
  scale_fill_series() +
  labs(
    x = NULL, y = 'Total viewers (m)',
    fill = 'Series', colour = 'Series',
    title = 'Viewership picked up in series 6',
    subtitle = glue('
    GBSB moved to BBC One for season 6, and it aired at the start of the pandemic
    Total viewers (m) across all series/episodes of GBSB
    ')
  ) +
  guides(x = 'none', fill = 'none')

gg_save('viewership-bar')

#--- lollipop chart ---

ratings |> 
  group_by(series) |> 
  mutate(
    avg_viewers = mean(total_viewers_m),
    diff_viewers = total_viewers_m - avg_viewers
  ) |> 
  filter(sum(!is.na(total_viewers_m)) >= 8) |> 
  ungroup() |> 
  ggplot(aes(episode, total_viewers_m, yend = avg_viewers)) +
  geom_segment(linewidth = 0.1) +
  geom_line(aes(y = avg_viewers), linewidth = 0.1) +
  geom_point(aes(colour = episode_type)) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_comma(expand_min = 0.05) +
  scale_colour_manual(values = type_pal) +
  facet_wrap(~ series) +
  labs(
    x = 'Episode', y = 'Total viewers (m)',
    title = 'Viewership seems steady across the episodes in a series',
    subtitle = 'Total viewers (m) across episodes by series of GBSB',
    caption = 'Only series with 8+ assessed episodes'
  ) +
  guides(colour = 'none')

gg_save('viewership-lollipop')

#--- line chart ---

ratings |> 
  group_by(series) |> 
  filter(sum(!is.na(total_viewers_m)) >= 8) |> 
  ungroup() |> 
  ggplot(aes(episode, total_viewers_m, colour = factor(series),
    label = case_when(episode_type == 'Finale' ~ series))) +
  geom_line() +
  geom_text(nudge_x = 0.15) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_comma(expand_min = 0.05) +
  scale_colour_series() +
  labs(
    x = 'Episode', y = 'Total viewers (m)',
    fill = 'Series', colour = 'Series',
    title = 'Viewership seems steady across the episodes in a series',
    subtitle = 'Total viewers (m) across episodes by series of GBSB',
    caption = 'Only series with 8+ assessed episodes'
  ) +
  guides(fill = 'none', colour = 'none')

gg_save('viewership-line')

#--- line chart faceted ---

ratings |> 
  group_by(series) |> 
  filter(sum(!is.na(total_viewers_m)) >= 8) |> 
  ungroup() |> 
  ggplot(aes(episode, total_viewers_m, colour = series)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  scale_y_comma(expand_min = 0.05) +
  scale_colour_series() +
  labs(
    x = 'Episode', y = 'Total viewers (m)',
    fill = 'Series', colour = 'Series',
    title = 'Viewership seems steady across the episodes in a series',
    subtitle = 'Total viewers (m) across episodes by series of GBSB',
    caption = 'Only series with 8+ assessed episodes'
  ) +
  facet_wrap(~series) +
  guides(fill = 'none', colour = 'none')

gg_save('viewership-facet')

#--- first last line ---

ratings |> 
  filter(episode_type != 'Middle') |> 
  select(episode_type, series, total_viewers_m) |> 
  ggplot(aes(series, total_viewers_m, colour = episode_type, fill = episode_type,
    group = episode_type)) +
  geom_line() +
  geom_point() +
  scale_y_comma(expand_min = 0.05) +
  scale_colour_manual(values = type_pal) +
  labs(
    x = 'Series', y = 'Total viewers (m)',
    fill = 'Episode Type', colour = 'Episode Type',
    title = 'Similar viewership for the first and last episodes in a series',
    subtitle = 'Total viewers (m) across episodes by series of GBSB'
  )

gg_save('viewership-line-p-f')

#--- first last dumbbell ---

ratings |> 
  filter(episode_type != 'Middle') |> 
  ggplot(aes(y = fct_rev(series), x = total_viewers_m, group = series, 
    colour = episode_type)) +
  geom_line() +
  geom_point() +
  scale_x_comma(expand_min = 0.05) +
  #scale_y_reverse() +
  scale_colour_manual(values = type_pal) +
  labs(
    y = 'Series',
    x = 'Total viewers (m)',
    colour = NULL,
    title = 'Similar viewership for the first and last episodes in a series',
    subtitle = 'Change in viewership between Premiere and Finale'
  )

gg_save('viewership-dumbbell')

#--- first last slope ---

ratings |> 
  filter(episode_type != 'Middle') |> 
  ggplot(aes(x = fct_reorder(episode_type, episode), y = total_viewers_m, 
    colour = factor(series), group = factor(series), 
    label = case_when(episode_type == 'Finale' ~ series))) +
  geom_line() +
  geom_point() +
  geom_text(hjust = -0.75) +
  scale_y_comma() +
  scale_colour_series() +
  labs(
    x = 'Episode',
    y = 'Total viewers (m)',
    colour = NULL,
    title = 'Similar viewership for the first and last episodes in a series',
    subtitle = 'Change in viewership between Premiere and Finale'
  ) +
  guides(colour = 'none')

gg_save('viewership-slope')

#--- finale bumps ---

ratings_first_last |> 
  ggplot(aes(fct_rev(series), num_change)) +
  geom_col() +
  scale_y_comma(expand_min = 0.05) +
  labs(
    y = 'Difference in total viewers (m)',
    x = 'Series',
    title = "No 'Finale Bump' pattern across series",
    subtitle = 'Change in viewership between Premiere and Finale'
  ) +
  coord_flip()

gg_save('viewership-f-bump')

#--- percentage change ---

ratings_first_last |> 
  ggplot(aes(fct_rev(series), pct_change)) +
  geom_col() +
  geom_hline(yintercept = median(ratings_first_last$pct_change), 
    colour = gbsb_col$blue, linewidth = 0.8) +
  scale_y_continuous(label = percent) +
  labs(
    y = '% Difference in total viewers',
    x = 'Series',
    title = "No 'Finale Bump' pattern across series",
    subtitle = 'Percent change in viewership between Premiere and Finale'
  ) +
  coord_flip()

gg_save('viewership-pct-change')

#--- percentage change lollipop ---

ratings_first_last |> 
  ggplot(aes(pct_change, fct_rev(series), label = label, xend = 0)) +
  geom_vline(xintercept = 0) +
  geom_text(aes(hjust = -1 *sign(pct_change) + 0.5)) +
  geom_point() +
  geom_segment() +
  scale_x_continuous(label = percent) +
  labs(
    x = '% change',
    y = 'Series',
    title = 'Series 3 had the biggest increase between Premiere and Finale',
    subtitle = 'Change in viewership between Premiere and Finale'
  )

gg_save('viewership-pct-lolli')

#--- first last scatterplot ---

ratings_first_last |> 
  ggplot(aes(Premiere, Finale, label = series)) +
  geom_point(size = 5) +
  geom_text(colour = 'white') +
  geom_abline(slope = 1, linewidth = 0.1) +
  scale_x_comma(expand_min = 0.05) +
  scale_y_comma(expand_min = 0.05) +
  labs(
    x = 'Premiere Total viewers (m)',
    y = 'Finale Total viewers (m)',
    title = 'Series 3 had the biggest difference between Premiere and Finale',
    subtitle = 'Scatterplot of total viewers (m) for Premiere vs Finale'
  ) +
  guides(colour = 'none')
  
gg_save('viewership-scatter')
