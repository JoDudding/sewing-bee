#-------------------------------------------------------------------------------
#' viewership-charts.r
cli::cli_h1('viewership-charts.r')
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' replicate the charts in bakeoff recipes
#-------------------------------------------------------------------------------

#--- load the libraries ---

library(tidyverse)
library(glue)
library(scales)
library(cli)
  
#--- read the data and palette ---

gbsb_palette <- readRDS('data/gbsb_palette.rds')
gbsb_ratings <- readRDS('data/gbsb_ratings.rds')

# gbsb_palette |> show_col()

#--- create series colour palette ---

set.seed(19991)

series_pal <- sample(gbsb_palette[2:12], length(unique(gbsb_ratings$series)))

names(series_pal) <- unique(gbsb_ratings$series)

# series_pal |> show_col()

type_pal <- c(Premiere = '#39566F', Middle = '#A29395', Finale = '#9C291B')

#--- custom ggplot scales ---

scale_fill_series <- function(...) {
  scale_fill_manual(..., values = series_pal)
}

scale_colour_series <- function(...) {
  scale_colour_manual(..., values = series_pal)
}

scale_x_comma <- function(
    ..., 
  breaks = scales::pretty_breaks(),
  expand_min = 0,
  expand_max = 0.05
) {
  scale_x_continuous(
    ..., 
    breaks = breaks, 
    expand = expansion(mult = c(expand_min, expand_max))
  )
}

scale_y_comma <- function(
  ..., 
  breaks = scales::pretty_breaks(),
  expand_min = 0,
  expand_max = 0.05
) {
  scale_y_continuous(
    ..., 
    breaks = breaks, 
    expand = expansion(mult = c(expand_min, expand_max))
  )
}

#--- ggplot theme ---

theme_set(
  theme_minimal() +
    theme(
      plot.title.position = 'plot',
      plot.caption.position = 'plot'
    )
)

update_geom_defaults('col', aes(fill = '#9C291B', colour = NA))
update_geom_defaults('line', aes(colour = '#39566F', linewidth = 1))
update_geom_defaults('point', aes(colour = '#39566F', size = 2))

#--- data prep ---

ratings <- gbsb_ratings |> 
  select(series, episode, total_viewers_m) |> 
  arrange(series, episode) |> 
  mutate(
    series_ep = row_number()
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
  ggplot(aes(series_ep, total_viewers_m, fill = factor(series))) +
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

#--- line chart faceted ---

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
  facet_wrap(~series) +
  guides(fill = 'none', colour = 'none')

#--- first last line ---

ratings |> 
  filter(episode_type != 'Middle') |> 
  select(episode_type, series, total_viewers_m) |> 
  ggplot(aes(series, total_viewers_m, colour = episode_type, fill = episode_type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  scale_y_comma(expand_min = 0.05) +
  scale_colour_manual(values = type_pal) +
  labs(
    x = 'Series', y = 'Total viewers (m)',
    fill = 'Episode Type', colour = 'Episode Type',
    title = 'Similar viewership for the first and last episodes in a series',
    subtitle = 'Total viewers (m) across episodes by series of GBSB'
  )

#--- first last dumbbell ---

ratings |> 
  filter(episode_type != 'Middle') |> 
  ggplot(aes(y = series, x = total_viewers_m, group = series, colour = episode_type)) +
  geom_line() +
  geom_point() +
  scale_x_comma() +
  scale_y_reverse(breaks = 1:nrow(ratings_first_last)) +
  scale_colour_manual(values = type_pal) +
  labs(
    y = 'Series',
    x = 'Total viewers (m)',
    colour = NULL,
    title = 'Change in viewership between Premiere and Finale'
  )

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
    title = 'Change in viewership between Premiere and Finale'
  ) +
  guides(colour = 'none')
  

#--- finale bumps ---

ratings_first_last |> 
  ggplot(aes(series, num_change)) +
  geom_col() +
  scale_x_reverse(breaks = 1:nrow(ratings_first_last)) +
  scale_y_comma(expand_min = 0.05) +
  labs(
    y = 'Difference in total viewers (m)',
    x = 'Series',
    title = "No 'Finale Bump' pattern across series",
    subtitle = 'Change in viewership between Premiere and Finale'
  ) +
  coord_flip()

#--- percentage change ---

ratings_first_last |> 
  ggplot(aes(series, pct_change)) +
  geom_col() +
  geom_hline(yintercept = median(ratings_first_last$pct_change)) +
  scale_x_reverse(breaks = 1:nrow(ratings_first_last)) +
  scale_y_continuous(label = percent) +
  labs(
    y = '% Difference in total viewers',
    x = 'Series',
    title = "No 'Finale Bump' pattern across series",
    subtitle = 'Percent change in viewership between Premiere and Finale'
  ) +
  coord_flip()

#--- percentage change lollipop ---

ratings_first_last |> 
  ggplot(aes(pct_change, series, label = label, xend = 0)) +
  geom_vline(xintercept = 0) +
  geom_text(aes(hjust = -1 *sign(pct_change) + 0.5)) +
  geom_point() +
  geom_segment() +
  scale_x_continuous(label = percent) +
  scale_y_reverse(breaks = 1:nrow(ratings_first_last)) +
  labs(
    x = '% change',
    y = 'Series',
    title = 'Series 3 had the biggest increase between Premiere and Finale',
    subtitle = 'Change in viewership between Premiere and Finale'
  )

#--- first last scatterplot ---

ratings_first_last |> 
  ggplot(aes(Premiere, Finale, label = series, colour = factor(series))) +
  geom_point() +
  geom_text(nudge_x = 0.1) +
  geom_abline(slope = 1, linewidth = 0.1) +
  scale_colour_series() +
  scale_x_comma(expand_min = 0.05) +
  scale_y_comma(expand_min = 0.05) +
  labs(
    x = 'Premiere Total viewers (m)',
    y = 'Finale Total viewers (m)',
    title = 'Scatterplot'
  ) +
  guides(colour = 'none')
  
