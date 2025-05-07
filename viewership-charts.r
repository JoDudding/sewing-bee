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
    series_label = case_when(episode == floor(mean(episode)) ~ series)
  ) |> 
  ungroup() 

#--- bar chart ---

ratings |> 
  ggplot(aes(series_ep, total_viewers_m, fill = factor(series),
    label = series_label)) +
  geom_col() +
  geom_text(nudge_y = 0.5) +
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

#--- line chart ---

ratings |> 
  group_by(series) |> 
  filter(sum(!is.na(total_viewers_m)) >= 8) |> 
  ungroup() |> 
  ggplot(aes(episode, total_viewers_m, colour = factor(series),
    label = case_when(episode_type == 'Finale' ~ series))) +
  geom_line(linewidth = 1) +
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
  geom_line(linewidth = 1) +
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
  geom_line(linewidth = 1) +
  geom_point(shape = 21, stroke = 1, colour = 'white') +
  scale_x_continuous(breaks = 1:10) +
  scale_y_comma(expand_min = 0.05) +
  scale_fill_manual(values = type_pal) +
  scale_colour_manual(values = type_pal) +
  labs(
    x = 'Series', y = 'Total viewers (m)',
    fill = 'Episode Type', colour = 'Episode Type',
    title = 'Similar viewership for the first and last episodes in a series',
    subtitle = 'Total viewers (m) across episodes by series of GBSB'
  )

#--- first last dumbbell ---

#--- first last slope ---

#--- finale bumps ---

#--- percentage change ---

#--- percentage change lollipop ---

#--- first last scatterplot ---

ratings |> 
  filter(episode_type != 'Middle') |> 
  select(episode_type, series, total_viewers_m) |> 
  spread(key = episode_type, value = total_viewers_m) |> 
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
  
