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

gbsb_palette <- readRDS('data/gbsb_ratings.rds')
gbsb_ratings <- readRDS('data/gbsb_ratings.rds')

#--- create series colour palette ---

set.seed(19991)

series_pal <- sample(gbsb_palette[2:12], length(unique(gbsb_ratings$series)))

names(series_pal) <- unique(gbsb_ratings$series)

series_pal |> 
  show_col()

scale_fill_series <- function(...) {
  scale_fill_manual(..., values = series_pal)
}

scale_colour_series <- function(...) {
  scale_colour_manual(..., values = series_pal)
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
