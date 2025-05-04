#-------------------------------------------------------------------------------
#' data-engineering.r
    cli::cli_h1('data-engineering.r')
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' extract sewing bee data from wikipedia
#-------------------------------------------------------------------------------

library(tidyverse)
library(glue)
library(scales)
library(cli)
library(rvest)
library(janitor)

#--- series overview ---

gbsb_url <- "https://en.wikipedia.org/wiki/The_Great_British_Sewing_Bee"

gbsb_wiki <- read_html(gbsb_url)

gbsb_tables <- html_nodes(gbsb_wiki, ".wikitable")

gbsb_overview <- gbsb_tables  |> 
  pluck(1) |> 
  html_table(fill = TRUE) |> 
  clean_names() |> 
  mutate(
    premiere = dmy(premiere),
    finale = dmy(finale)
  ) |> 
  print()

#--- get information from series pages ---

series <- gbsb_overview$series

gbsb_series_urls <- glue('{gbsb_url}_series_{series}')

gbsb_series_wiki <- tibble(
  series = series,
  wiki = map(gbsb_series_urls, read_html)
)

gbsb_series_tables <- gbsb_series_wiki |> 
  group_by(series) |> 
  mutate(
    table = map(
      wiki, 
      ~html_nodes(.x, ".wikitable") |> 
        html_table(fill = TRUE)
    )
  ) |> 
  unnest_longer(table) |> 
  mutate(
    table_type = case_when(
      row_number() == 1 ~ 'sewers',
      row_number() == 2 ~ 'elimination',
      row_number() == n() ~ 'ratings',
      TRUE ~ 'episodes'
    )
  ) |> 
  ungroup() |> 
  select(-wiki)
  

#--- series sewers ---

get_sewers <- function(num) {
  
  cli_alert_info('sewers for series {num}')
  
  gbsb_series_tables |> 
    filter(table_type == 'sewers') |> 
    select(-table_type) |> 
    filter(series == num) |> 
    unnest(table) |> 
    gather(-series, -2, key = 'key', value = 'value') |> 
    rename(sewer_fullname = 2) |> 
    mutate(
      across(2:4, ~str_remove(.x, '\\[.*\\]')),
      key = str_to_lower(key),
      key = if_else(
        key %in% c('age', 'occupation', 'placement'),
        key,
        'residence'
      )
    )
  
}

gbsb_sewers <- map_dfr(series, get_sewers) |> 
  spread(key = key, value = value) 

gbsb_sewers |> 
  group_by(series) |> 
  summarise(
    n_sewers = n(),
    n_with_age = sum(!is.na(age)),
    n_with_occ = sum(!is.na(occupation)),
    n_with_res = sum(!is.na(residence)),
    n_with_place = sum(!is.na(placement))
  ) |> 
  print()

#--- series eliminations ---

get_eliminations <- function(num) {
  
  cli_alert_info('eliminations for series {num}')
  
  gbsb_series_tables |> 
    filter(table_type == 'elimination') |> 
    select(-table_type) |> 
    filter(series == num) |> 
    unnest(table, names_repair = 'minimal') |> 
    gather(-series, -2, key = 'episode', value = 'result') |> 
    transmute(
      series,
      sewer = Sewer,
      episode = parse_number(episode),
      result = str_remove(result, '\\[.*\\]'),
      result = case_when(
        result %in% c('BG', 'WIN') ~ 'Garment of the week',
        result %in% c('OUT', 'ELIM') ~ 'Eliminated',
        str_sub(str_to_upper(result), 1, 3) == 'WIN' ~ 'Winner',
        str_sub(str_to_upper(result), 1, 3) == 'RUN' ~ 'Runner-up',
        result == 'WDR'~ 'Withdraw',
        ! str_trim(result) == '' ~ result
      )
    ) |> 
    group_by(series, episode) |> 
    filter(
      ! is.na(result) |
      cumsum(coalesce(result, '') %in% c('Eliminated', 'Winner', 'Withdraw')) == 0
    ) |> 
    ungroup()
  
}

#gbsb_eliminations <- map_dfr(series, get_eliminations)

cli_alert_danger('not working as tables have an extra header row for series 1-5')


bind_rows(
  get_eliminations(5),
  get_eliminations(6),
  get_eliminations(7),
  get_eliminations(8),
  get_eliminations(9),
  get_eliminations(10)
) |> 
  tabyl(series, result) |> 
  as_tibble()

get_eliminations(6) |> 
  filter(result == 'Garment of the week')

# 2 garments of the week in series 6 episode 6 looks correct




#--- series ratings ---

get_ratings <- function(num) {
  
  cli_alert_info('sewers for series {num}')
  
  gbsb_series_tables |> 
    filter(table_type == 'ratings') |> 
    select(-table_type) |> 
    filter(series == num) |> 
    unnest(table) |> 
    gather(-1, -2, key = 'key', value = 'value') |> 
    rename(episode = 2) |> 
    mutate(
      across(2:4, ~str_remove(.x, '\\[.*\\]')),
      key = str_to_lower(key)
    )
  
}

gbsb_ratings <- map_dfr(series, get_ratings) |> 
  mutate(
    key = case_when(
      key == 'airdate' ~ 'air_date',
      key == 'total viewers(millions)' ~ 'total_viewers_m',
      key == 'weekly rankingall channels' ~ 'weekly_ranking_all',
      key == 'bbc twoweekly ranking' ~ 'weekly_ranking_bbc',
      TRUE ~ str_replace_all(key, ' ', '_')
    )
  ) |> 
  spread(key = key, value = value) |> 
  mutate(
    air_date = dmy(air_date),
    total_viewers_m = as.numeric(total_viewers_m ),
    weekly_ranking_all = as.integer(weekly_ranking_all),
    weekly_ranking_bbc = as.integer(weekly_ranking_bbc)
  ) |> 
  print(n = 100)

#--- series episodes ---


#--- series episode themes ---


#--- save tables as csv ---


#--- save tables as rda ---

