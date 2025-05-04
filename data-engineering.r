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


#--- series ratings ---



#--- series episodes ---


#--- series episode themes ---


