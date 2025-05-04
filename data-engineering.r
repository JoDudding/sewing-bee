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

gbsb_tables <- html_nodes(gbsb_wiki, "table")

gbsb_overview <- gbsb_wiki  |> 
  html_node(".wikitable") |> 
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



#--- series sewers ---


#--- series eliminations ---


#--- series ratings ---



#--- series episodes ---
