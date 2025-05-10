#-------------------------------------------------------------------------------
#' data-engineering.r
    cli::cli_h1('data-engineering.r')
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' extract sewing bee data from wikipedia
#-------------------------------------------------------------------------------

source('scripts/_setup.r')

#--- hosts and judges (manual) ---

gbsb_host_judge <- tibble(
  person = c(
    'Claudia Winkleman',
    'Joe Lycett',
    'Sara Pascoe',
    'Kiell Smith-Bynoe',
    'Patrick Grant',
    'May Martin',
    'Esme Young'
  ),
  role = c('host', 'host', 'host', 'host', 'judge_1', 'judge_2', 'judge_2'),
  series = c(
    list(1:4),
    list(5:7),
    list(8:9),
    list(10),
    list(1:10),
    list(1:3),
    list(4:10)
  )
) |> 
  unnest_longer(series) |> 
  spread(key = role, value = person) |> 
  print()

#--- series overview ---

gbsb_url <- "https://en.wikipedia.org/wiki/The_Great_British_Sewing_Bee"

gbsb_wiki <- read_html(gbsb_url)

# get the tables from the main page

gbsb_tables <- html_nodes(gbsb_wiki, ".wikitable")

# extract the overview table

gbsb_overview <- gbsb_tables  |> 
  pluck(1) |> 
  html_table(fill = TRUE) |> 
  clean_names() |> 
  mutate(
    premiere = dmy(premiere),
    finale = dmy(finale)
  ) |> 
  left_join(gbsb_host_judge, by = 'series') |> 
  print()

if(sum(is.na(gbsb_overview$host)) > 0) {
  cli_abort('Update the manual table of hosts and judges')
}

# save

gbsb_overview |> 
  saveRDS('data/gbsb-overview.rds')

gbsb_overview |> 
  write_csv('data/gbsb-overview.csv')

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

# function to get sewers for each series

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

# run function across series

gbsb_sewers <- map_dfr(series, get_sewers) |> 
  spread(key = key, value = value) 

# check

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

# save

gbsb_sewers |> 
  saveRDS('data/gbsb-sewers.rds')

gbsb_sewers |> 
  write_csv('data/gbsb-sewers.csv')

#--- series eliminations ---

# function to get eliminations for each series

get_eliminations <- function(num) {
  
  cli_alert_info('eliminations for series {num}')
  
  gbsb_series_tables |> 
    filter(table_type == 'elimination') |> 
    select(-table_type) |> 
    filter(series == num) |> 
    unnest(table, names_repair = 'minimal') |> 
    remove_empty(which = 'cols') |> 
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
    ) 
  
}

# run function across series

gbsb_eliminations_all <- map_dfr(series, get_eliminations)

# remove episodes after elimination

gbsb_eliminations <- gbsb_eliminations_all |> 
  group_by(series, sewer) |> 
  arrange(series, sewer, -episode) |> 
  filter(cumsum(!is.na(result)) > 0) |> 
  ungroup() |> 
  arrange(series, sewer, episode)

# check

gbsb_eliminations |> 
  count(result) |> 
  print()

gbsb_eliminations |> 
  group_by(series, sewer) |> 
  filter(episode == max(episode)) |> 
  ungroup() |> 
  count(episode) |> 
  print()

# save

gbsb_eliminations |> 
  saveRDS('data/gbsb-eliminations.rds')

gbsb_eliminations |> 
  write_csv('data/gbsb-eliminations.csv')

#--- series ratings and viewership ---

# function to get ratings for each series

get_ratings <- function(num) {
  
  cli_alert_info('ratings for series {num}')
  
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

# run function across series and tidy

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
    episode = as.integer(episode ),
    air_date = dmy(air_date),
    total_viewers_m = as.numeric(total_viewers_m ),
    weekly_ranking_all = as.integer(weekly_ranking_all),
    weekly_ranking_bbc = as.integer(weekly_ranking_bbc)
  )

# check

gbsb_ratings |> 
  group_by(series) |> 
  summarise(
    n_episode = n(),
    min_air_date = min(air_date),
    max_air_date = max(air_date),
    missing_viewer_count = mean(is.na(total_viewers_m )),
    missing_ranking_all = mean(is.na(weekly_ranking_all)),
    missing_ranking_bbc = mean(is.na(weekly_ranking_bbc))
  ) |> 
  print()

# save

gbsb_ratings |> 
  saveRDS('data/gbsb-ratings.rds')

gbsb_ratings |> 
  write_csv('data/gbsb-ratings.csv')

#--- series episodes ---

# names to set before unnesting

names_4 <- c('sewer', 'pattern_rank', 'transformation_name', 'made_to_measure_name')

names_5 <- c('sewer', 'pattern_rank', 'transformation_name', 'transformation_rank', 
  'made_to_measure_name')

# tidy the tables before unnesting

episode_tables <- gbsb_series_tables |> 
  filter(table_type == 'episodes') |> 
  select(-table_type) |> 
  group_by(series) |> 
  mutate(episode = row_number()) |> 
  ungroup() |> 
  group_by(series, episode) |> 
  mutate(
    challenge_names = map(table, ~unique(names(.x)[2:length(names(.x))])),
    table_renamed = map(
      table, 
      ~if(ncol(.x) == 5) {
        set_names(.x, names_5) |> 
          mutate_all(as.character)
      } else {
        set_names(.x, names_4) |> 
          mutate_all(as.character)
      }
    )
  ) |> 
  ungroup()

# unnest episode table and tidy columns
# add in garment of the week winner

episode_results <- episode_tables |> 
  select(series, episode, table_renamed) |> 
  unnest(table_renamed) |> 
  mutate(
    pattern_rank = str_remove(pattern_rank, '\\=') |> 
      as.integer(),
    transformation_rank = str_remove(transformation_rank, '\\=') |> 
      as.integer()
  ) |> 
  select(
    series, episode, sewer, pattern_rank, transformation_name, transformation_rank, 
    made_to_measure_name
  ) |> 
  left_join(
    gbsb_eliminations |> 
      filter(result == 'Garment of the week') |> 
      transmute(
        series, episode, sewer,
        garmet_of_week_win = 1
      ),
    by = c('series', 'episode', 'sewer')
  ) |> 
  mutate(garmet_of_week_win = coalesce(garmet_of_week_win, 0))

# check

episode_results |> 
  count(pattern_rank) |> 
  print()

episode_results |> 
  count(transformation_rank) |> 
  print()

episode_results |> 
  count(garmet_of_week_win) |> 
  print()

#  save  

episode_results |> 
  saveRDS('data/gbsb-episodes.rds')

episode_results |> 
  write_csv('data/gbsb-episodes.csv')

#--- challenge names ---

# unnest challenge names and clean

challenge_names <- episode_tables |> 
  select(series, episode, challenge_names) |> 
  unnest(challenge_names) |> 
  separate(
    challenge_names, 
    into = c('challenge_type', 'challenge_name'), 
    sep = '\\('
  ) |> 
  mutate(
    challenge_type = str_to_sentence(challenge_type) |> 
      str_trim() |> 
      str_replace('Alteration', 'Transformation'),
    challenge_name = str_remove(challenge_name, '\\)') |> 
      str_trim()
  ) |> 
  print()

# check

challenge_names |> 
  count(challenge_type) |> 
  print()

challenge_names |> 
  count(challenge_name, sort = TRUE) |> 
  print()

#  save  

challenge_names |> 
  saveRDS('data/gbsb-challenge-names.rds')

challenge_names |> 
  write_csv('data/gbsb-challenge-names')

#--- series episode themes ---


#--- tidytuesday submission ---
# https://dslc-io.github.io/tidytuesdayR/articles/curating.html

#tt_clean()

#-------------------------------------------------------------------------------
