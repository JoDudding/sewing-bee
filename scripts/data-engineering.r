#-------------------------------------------------------------------------------
#' data-engineering.r
cli::cli_h1("data-engineering.r")
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' extract sewing bee data from wikipedia
#-------------------------------------------------------------------------------

source("scripts/_setup.r")

#--- hosts and judges (manual) ---

cli::cli_h2("hosts and judges")

gbsb_host_judge <- tibble(
  person = c(
    "Claudia Winkleman",
    "Joe Lycett",
    "Sara Pascoe",
    "Kiell Smith-Bynoe",
    "Patrick Grant",
    "May Martin",
    "Esme Young"
  ),
  role = c("host", "host", "host", "host", "judge_1", "judge_2", "judge_2"),
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
  mutate(series = as.factor(series))

log_obj("gbsb_host_judge")

#--- series overview ---

cli::cli_h2("series overview")

gbsb_url <- "https://en.wikipedia.org/wiki/The_Great_British_Sewing_Bee"

gbsb_wiki <- read_html(gbsb_url)

# get the tables from the main page

gbsb_tables <- html_nodes(gbsb_wiki, ".wikitable")

# extract the overview table

gbsb_overview <- gbsb_tables |>
  pluck(1) |>
  html_table(fill = TRUE) |>
  clean_names() |>
  mutate(
    series = as.factor(series),
    premiere = dmy(premiere),
    finale = dmy(finale)
  ) |>
  left_join(gbsb_host_judge, by = "series")

if (sum(is.na(gbsb_overview$host)) > 0) {
  cli_abort("Update the manual table of hosts and judges")
}

log_obj("gbsb_overview")

# check

glimpse(gbsb_overview)

gbsb_overview |>
  count(host, sort = TRUE) |>
  print()

# save

save_rds_csv("gbsb_overview")

#--- get information from series pages ---

cli::cli_h2("series pages")

series <- gbsb_overview$series

gbsb_series_urls <- glue("{gbsb_url}_series_{series}")

gbsb_series_wiki <- tibble(
  series = series,
  wiki = map(gbsb_series_urls, read_html)
)

log_obj("gbsb_series_wiki")

gbsb_series_tables <- gbsb_series_wiki |>
  group_by(series) |>
  mutate(
    table = map(
      wiki,
      ~ html_nodes(.x, ".wikitable") |>
        html_table(fill = TRUE)
    )
  ) |>
  unnest_longer(table) |>
  mutate(
    table_type = case_when(
      row_number() == 1 ~ "sewers",
      row_number() == 2 ~ "elimination",
      row_number() == n() ~ "ratings",
      TRUE ~ "episodes"
    )
  ) |>
  ungroup() |>
  select(-wiki)

log_obj("gbsb_series_tables")

#--- series eliminations ---

cli::cli_h2("series eliminations")

# function to get eliminations for each series

get_eliminations <- function(num) {
  cli_alert_info("eliminations for series {num}")

  gbsb_series_tables |>
    filter(table_type == "elimination") |>
    select(-table_type) |>
    filter(series == num) |>
    unnest(table, names_repair = "minimal") |>
    remove_empty(which = "cols") |>
    gather(-series, -2, key = "episode", value = "result") |>
    transmute(
      series,
      sewer = Sewer,
      episode = parse_number(episode),
      result = str_remove(result, "\\[.*\\]") |>
        str_trim(),
      result = case_when(
        result %in% c("BG", "WIN") ~ "Garment of the week",
        result %in% c("OUT", "ELIM") ~ "Eliminated",
        str_sub(str_to_upper(result), 1, 3) == "WIN" ~ "Winner",
        str_sub(str_to_upper(result), 1, 3) == "RUN" ~ "Runner-up",
        result == "WDR" ~ "Withdraw"
      )
    )
}

# run function across series

gbsb_eliminations_all <- map_dfr(series, get_eliminations)

log_obj("gbsb_eliminations_all")

# check

gbsb_eliminations_all |>
  count(series, result) |>
  spread(key = result, value = n) |>
  print()

# remove episodes after elimination

gbsb_eliminations <- gbsb_eliminations_all |>
  group_by(series, sewer) |>
  arrange(series, sewer, -episode) |>
  filter(cumsum(!is.na(result)) > 0) |>
  ungroup() |>
  arrange(series, sewer, episode) |>
  mutate(
    result = coalesce(result, "Through") |>
      factor(levels = c(
        "Winner", "Runner-up", "Garment of the week",
        "Through", "Eliminated", "Withdraw"
      ))
  )

log_obj("gbsb_eliminations")

# check

glimpse(gbsb_eliminations)

gbsb_eliminations |>
  count(series, result) |>
  spread(key = result, value = n) |>
  print()

gbsb_eliminations |>
  group_by(series, sewer) |>
  filter(episode == max(episode)) |>
  ungroup() |>
  count(episode) |>
  print()

# save

save_rds_csv("gbsb_eliminations")

#--- series ratings and viewership ---

cli::cli_h2("series atings and viewership")

# function to get ratings for each series

get_ratings <- function(num) {
  cli_alert_info("ratings for series {num}")

  gbsb_series_tables |>
    filter(table_type == "ratings") |>
    select(-table_type) |>
    filter(series == num) |>
    unnest(table) |>
    gather(-1, -2, key = "key", value = "value") |>
    rename(episode = 2) |>
    mutate(
      across(2:4, ~ str_remove(.x, "\\[.*\\]")),
      key = str_to_lower(key)
    )
}

# run function across series and tidy

gbsb_ratings <- map_dfr(series, get_ratings) |>
  mutate(
    key = case_when(
      key == "airdate" ~ "air_date",
      key == "total viewers(millions)" ~ "total_viewers_m",
      key == "weekly rankingall channels" ~ "weekly_ranking_all",
      key == "bbc twoweekly ranking" ~ "weekly_ranking_bbc",
      TRUE ~ str_replace_all(key, " ", "_")
    )
  ) |>
  spread(key = key, value = value) |>
  mutate(
    episode = as.integer(episode),
    air_date = dmy(air_date),
    total_viewers_m = as.numeric(total_viewers_m),
    weekly_ranking_all = as.integer(weekly_ranking_all),
    weekly_ranking_bbc = as.integer(weekly_ranking_bbc)
  )

log_obj("gbsb_ratings")

# check

glimpse(gbsb_ratings)

gbsb_ratings |>
  group_by(series) |>
  summarise(
    n_episode = n(),
    min_air_date = min(air_date),
    max_air_date = max(air_date),
    missing_viewer_count = mean(is.na(total_viewers_m)),
    missing_ranking_all = mean(is.na(weekly_ranking_all)),
    missing_ranking_bbc = mean(is.na(weekly_ranking_bbc))
  ) |>
  print()

# save

save_rds_csv("gbsb_ratings")

#--- series episodes ---

cli::cli_h2("series episodes")

# names to set before unnesting

names_4 <- c("sewer", "pattern_rank", "transformation_name", "made_to_measure_name")

names_5 <- c(
  "sewer", "pattern_rank", "transformation_name", "transformation_rank",
  "made_to_measure_name"
)

# tidy the tables before unnesting

episode_tables <- gbsb_series_tables |>
  filter(table_type == "episodes") |>
  select(-table_type) |>
  group_by(series) |>
  mutate(episode = row_number()) |>
  ungroup() |>
  group_by(series, episode) |>
  mutate(
    challenge_names = map(table, ~ unique(names(.x)[2:length(names(.x))])),
    table_renamed = map(
      table,
      ~ if (ncol(.x) == 5) {
        set_names(.x, names_5) |>
          mutate_all(as.character)
      } else {
        set_names(.x, names_4) |>
          mutate_all(as.character)
      }
    )
  ) |>
  ungroup()

log_obj("episode_tables")

# unnest episode table and tidy columns
# add in garment of the week winner

episode_results <- episode_tables |>
  select(series, episode, table_renamed) |>
  unnest(table_renamed) |>
  mutate(
    pattern_rank = str_remove(pattern_rank, "\\=") |>
      as.integer(),
    transformation_rank = str_remove(transformation_rank, "\\=") |>
      as.integer()
  ) |>
  select(
    series, episode, sewer, pattern_rank, transformation_name, transformation_rank,
    made_to_measure_name
  ) |>
  left_join(
    gbsb_eliminations |>
      filter(result == "Garment of the week") |>
      transmute(
        series, episode, sewer,
        garmet_of_week_win = 1
      ),
    by = c("series", "episode", "sewer")
  ) |>
  mutate(garmet_of_week_win = coalesce(garmet_of_week_win, 0))

log_obj("episode_results")

# check

glimpse(episode_results)

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
  saveRDS("data/gbsb-episodes.rds")

episode_results |>
  write_csv("data/gbsb-episodes.csv")

#--- challenge names ---

cli::cli_h2("challenge names")

# unnest challenge names and clean

challenge_names <- episode_tables |>
  select(series, episode, challenge_names) |>
  unnest(challenge_names) |>
  separate(
    challenge_names,
    into = c("challenge_type", "challenge_name"),
    sep = "\\("
  ) |>
  mutate(
    challenge_type = str_to_sentence(challenge_type) |>
      str_trim() |>
      str_replace("Alteration", "Transformation"),
    challenge_name = str_remove(challenge_name, "\\)") |>
      str_trim()
  )

log_obj("challenge_names")

# check

glimpse(challenge_names)

challenge_names |>
  count(challenge_type) |>
  print()

challenge_names |>
  count(challenge_name, sort = TRUE) |>
  print()

#  save

challenge_names |>
  saveRDS("data/gbsb-challenge-names.rds")

challenge_names |>
  write_csv("data/gbsb-challenge-names")

#--- series sewers ---

cli::cli_h2("series sewers")

# sewer names in the elimination table with their final placement

elim_sewers <- gbsb_eliminations |>
  group_by(series, sewer) |>
  arrange(series, sewer, result) |>
  summarise(
    max_ep = max(episode),
    best_result = first(result)
  ) |>
  ungroup() |>
  mutate(
    best_result = case_when(
      best_result %in% c("Winner", "Runner-up") ~ best_result
    )
  ) |>
  arrange(series, -max_ep, best_result) |>
  group_by(series, max_ep, best_result) |>
  mutate(n_ep = n()) |>
  nest(sewers = sewer) |>
  group_by(series) |>
  mutate(
    placement = coalesce(best_result, paste0(lag(cumsum(n_ep)) + 1, "th")) |>
      fct_inorder()
  ) |>
  unnest(sewers) |>
  ungroup() |>
  select(series, sewer, placement)

log_obj("elim_sewers")

# function to get sewers for each series

get_sewers <- function(num) {
  cli_alert_info("sewers for series {num}")

  gbsb_series_tables |>
    filter(table_type == "sewers") |>
    select(-table_type) |>
    filter(series == num) |>
    unnest(table) |>
    gather(-series, -2, key = "key", value = "value") |>
    rename(sewer_fullname = 2) |>
    mutate(
      across(2:4, ~ str_remove(.x, "\\[.*\\]")),
      key = str_to_lower(key),
      key = if_else(
        key %in% c("age", "occupation", "placement"),
        key,
        "residence"
      )
    )
}

# run function across series

gbsb_sewers <- map_dfr(series, get_sewers) |>
  spread(key = key, value = value) |>
  select(-placement) |>
  mutate(
    series = as.factor(series),
    age = as.integer(age)
  ) |>
  # add the sewer short name
  inner_join(
    elim_sewers,
    by = "series",
    relationship = "many-to-many"
  ) |>
  filter(str_detect(sewer_fullname, glue("^{sewer}")))

log_obj("gbsb_sewers")

# check

glimpse(gbsb_sewers)

gbsb_sewers |>
  count(placement) |>
  print()

gbsb_sewers |>
  group_by(series) |>
  summarise(
    n_sewers = n(),
    pct_with_age = mean(!is.na(age)),
    pct_with_occ = mean(!is.na(occupation)),
    pct_with_res = mean(!is.na(residence)),
    pct_with_place = mean(!is.na(placement))
  ) |>
  print()

# save

save_rds_csv("gbsb_sewers")

#--- series episode themes ---

cli::cli_h2("series episode themes")

# get level 3 headings from the series pages

gbsb_episode_theme_raw <- tibble(
  series,
  ep_theme = map(
    gbsb_series_urls,
    ~ read_html(.x) |>
      html_elements("h3") |>
      html_text2()
  )
) |>
  unnest_longer(ep_theme)

log_obj("gbsb_episode_theme_raw")

# clean it up

gbsb_episode_theme <- gbsb_episode_theme_raw |>
  filter(str_detect(ep_theme, "^Episode")) |>
  group_by(series) |>
  mutate(
    episode = row_number(),
    theme = str_remove(ep_theme, "^.*: ") |>
      str_remove(" Week") |>
      str_remove(" - .*$") |>
      str_remove("\\[.*\\]") |>
      str_trim() |>
      str_replace("^Reduce, Reuse.*$", "Reduce, Reuse, Recycle") |>
      str_replace("^Lingerie.*$", "Lingerie and Sleepwear") |>
      str_replace("^Children.*$", "Children's Clothes") |>
      str_replace("^1980's$", "1980s") |>
      str_replace("^70s$", "1970s")
  ) |>
  ungroup() |>
  # remove if no theme
  filter(
    theme != ep_theme,
    !str_detect(str_to_lower(theme), "final")
  ) |>
  select(series, episode, theme)

log_obj("gbsb_episode_theme")

# check

glimpse(gbsb_episode_theme)

gbsb_episode_theme |>
  count(theme, sort = TRUE) |>
  print()

# save

save_rds_csv("gbsb_episode_theme")

#--- tidytuesday submission ---

# https://dslc-io.github.io/tidytuesdayR/articles/curating.html

# tt_clean()

#-------------------------------------------------------------------------------
