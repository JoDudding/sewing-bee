#-------------------------------------------------------------------------------
#' tidytuesday-prep.r
cli::cli_h1("tidytuesday-prep.r")
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' do tasks to make data tidytuesday ready
#' Using vignette: https://dslc-io.github.io/tidytuesdayR/articles/curating.html
#-------------------------------------------------------------------------------

source("scripts/_setup.r")  

# tt_curate_data()

#--- create cleaning script ---

tt_clean()

#--- document each dataset ---

source("scripts/data-engineering.r")

# sewers

gbsb_sewers <- gbsb_sewers |> 
  mutate(
    series = as.integer(series),
    placement = as.character(placement)
  ) 

#tt_save_dataset(gbsb_sewers)

# series overview

gbsb_overview <- gbsb_overview |> 
  mutate(
    series = as.integer(series)
  )

#tt_save_dataset(gbsb_overview)

# eliminations

gbsb_eliminations <- gbsb_eliminations |> 
  mutate(
    series = as.integer(series),
    episode = as.integer(episode),
    result = as.character(result)
  )

#tt_save_dataset(gbsb_eliminations)

# ratings

gbsb_ratings <- gbsb_ratings |> 
  mutate(
    series = as.integer(series)
  )

# tt_save_dataset(gbsb_ratings)

#--- create introduction ---

tt_intro()

#--- provide metadata ---

tt_meta(
  title = "sewing-bee",
  article_title = "The Great British Sewing Bee on BBC",
  article_url = "https://www.bbc.co.uk/programmes/b03myqj2",
  source_title = "The Great British Sewing Bee on Wikipedia",
  source_url = "https://en.wikipedia.org/wiki/The_Great_British_Sewing_Bee",
  image_filename = c("sewingbee.png", "eliminations-series-8.png"),
  image_alt = c(
    "Cotton reel with Great British Sewing Bee logo on top",
    "Elimination/results table for series 8"
  ),
  attribution = "Jo Dudding",
  github = "jodudding"
)

#--- submit ---
# If you make changes, run tt_submit() again to update the pull request.

# tt_submit()


