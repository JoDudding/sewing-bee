#-------------------------------------------------------------------------------
#' _run_all.r
cli::cli_h1("_run_all.r")
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' run all the scripts in order
#-------------------------------------------------------------------------------

source("scripts/create-colour-palette.r")

source("scripts/data-engineering.r")

source("scripts/eliminations-and-results-charts.r")

source("scripts/viewership-charts.r")

source("scripts/predicting-winner.r")
