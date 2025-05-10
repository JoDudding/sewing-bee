#-------------------------------------------------------------------------------
#' eliminations-and-results-charts.r
    cli::cli_h1('eliminations-and-results-charts.r')
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' create heatmap of results and eliminations for each series
#-------------------------------------------------------------------------------

source("scripts/_setup.r")

#--- read data ---

gbsb_eliminations <- readRDS("data/gbsb-eliminations.rds")

#--- set colour palette ---

result_pal <- c(
  gbsb_col$red, gbsb_col$blue, "#C84722", 
  gbsb_col$grey, gbsb_palette[2], gbsb_palette[13]
)

names(result_pal) <- levels(gbsb_eliminations$result)

# show_col(result_pal)

#--- function to create chart for each series ---

series_elim <- function(num) {
  
  gbsb_eliminations |> 
    filter(series == num) |> 
    arrange(-episode, result) |> 
    mutate(
      sewer = fct_rev(fct_inorder(sewer)),
      episode = as.factor(episode)
    ) |> 
    ggplot(aes(episode, sewer, label = str_wrap(result, 10), fill = result)) +
    geom_tile(colour = 'white') +
    geom_text(colour = 'white', lineheight = 1) +
    scale_x_discrete(position = 'top') +
    scale_fill_manual(values = result_pal, na.value = gbsb_col$pale) +
    labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      title = glue('Results and elimination for series {num}')
    ) +
    guides(fill = 'none')
  
  gg_save(glue("eliminations-series-{num}"))

}

#--- run for each series ---

walk(
  levels(gbsb_eliminations$series),
  series_elim
)
