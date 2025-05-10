#-------------------------------------------------------------------------------
#' _setup.r
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' set up stuff like loading libraries, themes and functions shared by multiple
#' scripts
#-------------------------------------------------------------------------------

#--- packages ---

library(tidyverse)
library(glue)
library(scales)
library(cli)
library(rvest)
library(janitor)
library(tidytuesdayR)
library(showtext)

#--- options ---

options(
  dplyr.width = Inf,
  papersize = "a4",
  tab.width = 2,
  width = 80,
  max.print = 25,
  stringsAsFactors = FALSE,
  lubridate.week.start = 6,
  tibble.print_max = 25,
  tibble.print_min = 25,
  tibble.width = Inf,
  dplyr.summarise.inform = FALSE,
  tidyverse.quiet = TRUE
)

showtext_opts(dpi = 300)
showtext_auto()

#--- ggplot theme ---

# check the colour palette objects files have been created and load palette

if (
  !file.exists("data/gbsb_palette.rds") |
    !file.exists("data/gbsb_colours.rds")
) {
  cli_abort("Create palettes first by running {.file scripts/create-colour-palette.r}")
}

gbsb_palette <- readRDS("data/gbsb_palette.rds")
gbsb_col <- readRDS("data/gbsb_colours.rds")

# load fonts

font_add_google("Libre Franklin", "franklin")
font_add_google("Domine", "domine")

# set the theme

base_size <- 10

theme_set(
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "franklin", colour = gbsb_col$dark, lineheight = 1.2),
      plot.title = element_text(
        family = "franklin", size = rel(1.5), face = "bold",
        lineheight = 1.2
      ),
      plot.subtitle = element_text(family = "franklin", size = rel(1.2)),
      plot.caption = element_text(family = "franklin", hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.background = ggplot2::element_rect(fill = gbsb_col$pale, colour = NA),
      plot.background = ggplot2::element_rect(fill = gbsb_col$pale, colour = NA),
      legend.background = ggplot2::element_rect(fill = gbsb_col$pale, colour = NA),
      strip.background = ggplot2::element_rect(fill = gbsb_col$grey, colour = NA),
      panel.grid.major.x = element_line(linewidth = 0.15),
      panel.grid.major.y = element_line(linewidth = 0.15),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
)

# set defaults for geoms

update_geom_defaults("col", aes(fill = gbsb_col$red, colour = NA))
update_geom_defaults("line", aes(colour = gbsb_col$blue, linewidth = 1))
update_geom_defaults("segment", aes(colour = gbsb_col$blue))
update_geom_defaults("point", aes(colour = gbsb_col$blue, size = 2))
update_geom_defaults("text", aes(
  family = "franklin", colour = gbsb_col$dark,
  size = base_size / .pt * 0.8
))

#--- functions ---

# save ggplot objects

gg_save <- function(pic_name, plot = last_plot(), width = 7, height = width / 1.618, ...) {
  ggsave(
    filename = glue::glue("charts/{pic_name}.png"),
    plot = plot,
    width = width,
    height = height,
    ...
  )
}

# customer ggplot scales

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
    expand_max = 0.05) {
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
    expand_max = 0.05) {
  scale_y_continuous(
    ...,
    breaks = breaks,
    expand = expansion(mult = c(expand_min, expand_max))
  )
}
