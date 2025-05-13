#-------------------------------------------------------------------------------
#' create-colour-palette.r
cli::cli_h1("create-colour-palette.r")
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' use the eyedroppeR package to extract a colour palette from the great british
#' sewing bee cotton reel picture
#' https://gradientdescending.com/select-colours-from-an-image-in-r-with-eyedropper/
#-------------------------------------------------------------------------------

#--- load packages ---

# devtools::install_github("doehm/eyedroppeR")

library(eyedroppeR)
library(scales)

#--- extract a palette from the picture manually ---

# eyedropper(n = 5, img_path = "assets/sewingbee.jpg", label = "gbsb_palette_man")

gbsb_red <- "#aa1111"
gbsb_blue <- "#1e547b"
gbsb_dark <- "#2d2522"
gbsb_pale <- "#e8dfce"
gbsb_grey <- "#a2979b"

show_col(c(gbsb_red, gbsb_blue, gbsb_dark, gbsb_pale, gbsb_grey))

#--- extract a palette from the picture automatically ---

# extract_pal(n = 15, img_path = "assets/sewingbee.jpg", label = "gbsb_palette")

#--- copy the palette from the console ---

gbsb_palette <- c(
  "#2D2522", "#5D3220", "#7B523E", "#9C291B", "#C84722", "#AF7045",
  "#DB714E", "#CA9470", "#D0AD9E", "#DFCBBE", "#9CB1C3", "#A29395",
  "#8E736F", "#67849B", "#39566F"
)

#--- print to check ---

gbsb_palette |>
  show_col()

#--- save palette ---

gbsb_colours <- list(
  red = gbsb_red,
  blue = gbsb_blue,
  dark = gbsb_dark,
  pale = gbsb_pale,
  grey = gbsb_grey,
  palette = gbsb_palette
)

cli_text("{.strong gbsb_colours} contains:")

names(gbsb_colours) |>
  cli_li()

gbsb_colours |>
  saveRDS("data/gbsb_colours.rds")

cli::cli_alert_info("data/gbsb_colours.rds created")
