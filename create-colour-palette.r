#-------------------------------------------------------------------------------
#' create-colour-palette.r
    cli::cli_h1('create-colour-palette.r')
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' use the eyedroppeR package to extact a colour palette from the great british
#' sewing bee cotton reel picture
#-------------------------------------------------------------------------------

#--- load packages ---

devtools::install_github("doehm/eyedroppeR")

library(eyedroppeR)
library(scales)

#--- extract a palette from the picture ---

extract_pal(n = 15, img_path = 'sewingbee.jpg', label = "gbsb_palette")

#--- copy the palette from the console ---

gbsb_palette <- c(
  '#2D2522', '#5D3220', '#7B523E', '#9C291B', '#C84722', '#AF7045', 
  '#DB714E', '#CA9470', '#D0AD9E', '#DFCBBE', '#9CB1C3', '#A29395', 
  '#8E736F', '#67849B', '#39566F'
)

#--- print to check ---

gbsb_palette |> 
  show_col()

#--- save palette ---

gbsb_palette |> 
  saveRDS('data/gbsb_ratings.rds')
