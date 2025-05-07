
devtools::install_github("doehm/eyedroppeR")

library(eyedroppeR)

extract_pal(n = 15, img_path = 'sewingbee.jpg', label = "gbsb_palette")

gbsb_palette <- c(
  '#2D2522', '#5D3220', '#7B523E', '#9C291B', '#C84722', '#AF7045', 
  '#DB714E', '#CA9470', '#D0AD9E', '#DFCBBE', '#9CB1C3', '#A29395', 
  '#8E736F', '#67849B', '#39566F'
)

gbsb_palette |> 
  show_col()

gbsb_palette |> 
  saveRDS('data/gbsb_ratings.rds')
