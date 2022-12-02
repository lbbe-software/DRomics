#' 
#' Create an Hexagonal Sticker for the Package
#' 

hexSticker::sticker(
  
  subplot  = here::here("inst", "hexsticker", "icon.png"),
  package  = "dromics",
  filename = here::here("man", "figures", "hexsticker.png"),
  dpi      = 600,
  p_size   = 0,
  p_family = "Aller_Rg",
  h_fill   = "#d0d0d0",   # Background
  h_color  = "#9c5c16",   # Border
  s_x      = 1,
  s_y      = 1,
  s_width  = 0.60
)
