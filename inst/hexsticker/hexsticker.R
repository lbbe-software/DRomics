#' 
#' Create an Hexagonal Sticker for the Package
#' 

hexSticker::sticker(
  
  subplot  = here::here("inst", "hexsticker", "icon.png"),
  package  = "DRomics",
  filename = here::here("man", "figures", "hexsticker.png"),
  
  p_size   = 0.0,         # Title
  u_size   = 0.0,         # URL
  
  h_fill   = "#dedede",   # Background
  h_color  = "#9c5c16",   # Border
  
  s_x      = 1.00,        # Subplot
  s_y      = 1.05,        # Subplot
  s_width  = 0.85,        # Subplot
  s_height = 0.85,        # Subplot
  asp = 1
)
