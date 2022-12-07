#' 
#' Create an Hexagonal Sticker for the Package
#' 

hexSticker::sticker(
  
  subplot  = here::here("inst", "hexsticker", "icon.png"),
  package  = "DRomics",
  filename = here::here("man", "figures", "hexsticker.png"),
  dpi      = 600,
  
  p_size   = 0.0,         # Title
  u_size   = 6.0,         # URL
  p_family = "Aller_Rg",
  
  h_fill   = "#d0d0d0",   # Background
  h_color  = "#9c5c16",   # Border
  u_color  = "#433625",   # URL
  
  s_x      = 1.00,        # Subplot
  s_y      = 1.05,        # Subplot
  s_width  = 0.85,        # Subplot
  
  url      = "https://aursiber.github.io/DRomics"
)