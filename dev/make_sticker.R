library(hexSticker)
library(magick)
library(sysfonts)

img <- magick::image_read('~/PennSIVE/bossrdata/tmp/bossr_sticker_base_full.png')

fonts_dataset <- sysfonts::font_files()

add_font <- function(family){
  get_font_path <- function(family){
    fonts_dataset |>
      filter(family == .env$family) |>
      mutate(filepath = file.path(path, file)) |>
      pull(filepath) |>
      purrr::pluck(1)
  }
  sysfonts::font_add(family, get_font_path(family))
}

add_font('Perfect Dark (BRK)')

hexSticker::sticker(
  img, 
  package = 'bossr',
  s_width = 1.29,
  s_height = 1.29,
  s_x = 1,
  s_y = 1,
  url = 'github.com/pennsive/bossr',
  u_size = 6,
  u_color = 'green2',
  p_family = 'Perfect Dark (BRK)',
  p_size = 20,
  p_y = 1.5,
  p_color = 'green2',
  h_fill = 'black',
  h_color = 'purple1',
  h_size = 1
    ) |> print()
 
