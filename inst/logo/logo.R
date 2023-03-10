sysfonts::font_add_google('Zilla Slab', 'pf', regular.wt = 500)
fig <- magick::image_read_svg('inst/logo/oseberg-bw.svg')

hexSticker::sticker(
  subplot = fig,
  package = 'noAPI',
  p_size = 30,
  p_color = '#183271',
  p_x = 1,
  p_y = 1.4,
  p_family = 'pf',
  s_width = 1.2,
  s_height = 1,
  s_x = 1,
  s_y = .8,
  h_fill = '#169AFD',
  h_color = '#183271',
  h_size = 1.5,
  dpi = 320,
  filename = 'man/figures/logo.png'
) |> print()

usethis::use_logo(img = 'man/figures/logo.png')
