# ..........................................................
# 2023-03-18 -- never-in-union (Refactored 2026-06-05)
# prepare session ---------
# Ryo Mogi, rymo@sdu.dk
# Ewa Batyra, ebatyra@ced.uab.es
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# ..........................................................

library(tidyverse)
library(readxl)
library(sf)
library(janitor)
library(countrycode)
library(readstata13)
library(gapminder)

library(showtext)
sysfonts::font_add_google("Roboto Condensed", "rc")
sysfonts::font_add_google("Atkinson Hyperlegible", "ah")
showtext_auto()

library(patchwork)
library(paletteer)
library(hrbrthemes)
library(cowplot)
library(ggforce)
library(prismatic)
library(gggibbous)

# set ggplot2 theme
devtools::source_gist("653e1040a07364ae82b1bb312501a184")
theme_set(cowplot::theme_minimal_grid(font_family = "Roboto Condensed"))

# custom operators (from old 00_setting.R)
`%out%` = Negate(`%in%`)

# colors ------------------------------------------------------------------


col5 <- c(
  "#BF360C", # Asia
  "#7c9e0b", # LatAm
  "#ee8833", # Africa
  "#AB47BC", # FSU
  "#BCAAA4" # Europe
)

col4 <- c(
  "#662211", # africa
  "#EC407A", # latam
  "#bb2233", # asia
  "#F48FB1" # europe
)

library(prismatic)
library(ggsci)

pal <- ggsci::pal_cosmic(palette = "signature_substitutions")

col4 <- pal(5)[c(1,2,3,5)]
col4 <- pal(4)

col4 <- c(
  "#662211", # africa
  "#EC407A", # latam
  "#bb2233", # asia
  "#F48FB1" # europe
)

# # color tests

# col4 |> check_color_blindness()
# 
# col4 |> clr_desaturate(shift = 1)
# 
# # test
# col4 |> color()
# col4 |> clr_desaturate(shift = 1)
# col4 |> clr_deutan()
# col4 |> clr_protan()
# col4 |> clr_tritan()

# colors from my pnas paper
pal_six <- c(
  "#084488", # Europe
  "#3FB3F7", # ==Females
  "#003737", # ==Males
  "#268A8A", # LatAm
  "#A14500", # Africa
  "#eec21f" # Asia
)

# choose 4
pal_four <- pal_six[c(4,5,6,1)]
pal_sex <- pal_six[c(3,2)]
