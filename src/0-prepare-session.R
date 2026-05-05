#===============================================================================
# 2023-03-18 -- never-in-union
# prepare session
# Ryo Mogi, rymo@sdu.dk
# Ewa Batyra, ebatyra@ced.uab.es
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

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

col5 <- c(
  "#ee8833",
  "#BF360C",
  "#7c9e0b",
  "#AB47BC",
  "#BCAAA4"
)
