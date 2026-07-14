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
library(cowplot)
library(ggforce)
library(prismatic)
library(ggsci)

library(showtext)
sysfonts::font_add_google("Roboto Condensed", "rc")
sysfonts::font_add_google("Atkinson Hyperlegible", "ah")
showtext_auto()

# set ggplot2 theme
devtools::source_gist("653e1040a07364ae82b1bb312501a184")
theme_set(cowplot::theme_minimal_grid(font_family = "Roboto Condensed"))

# custom operators (from old 00_setting.R)
`%out%` = Negate(`%in%`)


# colors ------------------------------------------------------------------
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
# col4 |> color()
# col4 |> clr_grayscale()
# col4 |> clr_deutan()
# col4 |> clr_protan()
# col4 |> clr_tritan()

# colors from my pnas paper: Zarulli, V., Kashnitsky, I., & Vaupel, J. W. (2021). Death rates at specific life stages mold the sex gap in life expectancy. Proceedings of the National Academy of Sciences, 118(20). https://doi.org/10.1073/pnas.2010588118

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


# # color tests
# pal_four |> check_color_blindness()
# 
# pal_four |> color()
# pal_four |> clr_grayscale()
# pal_four |> clr_deutan()
# pal_four |> clr_protan()
# pal_four |> clr_tritan()
