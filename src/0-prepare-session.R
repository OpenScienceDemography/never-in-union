#===============================================================================
# 2023-03-18 -- never-in-union
# prepare session
# Ryo Mogi, rymo@sdu.dk
# Ewa Batyra, ebatyra@ced.uab.es
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

library(tidyverse)
library(magrittr)
library(readxl)
library(patchwork)
library(paletteer)
library(hrbrthemes)
library(sf)
library(janitor)
library(countrycode)

library(showtext)
sysfonts::font_add_google("Roboto Condensed", "rc")
sysfonts::font_add_google("Atkinson Hyperlegible", "ah")
showtext_auto()
# showtext_auto()
# library(ggdark)
library(cowplot)
library(ggforce)
library(prismatic)
library(gggibbous)

# remotes::install_github("jimjam-slam/ggflags")
library(ggflags)

# set ggplot2 theme
devtools::source_gist("653e1040a07364ae82b1bb312501a184")
theme_set(theme_ik())
