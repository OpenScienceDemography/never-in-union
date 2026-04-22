#===============================================================================
# 2023-03-18 -- never-in-union (Refactored 2026-04-03)
# visualize
# Ryo Mogi, rymo@sdu.dk
# Ewa Batyra, ebatyra@ced.uab.es
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# prepare the session
source("src/0-prepare-session.R")

# load the prepared dataset
load("out/never.rda")
load("out/never_edu2.rda")
load("out/geodata.rda")


never %>%
  ggplot(aes(p_childless, p_niu, color = sex)) +
  geom_point()


# never %>%
#   ggplot(aes(prop_childless, country))+
#   geom_point(size = 4, color = "#004444")+ # this one is needed to set the coordinate space
#   geom_hline(yintercept = seq(1, 80, 2), size = 4, color = "#ccffff")+
#   geom_vline(xintercept = 0, size = 2, color = "#004444AA")+
#   geom_text(
#     data = . %>% filter(sex == "Women"),
#     x = .36, aes(label = country %>% tolower),
#     size = 3.5, hjust = 1, family = "ah", fontface = 2,
#     color = "#004444AA"
#   )+
#   geom_point(size = 4, color = "#004444")+
#   geom_flag(
#     # data = . %>% filter(sex == "Men"),
#     x = -.01, aes(country = iso2 %>% tolower), size = 4
#   ) +
#   geom_moon(aes(ratio = prop_neverinunion, fill = sex, right = FALSE), size = 4, color = NA)+
#   facet_wrap(~ sex, nrow = 1)+
#   scale_fill_manual(values = c("#dfff00", "#00FFFF"))+
#   scale_x_continuous(position = "top")+
#   theme(
#     legend.position = "none",
#     panel.grid.major.y = element_blank(),
#     axis.text.y = element_blank(),
#     strip.text = element_blank(),
#     axis.text = element_text(face = 2),
#     axis.title = element_text(face = 2)
#   )+
#   labs(
#     x = "Proportion of childlesness",
#     y = NULL
#   )+
#   geom_text(
#     data = tibble(sex = c("Men", "Women"), sign = c("♂", "♀")),
#     aes(label = sign),
#     x = .005, y = 77,
#     size = 20, hjust = 0, colour = c("#687807FF", "#017979FF"),
#     family = "Roboto", fontface = 2
#   )

# ggsave("out/fig.pdf", width = 10, height = 10)

# UPD  2023-03-20 ------------------------------
# Remove flags, colorcode countries, add legend

# first 4 colors are taken from gapminder.org
# two more colors are produced with  "#ff5872" %>% clr_rotate(), 33 and 250 degrees
gap_colors <- c(
  "#00d5e9",
  "#ff5872",
  "#7feb02",
  "#E37900FF",
  "#C7B40BFF",
  "#3B90FFFF"
)

# # a separate dataset for sex comparisons
# other_sex <- never %>%
#   transmute(
#     country,
#     sex = sex %>%
#       str_replace("Men", "f") %>%
#       str_replace("Women", "Men") %>%
#       str_replace("f", "Women"),
#     prop_childless_other_rex = prop_childless
#   ) %>%
#   left_join(
#     never %>% select(country, sex, prop_childless)
#   )

never %>%
  ggplot(aes(p_childless, country)) +
  geom_hline(yintercept = seq(1, 90, 2), size = 4, color = "#eaeaea") +
  geom_vline(xintercept = 0, size = 2, color = "#004444AA") +
  geom_line(aes(group = country, color = continent), size = 1, alpha = 3 / 4) +
  geom_point(size = 3, aes(fill = sex), shape = 21, stroke = NA) +
  geom_text(
    data = . %>% filter(sex == "Women"),
    x = 41,
    aes(label = country %>% tolower, color = continent),
    size = 4,
    hjust = 1,
    family = "ah",
    fontface = 2,
    # color = "#004444AA"
  ) +
  scale_fill_manual(values = c("#1B5E20", "#4FC3F7")) +
  scale_color_manual(values = gap_colors) +
  scale_x_continuous(position = "top", limits = c(0, 41), expand = c(0, 0)) +
  # the segment for sex differences
  # geom_segment(
  #   data = other_sex,
  #   aes(x = prop_childless_other_rex, xend = prop_childless, yend = country),
  #   size = .5, color = "#004444AA"
  # )+
  # geom_point(
  #   data = other_sex,
  #   aes(x = prop_childless_other_rex),
  #   size = 1, color = "#004444AA", fill = "#4DB6AC", shape = 21
  # )+
  # # correct sex values
  # geom_point(
  #   size = 3/4, color = "#004444"
  # )+
  # geom_point(
  #   aes(alpha = prop_neverinunion == 0),
  #   size = 4, color = "#004444"
  # )+
  # scale_alpha_manual(values = c(1, .5))+
  # # geom_flag(
  # #   # data = . %>% filter(sex == "Men"),
  # #   x = -.01, aes(country = iso2 %>% tolower), size = 4
  # # ) +
  # geom_moon(aes(ratio = prop_neverinunion, fill = sex, right = FALSE), size = 4, color = NA)+
  # facet_wrap(~ sex, nrow = 1)+
  theme(
    plot.title = element_text(size = 24, face = 2, hjust = .5, family = "ah"),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    strip.text = element_blank(),
    axis.text = element_text(face = 2),
    axis.title = element_text(face = 2)
  ) +
  labs(
    x = "Proportion of childless population aged 35+, %",
    y = NULL,
    # title = "Not having children is often driven by inability to form a union",
    caption = "\nData: GGS wave 1, most recent DHS, and 1992 HDI; Sample: Individuals aged 35+ born in 1960s\nCountries are sorted by Gender Inequality Index from the lowest (top) to the highest (bottom)"
  ) +
  geom_text(
    data = tibble(
      sex = c("Men", "Women"),
      sign = c("♂", "♀")
    ),
    aes(label = sex),
    x = c(33, 5),
    y = 85,
    size = 10,
    hjust = 0.5,
    colour = c("#1B5E20", "#4FC3F7"),
    family = "Roboto",
    fontface = 2
  )

main <- last_plot()

# inset map of world regions
world_outline_robinson %>%
  ggplot() +
  geom_sf(aes(fill = continent), color = NA) +
  geom_sf(data = country_borders, color = "#ccffff", linewidth = .1) +
  scale_fill_manual(values = gap_colors, na.value = "#00444499") +
  theme_void() +
  theme(legend.position = "none")

inset <- last_plot()


# assemble
(out <- ggdraw(main) +
  draw_plot(inset, x = .3, width = .6, y = .05, height = .25))

ggsave("out/fig.pdf", plot = out, width = 9, height = 12)

# correlation plots -------------------------------------------------------

# x: GII, y: % of never-in-union among childless population
fig2main <- never %>%
  filter(edu2 == "All") %>%
  ggplot(aes(x = gii, y = p_niu)) +
  facet_wrap(~sex) +
  geom_point(aes(group = country, colour = continent)) +
  geom_smooth(
    method = "gam",
    se = FALSE,
    color = "#269292",
    alpha = .5,
    size = 1.5
  ) +
  scale_colour_manual(values = gap_colors) +
  scale_x_continuous(limits = c(0, 0.8)) +
  # xlim(0, 0.8) +
  labs(
    x = "Gender Inequality Index",
    y = "% never-formed-union among childless population"
  ) +
  theme(
    plot.title = element_text(size = 24, face = 2, hjust = .5, family = "ah"),
    legend.position = "none",
    panel.spacing = unit(2, "lines"),
    strip.text = element_blank(),
    axis.text = element_text(face = 2),
    axis.title = element_text(face = 2)
  ) +
  geom_text(
    data = tibble(
      sex = c("Men", "Women"),
      sign = c("♂", "♀")
    ),
    aes(label = sex),
    x = c(.08, .64),
    y = 84,
    size = 10,
    hjust = 0.5,
    colour = c("#1B5E20", "#4FC3F7"),
    family = "Roboto Condensed",
    fontface = 2
  )

# assemble
(fig2 <- ggdraw(fig2main) +
  draw_plot(inset, x = .35, width = .4, y = .75, height = .2))

ggsave("out/fig2-p-niu.pdf", fig2, width = 9, height = 6.5)

# x: GII, y: % of never-in-union among childless population
fig3main <- never_edu2 %>%
  ggplot(aes(x = gii, y = p_niu)) +
  facet_wrap(sex ~ edu2) +
  geom_point(aes(group = country, colour = continent)) +
  geom_smooth(
    method = "gam",
    se = FALSE,
    color = "#269292",
    alpha = .5,
    size = 1.5
  ) +
  scale_colour_manual(values = gap_colors) +
  scale_x_continuous(limits = c(0, 0.8)) +
  coord_cartesian(expand = FALSE) +
  # xlim(0, 0.8) +
  labs(
    x = "Gender Inequality Index",
    y = "% never-formed-union among childless population"
  ) +
  theme(
    plot.title = element_text(size = 24, face = 2, hjust = .5, family = "ah"),
    legend.position = "none",
    panel.spacing = unit(2, "lines"),
    strip.text = element_blank(),
    axis.text = element_text(face = 2),
    axis.title = element_text(face = 2)
  ) +
  geom_text(
    data = tibble(
      sex = c("Men", "Women"),
      sign = c("♂", "♀")
    ) |>
      crossing(edu2 = c("Low", "High")),
    aes(label = sex),
    x = .02,
    y = 98,
    size = 10,
    hjust = 0,
    vjust = 1,
    colour = c("#1B5E20", "#4FC3F7") |> rep(each = 2),
    family = "Roboto Condensed",
    fontface = 2
  ) +
  geom_text(
    data = tibble(
      sex = c("Men", "Women"),
      sign = c("♂", "♀")
    ) |>
      crossing(edu2 = c("Low", "High")),
    aes(label = paste0("Education:\n", edu2)),
    x = .8,
    y = 98,
    size = 7,
    hjust = 1,
    vjust = 1,
    colour = c("#1B5E20", "#4FC3F7") |> rep(each = 2),
    family = "Roboto Condensed",
    fontface = 2,
    lineheight = .95
  )

# assemble
(fig3 <- ggdraw(fig3main) +
  draw_plot(inset, x = .35, width = .4, y = .45, height = .25))

ggsave("out/fig3-p-niu-edu.pdf", fig3, width = 9, height = 12)


# convert to PNG from the saved PDF  --------------------------------------

devtools::source_gist("c7037e2b1bc6d0d6e38fc4a41de9a8c7")

convert_pdf_to_png("out")

# library(pdftools)
# pdf_convert("out/fig.pdf", filenames = "out/fig.png", dpi = 300)
# pdf_convert("out/fig-no-tit.pdf", filenames = "out/fig-no-tit.png", dpi = 300)
