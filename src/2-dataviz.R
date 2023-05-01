#===============================================================================
# 2023-03-18 -- never-in-union
# visualize
# Ryo Mogi, rymo@sdu.dk
# Ewa Batyra, ebatyra@ced.uab.es
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# prepare the session
source("src/0-prepare-session.R")

# load the prepared dataset
load("out/never.rda")
load("out/geodata.rda")


never %>% 
  ggplot(aes(prop_childless, prop_neverinunion, color = sex))+
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
gap_colors <- c("#ff5872","#7feb02", "#ffe700", "#00d5e9", "#E37900FF", "#3B90FFFF")

# a separate dataset for sex comparisons
other_sex <- never %>% 
  transmute(
    country, 
    sex = sex %>% 
      str_replace("Men", "f") %>% 
      str_replace("Women", "Men") %>% 
      str_replace("f", "Women"), 
    prop_childless_other_rex = prop_childless
  ) %>% 
  left_join(
    never %>% select(country, sex, prop_childless)
  )

never %>% 
  ggplot(aes(prop_childless, country))+
  geom_point(size = 4, color = "#004444")+ # this one is needed to set the coordinate space
  geom_hline(yintercept = seq(1, 80, 2), size = 5, color = "#ccffff")+
  geom_vline(xintercept = 0, size = 2, color = "#004444AA")+
  geom_text(
    data = . %>% filter(sex == "Women"),
    x = .43, aes(label = country %>% tolower, color = continent), 
    size = 3.5, hjust = 1, family = "ah", fontface = 2,
    # color = "#004444AA"
  )+
  # the segment for sex differences
  geom_segment(
    data = other_sex,
    aes(x = prop_childless_other_rex, xend = prop_childless, yend = country),
    size = .5, color = "#004444AA"
  )+
  geom_point(
    data = other_sex,
    aes(x = prop_childless_other_rex),
    size = 1, color = "#004444AA", fill = "#4DB6AC", shape = 21
  )+
  # correct sex values
  geom_point(
    size = 3/4, color = "#004444"
  )+
  geom_point(
    aes(alpha = prop_neverinunion == 0),
    size = 4, color = "#004444"
  )+
  scale_alpha_manual(values = c(1, .5))+
  # geom_flag(
  #   # data = . %>% filter(sex == "Men"),
  #   x = -.01, aes(country = iso2 %>% tolower), size = 4
  # ) +
  geom_moon(aes(ratio = prop_neverinunion, fill = sex, right = FALSE), size = 4, color = NA)+
  facet_wrap(~ sex, nrow = 1)+
  scale_fill_manual(values = c("#dfff00", "#00FFFF"))+
  scale_color_manual(values = gap_colors)+
  scale_x_continuous(position = "top", limits = c(0, .42))+
  theme(
    plot.title = element_text(size = 24, face = 2,  hjust = .5, family = "ah"), 
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    strip.text = element_blank(),
    axis.text = element_text(face = 2), 
    axis.title = element_text(face = 2)
  )+
  labs(
    x = "proportion of childless population aged 35+",
    y = NULL,
    title = "Not having kids is often driven by inability to form a union", 
    caption = "\nData: GGS wave 1, most recent DHS, and 1992 HDI; Sample: Individuals aged 35+ born in 1960s"
  )+
  geom_text(
    data = tibble(sex = c("Men", "Women"), sign = c("♂", "♀")),
    aes(label = sign),
    x = .005, y = 62,
    size = 20, hjust = 0, colour = c("#687807FF", "#017979FF"), 
    family = "Roboto", fontface = 2
  )

main <- last_plot()

# inset map of world regions
world_outline_robinson %>% 
  ggplot()+
  geom_sf(aes(fill = continent), color = NA)+
  geom_sf(data = country_borders, color = "#ccffff", size = .25)+
  scale_fill_manual(values = gap_colors, na.value = "#00444499")+
  theme_void()+
  theme(legend.position = "none")

inset <- last_plot()

# legend
tibble(
  prop = c(.2, .4, .7),
  x = c(.35, .5, .65),
  y = .5,
  perc = (prop * 100) %>% paste0("%")
) %>%
  ggplot(aes(x, y))+
  geom_point(size = 4, color = "#004444")+ 
  geom_moon(aes(ratio = prop, right = FALSE), size = 4, fill = "#dfff00", color = NA)+
  geom_text(aes(label = perc), y = .4, family = "ah", fontface = 2, color = "#004444", size = 5.5)+
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)+
  theme_void()+
  annotate(
    "label", x = .5, y = .64, label = "among childless population\nproportion of those who have\nnever been in a union\nby age 35\n \n \n ", lineheight = .7, fill = NA,
    family = "ah", fontface = 2, color = "#004444", size = 6
  )

legend <- last_plot()

# assemble
(
  out <- ggdraw(main)+
    draw_plot(inset, x = .15, width = .4, y = -.05, height = .3)+
    draw_plot(legend, x = .1, width = .5, y = .33, height = .25)
)

ggsave("out/fig.pdf", plot = out, width = 10, height = 10)
ggsave("out/fig.png", plot = out, width = 10, height = 10)
  