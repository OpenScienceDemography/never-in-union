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


never %>% 
  ggplot(aes(prop_childless, prop_neverinunion, color = sex))+
  geom_point()


never %>% 
  ggplot(aes(prop_childless, country))+
  geom_point(size = 4, color = "#004444")+ # this one is needed to set the coordinate space
  geom_hline(yintercept = seq(1, 80, 2), size = 4, color = "#ccffff")+
  geom_vline(xintercept = 0, size = 2, color = "#004444AA")+
  geom_text(
    data = . %>% filter(sex == "Women"),
    x = .36, aes(label = country %>% tolower), 
    size = 3.5, hjust = 1, family = "ah", fontface = 2,
    color = "#004444AA"
  )+
  geom_point(size = 4, color = "#004444")+
  geom_flag(
    # data = . %>% filter(sex == "Men"),
    x = -.01, aes(country = iso2 %>% tolower), size = 4
  ) +
  geom_moon(aes(ratio = prop_neverinunion, fill = sex, right = FALSE), size = 4, color = NA)+
  facet_wrap(~ sex, nrow = 1)+
  scale_fill_manual(values = c("#dfff00", "#00FFFF"))+
  scale_x_continuous(position = "top")+
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    strip.text = element_blank(),
    axis.text = element_text(face = 2), 
    axis.title = element_text(face = 2)
  )+
  labs(
    x = "Proportion of childlesness",
    y = NULL
  )+
  geom_text(
    data = tibble(sex = c("Men", "Women"), sign = c("♂", "♀")),
    aes(label = sign),
    x = .005, y = 77,
    size = 20, hjust = 0, colour = c("#687807FF", "#017979FF"), 
    family = "Roboto", fontface = 2
  )

ggsave("out/fig.pdf", width = 10, height = 10)
