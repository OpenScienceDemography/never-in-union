#### try to explore a scatter version to show our findings

# prepare the session
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
library(cowplot)
library(ggforce)
library(prismatic)
library(gggibbous)
`%out%` = Negate(`%in%`)

# load the prepared dataset
load("out/never.rda")

never %>% 
  ggplot(aes(x = prop_childless, y = prop_neverinunion, colour = continent)) +
  facet_wrap(~ sex) +
  geom_point() +
  labs(x = "% of childless population among total population",
       y = "% of individuals who have never formed a union among childless population") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/twoprops_dot.png", width = 7.5, height = 6, bg = "white")

never %>% 
  ggplot(aes(x = prop_childless, y = prop_neverinunion, colour = continent)) +
  facet_wrap(~ sex) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  labs(x = "% of childless population among total population",
       y = "% of individuals who have never formed a union among childless population") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/twoprops_dot_fit.png", width = 7.5, height = 6, bg = "white")


# add a fitted line except Europe
fitline <- never %>% 
  filter(continent %out% "Europe") 

ols_m <- lm(prop_neverinunion ~ prop_childless, data = fitline %>% filter(sex == "Men"))
summary(ols_m)

ols_w <- lm(prop_neverinunion ~ prop_childless, data = fitline %>% filter(sex == "Women"))
summary(ols_w)

never %>% 
  filter(continent %out% "Europe") %>% 
  ggplot(aes(x = prop_childless, y = prop_neverinunion)) +
  facet_wrap(~ sex) +
  geom_smooth(method = lm, se = F,
              colour = "#FFEDA0", size = 5, alpha = 0.8) +
  geom_point(aes(colour = continent)) +
  labs(x = "% of childless population among total population",
       y = "% of individuals who have never formed a union among childless population") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/twoprops_dot_fit_excptEurope.png", width = 7.5, height = 6, bg = "white")
