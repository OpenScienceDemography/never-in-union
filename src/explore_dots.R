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

col7 <- c("#332288", "#88CCEE", "#117733", "#999933", "#FD8D3C", "#882255", "#DDDDDD")

# load the prepared dataset
load("out/never.rda")

never <- never %>% 
  mutate(country = case_when(country == "Democratic Republic of the Congo" ~ "D.R.Congo", 
                             T~ country),
         prop_childless = prop_childless * 100,
         prop_neverinunion = prop_neverinunion * 100)

# % of childless
level_country <- never %>% 
  filter(sex == "Men") %>% 
  arrange(prop_childless)

country_num <- level_country %>% 
  filter(sex == "Men") %>% 
  select(country) %>% 
  mutate(country_num = n():1) %>% 
  filter(country_num %in% seq(5, n(), 5))

never %>% 
  mutate(country = factor(country, levels = level_country$country)) %>% 
  ggplot(aes(x = prop_childless, y = country, group = sex, colour = sex)) +
  geom_hline(yintercept = country_num$country, size = 2, color = "#eaeaea") +
  geom_line(aes(group = country), colour = "grey") +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c(col7[3], col7[5])) +
  labs(x = "% of childless population among total population") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, face = 2))
ggsave("out/childless_sex.png", width = 7.5, height = 8, bg = "white")

# two props
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
  scale_colour_manual(values = col7[1:5]) +
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
