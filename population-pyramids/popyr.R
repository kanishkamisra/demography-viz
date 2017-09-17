setwd("demography-viz/population-pyramids/")
library(tidyverse)
library(geofacet)
library(kani)
library(gganimate)
library(tweenr)

asia <- read_csv("asia_population.csv")

# Some basic preprocessing for convenience
asia <- asia %>%
  mutate(Age = stringr::str_replace(Age, "--", "-"))

age_levels <- asia %>%
  distinct(Age) %>%
  pull(Age)

asia$Age <- factor(asia$Age, levels = age_levels)

# For one country

india <- asia %>%
  filter(Area == "India")

india %>%
  # filter(Year == "1970") %>%
  group_by(Year, Sex) %>%
  mutate(share = Population/sum(Population)) %>%
  ungroup() %>%
  mutate(share = case_when(
    Sex == "Female" ~ -share,
    TRUE ~ share
  )) %>%
  ggplot(aes(Age, share, fill = Sex)) + 
  geom_bar(stat = 'identity', width = 1, alpha = 0.8) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10), labels = function(x){ paste0(abs(x)*100, "%") }) +
  # scale_x_discrete(breaks = scales::pretty_breaks(n=10)) +
  coord_flip() + 
  facet_wrap(~Year, ncol = 3) +
  theme_kani() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text = element_text(size = rel(1.2)),
    strip.text = element_text(size = rel(1.2), face = "bold"),
    axis.title = element_text(size = rel(1.2)),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white")
  ) +
  labs(
    title = "Population Pyramids for India 1970-2015",
    subtitle = "Share of populations for males and females as percentages within gender",
    y = "Share",
    x = "Age Group"
  )
  
  
ggsave("India-pyr1b.png", height = 21, width = 20)

# Animation?

india_plot <- asia %>%
  filter(Area %in% c("India", "China")) %>%
  # filter(Year == "2000") %>%
  group_by(Area, Year, Sex) %>%
  mutate(share = Population/sum(Population)) %>%
  ungroup() %>%
  mutate(share = case_when(
    Sex == "Female" ~ -share,
    TRUE ~ share
  )) %>%
  ggplot(aes(Age, share, fill = Sex, frame = Year)) + 
  geom_bar(stat = 'identity', width = 1, alpha = 0.8, position = "identity") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10), labels = function(x){ paste0(abs(x)*100, "%") }) +
  # scale_x_discrete(breaks = scales::pretty_breaks(n=10)) +
  coord_flip() + 
  facet_wrap(~Area) +
  theme_kani() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text = element_text(size = rel(1.2)),
    # strip.text = element_text(size = rel(1.2), face = "bold"),
    axis.title = element_text(size = rel(1.2)),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white")
    # strip.background = element_rect(fill = "white")
  ) +
  labs(
    title = "Population Pyramids for India and China in",
    subtitle = "Share of populations for males and females\nas percentages within gender",
    y = "Share",
    x = "Age Group"
  )

animation::ani.options(loop = 0)
gganimate(india_plot, filename = "attempt2.gif", interval = 0.3, ani.width = 800)


