setwd("demography-viz/population-pyramids/")
library(tidyverse)
library(geofacet)
library(kani)
library(gganimate)
library(tweenr)
options(scipen = 99)

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
pyr_plot <- asia %>%
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
pyr_plot
animation::ani.options(loop = 0)
gganimate(pyr_plot, filename = "attempt2.gif", interval = 0.3, ani.width = 800)


# geofacet plots

asia_grid <- data.frame(
  row = c(1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 8, 9, 9),
  col = c(3, 8, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 2, 3, 4, 5, 7, 9, 10, 11, 3, 4, 9, 10, 12, 7, 9, 11, 10, 11, 12),
  code = c("GEO", "KAZ", "TUR", "ARM", "AZB", "TKM", "AFG", "UZB", "KGZ", "MNG", "PRK", "JPN", "CYP", "LBN", "SYR", "IRQ", "IRN", "PAK", "TJK", "NPL", "CHN", "KOR", "PSE", "JOR", "KWT", "BHR", "IND", "BGD", "BTN", "MAC", "HKG", "TWN", "ISR", "SAU", "QAT", "ARE", "LKA", "MMR", "LAO", "VNM", "YEM", "OMN", "THA", "KHM", "PHL", "MDV", "MYS", "BRN", "SGP", "IDN", "TLS"),
  name = c("Georgia", "Kazakhstan", "Turkey", "Armenia", "Azerbaijan", "Turkmenistan", "Afghanistan", "Uzbekistan", "Kyrgyzstan", "Mongolia", "Dem. People's Republic of Korea", "Japan", "Cyprus", "Lebanon", "Syrian Arab Republic", "Iraq", "Iran (Islamic Republic of)", "Pakistan", "Tajikistan", "Nepal", "China", "Republic of Korea", "State of Palestine", "Jordan", "Kuwait", "Bahrain", "India", "Bangladesh", "Bhutan", "Macao SAR", "Hong Kong SAR", "Taiwan Province of China", "Israel", "Saudi Arabia", "Qatar", "United Arab Emirates", "Sri Lanka", "Myanmar", "Lao People's Democratic Republic", "Viet Nam", "Yemen", "Oman", "Thailand", "Cambodia", "Philippines", "Maldives", "Malaysia", "Brunei Darussalam", "Singapore", "Indonesia", "Timor-Leste"),
  stringsAsFactors = FALSE
)

asia %>%
  # filter(Area %in% c("India", "China")) %>%
  filter(Year == "2000") %>%
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
  facet_geo(~Area, grid = asia_grid, label = "code") +
  theme_kani() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    # axis.text = element_text(size = rel(1.2)),
    # strip.text = element_text(size = rel(1.2), face = "bold"),
    axis.title = element_text(size = rel(1.2)),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white")
    # strip.background = element_rect(fill = "white")
  ) +
  labs(
    title = "Population Pyramids for Asian Countries",
    subtitle = "Share of populations for males and females\nas percentages within gender",
    y = "Share",
    x = "Age Group"
  )

ggsave("asia_pyr.png", height = 20, width = 22)
