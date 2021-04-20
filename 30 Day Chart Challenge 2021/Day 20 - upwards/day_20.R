library(tidyverse)
library(lubridate)
library(showtext)

# data from Met Office https://www.metoffice.gov.uk/research/climate/maps-and-data/uk-and-regional-series

mean <- read.csv("uk_mean.csv")
max <- read.csv("uk_max.csv")
min <- read.csv("uk_min.csv")

# change data from wide to long format
mean_l <- mean %>%
  pivot_longer(cols = 2:13,
             names_to = "month",
             values_to = "mean")

max_l <- max %>%
  pivot_longer(cols = 2:13,
               names_to = "month",
               values_to = "max")

min$may <- as.numeric(min$may)
min$aug <- as.numeric(min$aug)

min_l <- min %>%
  pivot_longer(cols = 2:13,
               names_to = "month",
               values_to = "min")
# join together

all <- mean_l %>%
  left_join(min_l, by = c("year", "month"))

all <- all %>%
  left_join(max_l, by = c("year", "month")) %>%
  filter(year < 2021)

all <- all %>%
  tidyr::unite(y_m_3, c(month, year), sep = "-") %>%
  dplyr::mutate(y_m_3 = lubridate::parse_date_time(y_m_3, "my"))

all_1950 <- all %>%
  filter(y_m_3 >= 1950-01-01)

all_sum <-  all %>%
  group_by(year) %>%
  filter(!is.na(min)) %>%
  summarise(mean = mean(mean),
            min = mean(min),
            max = mean(max.y)) %>%
  ungroup()


font_add_google("Montserrat", "Montserrat")
showtext_auto()

ylab <- expression("Temperature ("*degree*C*")")

ggplot(all_sum) +
  geom_ribbon(aes(x = year, ymin = min, ymax = max), alpha = 0.5, fill = "white") +
  geom_point(aes(x = year, y = min), colour = "#277da1", size= 2) +
  geom_line(aes(x = year, y = min), colour = "#277da1", size = 0.5) +
  geom_point(aes(x = year, y = max), colour = "#f94144", size = 2) +
  geom_line(aes(x = year, y = max), colour = "#f94144", size = 0.5) +
  #geom_segment(aes(x = year, xend = year, y = min, yend = max), colour = "grey", alpha = 0.5) +
  geom_point(aes(x = year, y = mean), colour = "#00664D", size = 3, alpha = 0.5) +
  geom_smooth(aes(x = year, y = mean), method=lm, se=FALSE, colour = "#00664D") +
  scale_x_continuous(breaks = c(1880, 1900, 1920, 1940, 1960, 1980, 2000, 2020), labels = c("1880", "1900", "1920", "1940", "1960", "1980", "2000", "2020")) +
  scale_y_continuous(breaks = c(2,3, 4, 5,6, 7,8, 9,10, 11,12, 13,14), labels = c("2", "", "4","", "6","", "8", "", "10","", "12","", "14")) +
  labs(x = "\n Year", 
       y = ylab,
       title = "Average Yearly Temperature in the UK \n",
       subtitle = "The average temperature has risen from 8.5 degrees in 1884 \n to 9.6 degrees in 2020, an increase of 12.9%.",
       caption = "Data from the Met Office") +
  annotate("text", x = 1910, y = 5.65, label = "minimum", colour = "#277da1", family = "Montserrat", fontface = "bold", size = 5) +
  annotate("text", x = 1910, y = 10.3, label = "maximum", colour = "#f94144", family = "Montserrat", fontface = "bold", size = 5) +
  annotate("text", x = 1975, y = 9.5, label = "mean", colour = "#00664D", family = "Montserrat", fontface = "bold", size = 5) +
  theme_minimal() +
  theme(plot.background = element_rect(colour = "#2F4858", fill = "#2F4858"),
        text = element_text(family = "Montserrat", colour = "white"),
        axis.text = element_text(colour = "white", size = 14),
        axis.title = element_text(size = 18),
        panel.grid = element_blank(),
        axis.ticks = element_line(colour = "white"),
        axis.line = element_line(colour = "white"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5, lineheight = 1.2),
        plot.caption = element_text(size = 10),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

ggsave("day_20_up.png", width = 9, height = 9)
