
library(tidytuesdayR)
library(tidyverse)
library(showtext)


# data from https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-12-22/readme.md

big_mac <- read.csv("big-mac-full-index.csv")

# extract year from date

big_mac$date <- as.Date(big_mac$date, format = "%Y-%m-%d")
big_mac$year <- format(as.Date(big_mac$date), "%Y")

# calculate quantile's 
big_mac_sum <- big_mac %>%
  select(year, dollar_price) %>%
  group_by(year) %>%
  summarise(x = quantile(dollar_price, c(0.05, 0.25, 0.5, 0.75, 0.95)), q = c("p_5", "p_25", "med", "p_75", "p_95")) %>%
  ungroup()

# change from long to wide format
sum_l <- big_mac_sum %>%
  pivot_wider(names_from = q, values_from = x)

# mess around with dates (again...) to extract year
sum_l$year2 <- format(as.Date(sum_l$year), "%Y")
sum_l$year3 <- as.numeric(sum_l$year2)

font_add_google("Montserrat", "Montserrat")
showtext_auto()

ggplot(sum_l) +
  geom_errorbar(aes(x = year3, ymin = p_5, ymax = p_95), size = 1.5, colour = "#9CD3BA") +
  geom_errorbar(aes(x = year3, ymin = p_25, ymax = p_75), size = 2, colour = "#62BC93") +
  geom_point(mapping = aes(x = year3, y = med), size = 5, colour = "#2d6a4f") +
  geom_smooth(aes(x = year3, y = med), colour = "#f49b53", se = FALSE) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, by = 1), labels = seq(0, 8, by = 1)) +
  scale_x_continuous(limits = c(1998, 2025),  breaks = seq(2000, 2021, by = 1)) +
  labs(y = "US $ \n",
       title = "Big Mac Index \n",
       subtitle = "Trend of the price of a McDonald's Big Mac around the world, \n in US Dollars, from 2000 to 2021",
       caption = "\n Data from TidyTuesday 2020 | Week 52") +
  annotate("text", x = 2023, y = 3.65, label = "median", colour = "#2d6a4f", family = "Montserrat", fontface = "bold", size = 6) +
  annotate("text", x = 2024, y = 2.8, label = "25th percentile", colour = "#62BC93", family = "Montserrat", fontface = "bold", size = 5.5) +
  annotate("text", x = 2024, y = 4.2, label = "75th percentile", colour = "#62BC93", family = "Montserrat", fontface = "bold", size = 5.5) +
  annotate("text", x = 2024, y = 2.15, label = "5th percentile", colour = "#9CD3BA", family = "Montserrat", fontface = "bold", size = 5.5) +
  annotate("text", x = 2024, y = 5.8, label = "95th percentile", colour = "#9CD3BA", family = "Montserrat", fontface = "bold", size = 5.5) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#2F4858"),
        #axis.line.x = element_line(colour = "#2F4858"),
        axis.ticks.x = element_line(colour = "#2F4858"),
        axis.text.x = element_text(size = 8, face = "bold", colour = "#2F4858"),
        axis.text.y = element_text(size = 12, face = "bold",  colour = "#2F4858"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5, lineheight = 1.2),
        text = element_text(family = "Montserrat", colour = "#2F4858"),
        plot.margin = unit(c(0.2, 1, 0.2, 0.4), "cm"),
        plot.caption = element_text(size = 8))

ggsave("day_26_trends.png", width = 13, height = 8)
