library(tidyverse)
library(showtext)

# data from https://population.un.org/wpp/Download/Probabilistic/Population/

data <- read.csv("pop_proj_un.csv")

font_add_google("Montserrat", "Montserrat")
showtext_auto()

ggplot(data) +
  geom_smooth(aes(x = year, y = lower_80), colour = "#fb8b24", lty = "dashed") +
  geom_smooth(aes(x = year, y = upper_80), colour = "#fb8b24", lty = "dashed") +
  geom_smooth(aes(x = year, y = lower_95), colour = "#9a031e", lty = "dashed") +
  geom_smooth(aes(x = year, y = upper_95), colour = "#9a031e", lty = "dashed") +
  geom_ribbon(aes(x = year, ymin = lower_95, ymax = upper_95), fill = "#9a031e", alpha = 0.2) +
  geom_ribbon(aes(x = year, ymin = lower_80, ymax = upper_80), fill = "#fb8b24", alpha = 0.5) +
  geom_smooth(aes(x = year, y = median), colour = "#0f4c5c") +
  scale_x_continuous(limits = c(2015, 2125), breaks = seq(2020, 2100, by = 10), labels = seq(2020, 2100, by = 10)) +
  scale_y_continuous(limits = c(7500000, 13000000), breaks = seq(8000000, 13000000, by = 1000000), labels = c("8 billion", "9 billion", "10 billion", "11 billion", "12 billion", "13 billion")) +
  geom_text(data %>% filter(year == 2100), mapping = aes(year +5, median), label = "Median", colour = "#0f4c5c", family = "Montserrat", size = 4, fontface = "bold") +
  geom_text(data %>% filter(year == 2100), mapping = aes(year +15, lower_80), label = "Lower 80% prediction interval", colour = "#fb8b24", family = "Montserrat", size = 4, fontface = "bold") +
  geom_text(data %>% filter(year == 2100), mapping = aes(year +15, upper_80), label = "Upper 80% prediction interval", colour = "#fb8b24", family = "Montserrat", size = 4, fontface = "bold") +
  geom_text(data %>% filter(year == 2100), mapping = aes(year +15, lower_95), label = "Lower 95% prediction interval", colour = "#9a031e", family = "Montserrat", size = 4, fontface = "bold") +
  geom_text(data %>% filter(year == 2100), mapping = aes(year +15, upper_95), label = "Upper 95% prediction interval", colour = "#9a031e", family = "Montserrat", size = 4, fontface = "bold") +
  labs(y = "Global Population \n",
      title = "Global Population Projections \n", 
       subtitle = "UN probabilistic population projections from 2020 to 2100, \n including 80% and 95% prediction intervals \n",
       caption = "\n Data from United Nations, Department of Economic and Social Affairs, Population Division (2019)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 14, vjust = 4),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16, lineheight = 1.2),
        plot.caption = element_text(size = 8, vjust = -5),
        text = element_text(family = "Montserrat", colour = "#2F4858"),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "#2F4858"),
        axis.ticks = element_line(colour = "#2F4858"),
        axis.text.x = element_text(size = 14, face = "bold", vjust = -2),
        axis.text.y = element_text(size = 12, face = "bold", hjust = -1),
        plot.margin = unit(c(0.5, 0.5, 1, 2), "cm"))

ggsave("day_25_demographics.png", width = 13, height = 8)
