library(tidyverse)
library(showtext)
library(ggbump)

# data from wikipedia https://en.wikipedia.org/wiki/Women%27s_Cricket_World_Cup

cricket <- read.csv("cricket.csv")

font_add_google("Montserrat", "Montserrat")
showtext_auto()

ggplot(cricket, aes(year, position, colour = team)) +
  geom_point(size = 8) +
  geom_text(data = cricket %>% filter(year == min(year)),
            aes(x = year - .4, label = team), size = 5, hjust = 1, fontface = "bold") +
  geom_text(data = cricket %>% filter(year == max(year)),
            aes(x = year + .4, label = team), size = 5, hjust = 0, fontface = "bold") +
  geom_bump(size = 2, smooth = 8) +
  geom_point(data = cricket %>% filter(host == 1), mapping = aes(year, position), size = 5, colour = "white", alpha = 0.8) +
  scale_x_continuous(limits = c(1998, 2018), breaks = c(2000, 2005, 2009, 2013, 2017), labels = c("2000", "2005", "2009", "2013", "2017")) +
  scale_y_reverse(limits = c(8.7, 1), breaks = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("1st", "2nd", "3rd", "4th", "5th" , "6th", "7th", "8th")) +
  scale_color_manual(values = c("#ffd60a", "#d90429", "#0085CC", "#aacc00", "#f77f00", "black", "#40916c", "#004b23", "#023e8a", "#7C1354")) +
  labs(title = "ICC Women's Cricket World Cup \n",
       subtitle = "Finishing positions of teams competing in the World Cup tournaments this century. \n
       The host countries are highlighted. \n",
       caption = "\n Data from Wikipedia") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        text = element_text(family = "Montserrat", colour = "#2F4858"),
        plot.background = element_rect(fill = "white", colour = "white"),
        axis.text = element_text(colour = "#2F4858", size = 18, face = "bold"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 20, hjust = 0.5, lineheight = 1.15),
        plot.caption = element_text(size = 10), 
        plot.margin = unit(c(1, 1, 0.5, 1), "cm"))

ggsave("day_18_connections.png", width = 18, height = 9)
