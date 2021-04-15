
library(tidyverse)
library(showtext)

planets <- read.csv("planets.csv") 

planets$diameter._km <- as.numeric(planets$diameter._km)

font_add_google("Montserrat", "Montserrat")
showtext_auto()

ggplot(planets, aes(x = order_from_sun, y = no_of_moons)) +
  geom_point(data = planets %>% filter(order_from_sun == 1), aes(x = order_from_sun, y = no_of_moons), size = 4, shape = 21, fill = "#5C5947", alpha = 0.9, colour = "#5C5947") +
  geom_point(data = planets  %>% filter(order_from_sun == 2), aes(x = order_from_sun, y = no_of_moons), size = 10, shape = 21, fill = "#c27847", alpha = 0.9, colour = "#c27847") +
  geom_point(data = planets %>% filter(order_from_sun == 3), aes(x = order_from_sun, y = no_of_moons), size = 11, shape = 21, fill = "#254A7E", alpha = 0.9, colour = "#254A7E") +
  geom_point(data = planets  %>% filter(order_from_sun == 4), aes(x = order_from_sun, y = no_of_moons), size = 6, shape = 21, fill = "#cc4700", alpha = 0.9, colour = "#cc4700") +
  geom_point(data = planets  %>% filter(order_from_sun == 5), aes(x = order_from_sun, y = no_of_moons), size = 110, shape = 21, fill = "#9c977f", alpha = 0.9, colour = "#9c977f") +
  geom_point(data = planets  %>% filter(order_from_sun == 6), aes(x = order_from_sun, y = no_of_moons), size = 99, shape = 21, fill = "#c7ab84", alpha = 0.9, colour = "#c7ab84") +
  geom_point(data = planets  %>% filter(order_from_sun == 7), aes(x = order_from_sun, y = no_of_moons), size = 42, shape = 21, fill = "#cdeff8", alpha = 0.9, colour = "#cdeff8") +
  geom_point(data = planets  %>% filter(order_from_sun == 8), aes(x = order_from_sun, y = no_of_moons), size = 40, shape = 21, fill = "#9b8ffe", alpha = 0.9, colour = "#9b8ffe") +
  ylim(-5, 110) +
  annotate("text", x = 1, y = 16, label = "Type: Rock \n Moons: 0 \n Diameter: \n 4,900 km", colour = "white", size = 3.5, family = "Montserrat", hjust = 0.5) +
  annotate("text", x = 2, y = 17, label = "Type: Rock \n Moons: 0 \n Diameter: \n 12,100 km", colour = "white", size = 3.5, family = "Montserrat", hjust = 0.5) +
  annotate("text", x = 3, y = 20, label = "Type: Rock \n Moons: 1 \n Diameter: \n 12,800 km", colour = "white", size = 3.5, family = "Montserrat", hjust = 0.5) +
  annotate("text", x = 4, y = 18, label = "Type: Rock \n Moons: 2 \n Diameter: \n 6,700 km", colour = "white", size = 3.5, family = "Montserrat", hjust = 0.5) +
  annotate("text", x = 5, y = 32, label = "Type: Gas \n Moons: 79 \n Diameter: \n 143,000 km  \n", colour = "white", size = 3.5, family = "Montserrat", hjust = 0.5) +
  annotate("text", x = 6, y = 32, label = "Type: Gas \n Moons: 82 \n Diameter: \n 120,500 km  \n", colour = "white", size = 3.5, family = "Montserrat", hjust = 0.5) +
  annotate("text", x = 7, y = -1.5, label = "Type: Ice \n Moons: 27 \n Diameter: \n 51,100 km  \n", colour = "white", size = 3.5, family = "Montserrat", hjust = 0.5) +
  annotate("text", x = 8, y = 40, label = "Type: Ice \n Moons: 14 \n Diameter: \n 49,500 km  \n ", colour = "white", size = 3.5, family = "Montserrat", hjust = 0.5) +
  labs(y = "Number of Moons \n", x = "Order from the Sun",
       title = " \n Planets of Our Solar System \n",
       subtitle = "Relationship between the number of moons, planet type and diameter") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), labels = c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "")) +
  theme(plot.background = element_rect(fill = "#586c79", colour = "#586c79"),
        panel.background = element_rect(fill = "#586c79", colour = "#586c79"),
        text = element_text(family = "Montserrat", colour = "white"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(colour = "white", face = "bold", size = 14),
        axis.text.y = element_text(colour = "white", size = 14),
        axis.title.y = element_text(colour = "white", face = "bold", size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #panel.grid.major.y = element_blank(),
        #panel.grid.minor.y = element_line(colour = "white", size = 0.1),
        panel.grid.major.y = element_line(colour = "white", size = 0.1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(size = 18),
        plot.margin = unit(c(0, 0.5, 2, 1), "cm"))

ggsave("day_14_space.png", width = 14.5, height = 8)
