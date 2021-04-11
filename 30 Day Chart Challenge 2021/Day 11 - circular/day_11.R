library(tidyverse)
library(showtext)

flag <- read.csv("flag_cols.csv")

flag$order <- seq(1:10)
flag$height <- 100

ggplot(flag, aes(x = colour, y = no)) +
  geom_point(data = flag %>% filter(order == 1), aes(x = order, y = height), size = 80, shape = 21, fill = "#bf1a2f", alpha = 0.8, colour = "#bf1a2f") +
  geom_point(data = flag  %>% filter(order == 2), aes(x = order, y = height), size = 60, shape = 21, fill = "white", alpha = 0.8, colour = "white") +
  geom_point(data = flag %>% filter(order == 3), aes(x = order, y = height), size = 40, shape = 21, fill = "#19647E", alpha = 0.8, colour = "#19647E") +
  geom_point(data = flag  %>% filter(order == 4), aes(x = order, y = height), size = 30, shape = 21, fill = "#FFBE0B", alpha = 0.8, colour = "#FFBE0B") +
  geom_point(data = flag  %>% filter(order == 5), aes(x = order, y = height), size = 25, shape = 21, fill = "#2B9348", alpha = 0.8, colour = "#2B9348") +
  geom_point(data = flag  %>% filter(order == 6), aes(x = order, y = height), size = 20, shape = 21, fill = "black", alpha = 0.8, colour = "black") +
  geom_point(data = flag  %>% filter(order == 7), aes(x = order, y = height), size = 14, shape = 21, fill = "#f17105", alpha = 0.8, colour = "#f17105") +
  geom_point(data = flag  %>% filter(order == 8), aes(x = order, y = height), size = 14, shape = 21, fill = "#823329", alpha = 0.8, colour = "#823329") +
  geom_point(data = flag  %>% filter(order == 9), aes(x = order, y = height), size = 8, shape = 21, fill = "gray", alpha = 0.8, colour = "gray") +
  geom_point(data = flag  %>% filter(order == 10), aes(x = order, y = height), size = 6, shape = 21, fill = "#c683d3", alpha = 0.8, colour = "#593c8f") +
  labs(title = "\n Colours in the Flags of the World",
       subtitle = "Number of flags containing each colour",
       caption = "Data from www.crwflags.com/fotw/flags/xf-csts.html") +
  ylim(0, 200) +
  xlim(0, 11.5) +
  coord_polar() +
  geom_text(flag %>% filter(colour != c("Red", "White", "Blue")), mapping = aes(x = order, y = height, label = no), colour = "white", fontface = "bold") +
  geom_text(flag %>% filter(colour == "White"), mapping = aes(x = order, y = height, label = no, vjust = -1.5), colour = "#2F4858", fontface = "bold") +
  geom_text(flag %>% filter(colour == "Blue"), mapping = aes(x = order, y = height, label = no, vjust = -1.5), colour = "white", fontface = "bold") +
  geom_text(flag %>% filter(colour == "Red"), mapping = aes(x = order, y = height, label = no, vjust = -5.5), colour = "white", fontface = "bold") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Montserrat"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "#2F4858", colour = "#2F4858"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, colour = "white"),
        plot.subtitle = element_text(size = 14, hjust = 0.5, colour = "white"),
        plot.caption = element_text(size = 8, hjust = 0.5, colour = "white")) 


ggsave("day_11_circular.png", width = 6, height = 6)
