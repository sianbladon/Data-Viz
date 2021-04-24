
library(tidyverse)
library(showtext)
library(patchwork)
library(ggtext)

#data from here: https://tunecaster.com/special/songs-in-color/hit-black.html

songs <- read.csv("songs_2.csv")

font_add_google("Montserrat", "Montserrat")
showtext_auto()

black <- ggplot() +
  geom_bar(songs %>% filter(colour == "black"), mapping = aes(y = year), fill = "black") +
  scale_y_continuous(limits = c(1956, 2014), breaks = seq(1957, 2013, by = 1), label = seq(1957, 2013, by = 1), expand = c(0, 0.05)) +
  scale_x_continuous(limits = c(0, 4), label = c("0", "1", "2", "3", "4")) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(colour = "white", hjust = 0.5, size = 14, face = "bold"),
        text = element_text(family = "Montserrat", colour = "white"),
        plot.background = element_rect(fill = "#6D7E8A", colour = "#6D7E8A"),
        panel.background = element_rect(fill = "#6D7E8A", colour = "#6D7E8A"),
        panel.grid = element_blank(),
        axis.text.x = element_text(colour = "white", size = 14),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0, 0, 1, 0), "cm"))

white <- ggplot() +
  geom_bar(songs %>% filter(colour == "white"), mapping = aes(y = year), fill = "white") +
  scale_y_continuous(limits = c(1956, 2014), expand = c(0, 0.05)) +
  #scale_x_discrete() +
  scale_x_reverse() +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "Montserrat", colour = "white"),
        plot.background = element_rect(fill = "#6D7E8A", colour = "#6D7E8A"),
        panel.background = element_rect(fill = "#6D7E8A", colour = "#6D7E8A"),
        panel.grid = element_blank(),
        axis.text.x = element_text(colour = "white", size = 14),
        axis.title.x = element_blank(),
        axis.ticks.y.right = element_line(colour = "white"),
        axis.ticks.length.y.right = unit(1, "cm"),
        plot.margin = unit(c(0, 0, 1, 0), "cm"))

plot <- white + black

plot + plot_annotation(title = "Black and White Songs \n",
                       subtitle = "Number of hit songs each year with the words <br>  <span style='color:black;'><strong>black</strong></span> or <span style='color:white;'><strong>white</strong></span> in  the title, <br> from 1957-2013 <br>",
                       caption = "Data from tunecaster.com",
                       theme = theme(text = element_text('Montserrat',  colour = "white"),
                                     plot.background = element_rect(fill = "#6D7E8A", colour = "#6D7E8A"),
                                     plot.title = element_text(face = "bold", size = 22, hjust = 0.5, lineheight = 1.5),
                                     plot.subtitle = element_markdown(size = 18, hjust = 0.5, lineheight = 1.5),
                                     plot.caption = element_text(size = 12),
                                     plot.margin = unit(c(1, 1, 0.5, 1), "cm")))

ggsave("day_24_monochrome.png", width = 10, height = 15)
