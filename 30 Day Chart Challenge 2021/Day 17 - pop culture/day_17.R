library(tidyverse)
library(showtext)

tay <- read.csv("taylor.csv")

tay <- tay %>% 
  mutate(streams_mil = streams/1000000,
         cumulative_mil = cumulative/1000000)

font_add_google("Montserrat", "Montserrat")
showtext_auto()

ggplot(tay, aes(day, streams_mil)) +
  geom_point(colour = "#F9627D", size = 9) +
  geom_segment(aes(x = day, xend = day, y = 0, yend = streams_mil), colour = "#83B692", size = 1.5) +
  geom_point(aes(day, cumulative_mil), colour = "#645DD7", size = 7) +
  geom_step(aes(day, cumulative_mil), lty = 6, colour = "#F9ADA0", size = 1.5, alpha = 0.9) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175), labels = c("0", "25", "50", "75", "100", "125", "150", "175")) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), labels = c("1", "2", "3", "4", "5", "6", "7")) +
  labs(x = "Days Since Release", y = "Spotify Streams (millions)",
       title = "Fearless (Taylor's Version) \n",
       subtitle = "In the first week since it was released on 09/04/2021, Taylor Swift's \n rerecorded version of her 2nd album Fearless has been \n streamed over 170 million times on Spotify.\n
       It is the biggest debut of 2021 in the Billboard 200 album chart \n and is Taylor's 9th consecutive #1 album.") +
  annotate("text", x = 2.75, y = 140, label = "Cumulative Streams", colour = "#645DD7", size = 5, fontface = "bold") +
  annotate("text", x = 4.5, y = 60, label = "Daily Streams", colour = "#F9627D", size = 5, fontface = "bold") +
  #annotate("rect", xmin = 2.3, xmax = 3.7, ymin = 135, ymax = 145, fill = "grey", alpha = 0.2) +
  theme_minimal() +
  theme(plot.background = element_rect(colour = "#2F4858", fill = "#2F4858"),
        text = element_text(family = "Montserrat", colour = "white"),
        axis.line.y = element_line(colour = "white"),
        axis.line.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "white", size = 0.1),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "white", size = 12),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5, lineheight = 1.2),
        axis.title = element_text(face = "bold", size = 14),
        plot.margin = unit(c(1, 0.5, 0.5, 1.5), "cm")) 
       

ggsave("day_17_pop.png", width = 10, height = 8)
