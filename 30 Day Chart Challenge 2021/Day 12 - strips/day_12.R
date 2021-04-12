library(tidyverse)
library(showtext)

jimmy <- read.csv("jimmy.csv")

jimmy <- jimmy %>%
  select(test, date, country, ground, bowling_wkts, bowling_aggr) %>%
  mutate(prev_bowl_aggr = lag(bowling_aggr, default = 0)) %>%
  mutate(innings_wkt = bowling_aggr - prev_bowl_aggr)
  
font_add_google("Montserrat", "Montserrat")
showtext_auto() 

ggplot(jimmy, aes(x = innings_wkt, y = country, colour = country)) +
  geom_jitter(position = position_jitter(height = 0, width = 0.4), size = 12, shape = "|", alpha = 0.7) +
  geom_vline(xintercept = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5), colour = "#B0D4B4") +
  labs(x = "Wickets Taken per Innings", title = "Jimmy Anderson's Test Wickets \n",
       subtitle = "Number of wickets taken per innings \n of test cricket, by opposition country \n") +
  scale_colour_manual(aes(colour = country), values = c("#ffea00", "#55a630","#0085CC","black","#2c6e49","#FFD60A","#004A8F","#7C1354","#c2242d")) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7), labels = c("0", "1", "2", "3", "4", "5", "6", "7")) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(family = "Montserrat", colour = "#2F4858"),
        axis.text.y = element_text(size = 12, face = "bold", colour = "#2F4858"),
        axis.text.x = element_text(size = 12, face = "bold", colour = "#2F4858"),
        axis.title.x = element_text(size = 12, face = "bold", colour = "#2F4858", vjust = -1.5),
        plot.title = element_text(size = 18, face = "bold", colour = "#2F4858", lineheight = 1.5),
        plot.subtitle = element_text(size = 16, colour = "#2F4858", lineheight = 1.3))

ggsave("day_12_strip.png", width = 8, height = 8)
