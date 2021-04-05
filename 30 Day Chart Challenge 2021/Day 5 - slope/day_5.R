library(tidyverse)
library(showtext)
library(ggrepel)

elev <- read.csv("sa_country_elev.csv")

sa <- elev %>%
  filter(country %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela"))

font_add_google("Montserrat", "Montserrat")
showtext_auto()

ggplot(sa, aes(x = elevation, y = m, group = country)) +
  geom_line(aes(colour = country), size = 1, alpha = 0.8) +
  geom_point(aes(colour = country), size = 3, alpha = 0.8) +
  scale_colour_manual(values = c("#83B692", "#F9ADA0", "#F9627D", "#C65B7C", "#5B3758", "#8332AC", "#462749", "#153243", "#284B63", "#645DD7", "#FFB100", "#F2542D")) +
  scale_x_discrete(position = "bottom", labels = c("lowest", "average", "highest")) +
  scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000), labels = c("      0 m", "1,000 m", "2,000 m", "3,000 m", "4,000 m", "5,000 m", "6,000 m")) +
  geom_text_repel(data = sa %>% filter(elevation == "c"), mapping = aes(y = m, label = country), size = 5, fontface = "bold", force = 5, colour = c("#83B692", "#F9ADA0", "#F9627D", "#C65B7C", "#5B3758", "#8332AC", "#462749", "#153243", "#284B63", "#645DD7", "#FFB100", "#F2542D"), nudge_x = 0.25) +
  labs(title = "Elevation of South American Countries \n",
       subtitle = "The lowest, average and highest points of elevation in metres \n",
       caption = "Data from Wikipedia") +
  theme_minimal() +
  theme(text = element_text(family = "Montserrat"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 2.5, size = 14, face = "bold", colour = "#2F4858"),
        plot.subtitle = element_text(size = 16, colour = "#2F4858"),
        plot.caption = element_text(size = 10, colour = "#2F4858"),
        plot.title = element_text(size = 22,face = "bold", colour = "#2F4858"),
        axis.text.x = element_text(size = 14, face = "bold", colour = "#2F4858"),
        legend.position = "none")

ggsave("day_5_slopechart.jpeg", plot = last_plot(), width = 10, height = 8)
