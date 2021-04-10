library(tidyverse)
library(ggridges)
library(showtext)

tdf_stage <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_stages.csv')

# extract year from date

tdf_stage <- tdf_stage %>%
  mutate(year = year(Date))

font_add_google("Montserrat", "Montserrat")
showtext_auto()

#plot
ggplot(tdf_stage, aes(x = Distance, y = as.factor(year), group = as.factor(year))) +
  geom_density_ridges(aes(colour = year), scale = 8, fill = "#2F4858") +
  #scale_colour_gradient2(low = "#ffea00", mid = "white", high = "#ffea00", midpoint = 1968) +
  scale_colour_gradient2(low = "white", high = "white", mid = "#ffea00", midpoint = 1963) +
  labs(x = "\n Distance (km)", y = "", title = "Tour de France Stages \n",
       subtitle = "Distribution of the distance (km) of each \n stage in every year of the Tour de France \n",
       caption = "Data from TidyTuesday 2020 - Week 15") +
  scale_y_discrete(breaks = c("1903", "1965", "2017"), labels = c("1903", "1965", "2017")) +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 500), labels = c("0", "", "200", "", "400", "")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Montserrat"),
        plot.background = element_rect(fill = "#2F4858", colour = "#2F4858"),
        legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(colour = "white", size = 16, face = "bold"),
        axis.text.x = element_text(colour = "white", size = 14),
        axis.text.y = element_text(colour = "white", size = 14),
        axis.ticks.x = element_line(colour = "white"),
        plot.title = element_text(colour = "white", size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(colour = "white", size = 14, hjust = 0.5, lineheight = 1.2),
        plot.caption = element_text(colour = "white", size = 8))

ggsave("day_10_density.png", width = 10, height = 10)
