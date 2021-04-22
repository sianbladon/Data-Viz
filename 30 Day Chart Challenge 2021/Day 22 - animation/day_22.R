library(tidyverse)
library(gganimate)

# data from here: https://www.gapminder.org/data/

anim <- ggplot(all_2, aes(x = child_per_w, y = child_mort, size = pop))  +
  geom_point(shape = 21, aes(fill = continent), colour = "#2F4858") +
  scale_size_continuous(range = c(2,30)) +
  scale_fill_manual(name = "Continent", values = c("#83b692",
                              "#f9ada0",
                              "#f9627d",
                              "#8a9eff",
                              "#5b3758",
                              "#8332ac")) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  labs(x = "\n Fertility Rate (babies born per woman)", y = "Child Mortality Rate (deaths per 1,000 live births) \n",
       title = "Fertility Rate vs Child Mortality Rate", subtitle = "Year: {frame_time}",
       caption = "FREE DATA FROM GAPMINDER.ORG, CC-BY LICENSE") +
  theme_minimal() +
  theme(legend.position = "bottom",
        #text = element_text(family = "Montserrat", colour = "#2F4858"),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")) +
  guides(size = FALSE, fill = guide_legend(override.aes = list(size = 6))) +
  transition_time(year)

animate(anim, height = 500, width =500)
anim_save("day_22.gif")
