library(tidyverse)
library(rayshader)
library(showtext)

# for data cleaning see day_23.R

font_add_google("Montserrat", "Montserrat")
showtext_auto()

ggplot(new, aes(x = day, y = month)) +
  geom_tile(aes(fill = value)) +
  scale_y_discrete(labels = c("Oct-19", "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "Dec-20", "Jan-21", "Feb-21", "Mar-21", "Apr-21")) +
  scale_fill_gradient(name = "Steps per Day", low = "#DFF6F4", high = "#2a9d8f") +
  labs(x = "Day") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        text = element_text(family = "Montserrat", colour = "#2F4858"),
        axis.title.x = element_text(size =12, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold", colour = "#2F4858"),
        axis.text.x = element_text(size = 10, face = "bold", colour = "#2F4858"),
        legend.title = element_text(size = 12, face = "bold"),
        panel.grid = element_blank())

#render 3D plot
plot_gg(steps, width = 8, height = 8, zoom = 0.8, scale = 750, theta = -45)

#save as video
render_movie("day_30.mp4")
# save as static
render_snapshot("day_30_3d.png")

#2D version
ggsave("day_30.png", width = 10, height = 8)
