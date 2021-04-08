library(palmerpenguins)
library(tidyverse)
library(patchwork)
library(ggtext)

font_add_google("Montserrat", "Montserrat")
showtext_auto()

 a <- ggplot() +
  geom_histogram(data = filter(penguins, species == "Adelie"), mapping = aes(y = bill_length_mm, x = ..count..), binwidth = 1.5, colour = "white", fill = "#967AA1") +
  geom_histogram(data = filter(penguins, species == "Gentoo"), mapping = aes(y = bill_length_mm, x = -..count..), binwidth = 1.5, colour = "white", fill = "#26407D") +
  labs(y = "millimetres", title = "Bill Length") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", colour = "#2F4858", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(family = "Montserrat"),
        axis.text.y = element_text(size = 12, colour = "#2F4858"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "#2F4858"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

b <- ggplot() +
  geom_histogram(data = filter(penguins, species == "Adelie"), mapping = aes(y = body_mass_g, x = ..count..), binwidth = 200, colour = "white", fill = "#967AA1") +
  geom_histogram(data = filter(penguins, species == "Gentoo"), mapping = aes(y = body_mass_g, x = -..count..), binwidth = 200, colour = "white", fill = "#26407D") +
  labs(y = "grams", title = "Body Mass") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", colour = "#2F4858", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(family = "Montserrat"),
        axis.text.y = element_text(size = 12, colour = "#2F4858"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "#2F4858"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

c <- ggplot() +
  geom_histogram(data = filter(penguins, species == "Adelie"), mapping = aes(y = flipper_length_mm, x = ..count..), binwidth = 3, colour = "white", fill = "#967AA1") +
  geom_histogram(data = filter(penguins, species == "Gentoo"), mapping = aes(y = flipper_length_mm, x = -..count..), binwidth = 3, colour = "white", fill = "#26407D") +
  labs(y = "millimetres", title = "Flipper Length") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", colour = "#2F4858", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(family = "Montserrat"),
        axis.text.y = element_text(size = 12, colour = "#2F4858"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "#2F4858"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

d <- ggplot() +
  geom_histogram(data = filter(penguins, species == "Adelie"), mapping = aes(y = bill_depth_mm, x = ..count..), binwidth = 0.5, colour = "white", fill = "#967AA1") +
  geom_histogram(data = filter(penguins, species == "Gentoo"), mapping = aes(y = bill_depth_mm, x = -..count..), binwidth = 0.5, colour = "white", fill = "#26407D") +
  labs(y = "millimetres", title = "Bill Depth") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", colour = "#2F4858", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(family = "Montserrat"),
        axis.text.y = element_text(size = 12, colour = "#2F4858"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "#2F4858"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

p <- (a + d)/(c + b)

p + plot_annotation(title="\n Penguin Species Characteristics \n", 
                                      subtitle = "Distribution of physical characteristics of <br> 
                                      <span style='color:#26407D;'><strong>Gentoo</strong></span> and <span style='color:#967AA1;'><strong>Adelie</strong></span>  <br> penguins <br>",
                                      theme = theme(text = element_text('Montserrat',  colour = "#535657"),
                                                    plot.title = element_text(face = "bold", size = 22, hjust = 0.5, lineheight = 1.5),
                                                    plot.subtitle = element_markdown(size = 18, hjust = 0.5, lineheight = 1.5)))
                                                    #plot.margin = unit(c(1.5, 0, 0, 0), "cm")))

ggsave("day_8_penguins.png", width = 10, height = 10)
