library(ggspatial)
library(libwgeom)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# data from here https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-05-12/readme.md
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')

coast <- ne_coastline(scale = "medium", returnclass = "sf")

volcano <- volcano %>%
  filter(tectonic_settings != "Unknown") %>%
  mutate(tectonic = case_when(tectonic_settings == "Intraplate / Continental crust (>25 km)" |
                              tectonic_settings == "Intraplate / Intermediate crust (15-25 km)" |
                                tectonic_settings == "Intraplate / Oceanic crust (< 15 km)" ~ "Intraplate",
                                tectonic_settings == "Rift zone / Continental crust (>25 km)" |
                                tectonic_settings == "Rift zone / Intermediate crust (15-25 km)" |
                                tectonic_settings == "Rift zone / Oceanic crust (< 15 km)" ~ "Rift Zone",
                                tectonic_settings == "Subduction zone / Continental crust (>25 km)" |
                                tectonic_settings == "Subduction zone / Crustal thickness unknown" |
                                tectonic_settings == "Subduction zone / Intermediate crust (15-25 km)" |
                                tectonic_settings == "Subduction zone / Oceanic crust (< 15 km)" ~ "Subduction Zone"))
table(volcano$tectonic)

font_add_google("Montserrat", "Montserrat")
showtext_auto()

## plot
ggplot(data = coast) +
  geom_sf(colour = "white", fill = "#2F4858", size = 0.2) +
  geom_point(data = volcano, aes(x = longitude, y = latitude, colour = tectonic),
             size = 2.5, alpha = 0.6) +
  scale_colour_manual(values = c("#d62828", "#f77f00", "#fcbf49")) +
  labs(title = "\n Volcanoes \n",
       caption = "Data from Tidy Tuesday 2020 - Week 20") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#2F4858", colour = "#2F4858"),
        plot.background = element_rect(fill = "#2F4858", colour = "#2F4858"),
        text = element_text(family = "Montserrat"),
        plot.title = element_text(colour = "white", size = 16, face = "bold", lineheight = 2, hjust = 0.5),
        plot.caption = element_text(colour = "white", size = 10),
        legend.position = "top",
        legend.text = element_text(colour = "white", size = 12),
        legend.title = element_blank())

ggsave("day_7_map.png", width = 12, height = 7)
