library(tidyverse)
library(waffle)
library(showtext)
library(ggtext)

# vaccine data from the gov.uk coronavirus dashboard, 
vacc <- read.csv("vacc_data.csv")

# adding font families for the geom_pictogram
library(extrafont)

extrafont::font_import (path="/Users/sianbladon/Downloads", pattern = "fa-", prompt =  FALSE)
loadfonts()

extrafont::fonttable() %>% 
  dplyr::as_tibble() %>% 
  dplyr::filter(grepl("Awesom", FamilyName)) %>% 
  select(FamilyName, FontName, fontfile)
  
font_add(family = "FontAwesome5Free-Solid", regular = "/Users/sianbladon/Downloads/fa-solid-900.ttf")
font_add(family = "FontAwesome5Free-Regular", regular = "/Users/sianbladon/Downloads/fa-regular-400.ttf")
font_add(family = "FontAwesome5Brands-Regular", regular = "/Users/sianbladon/Downloads/fa-brands-400.ttf")
showtext_auto()

# plot
ggplot(vacc) +
  facet_wrap(~ dose, ncol = 1, strip.position = 'top') +
  geom_pictogram(aes(values = number, label = vaccinated, colour = vaccinated), 
                 n_rows = 5, size = 12, 
                 make_proportional = TRUE,
                 family = "FontAwesome5Free-Solid") +
  scale_label_pictogram(name = NULL, values = c("male", "male"), labels = NULL) +
  scale_colour_manual(values = c("#B0D0D3","#C08497"), labels = NULL) +
  scale_x_continuous(breaks = c(5, 10, 15, 20), labels = c("25 %", "50 %", "75 %", "100 %")) +
  labs(x = "", y = "", title = "UK COVID Vaccination Progress \n",
       subtitle = "<br>Proportion of adults in the UK who are <span style='color:#C08497;'><strong>vaccinated</strong></span> vs <span style='color:#B0D0D3;'><strong>unvaccinated</strong></span>, <br> as of 30/03/2021. <br>  
         Each figure represents 5 % of the adult population.<br>",
       caption = "Data from https://coronavirus.data.gov.uk/details/vaccinations") +
  theme_minimal() +
  theme(text = element_text(family = "Montserrat"),
        legend.position = "none",
        axis.text.y.left = element_blank(),
        strip.text = element_text(size = 16, face = "bold",colour = "#2F4858"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 24,face = "bold", colour = "#2F4858"),
        axis.text.x = element_text(size = 14, face = "bold", colour = "#2F4858"),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_line(colour = "grey"),
        plot.subtitle = element_markdown(size = 18, colour = "#2F4858"),
        plot.caption = element_text(size = 12, colour = "#2F4858"))

ggsave("day_2_pictogram.jpeg", plot = last_plot(), width = 12, height = 10)
