## Tidy Tuesday 2020 - Week 46 - Phone Subscription Data


## load packages
library(tidyverse)
library(viridisLite)
library(cowplot)
library(showtext)


## read in data
mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

## load fonts
font_add_google("Roboto Slab", "roboto")    
showtext_auto() 

# extract South and Central American countries and clean 

south_america <- c("Argentina", "Belize", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Ecuador", "El Salvador", "Guatemala", "Guyana", "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")

south_america_mob <- mobile %>%
  filter(entity %in% south_america)

landline$entity <- as.factor(landline$entity)

south_america_land <- landline %>%
  filter(entity %in% south_america) %>%
  filter(year != "2018" & year != "2019") %>%
  mutate(entity = droplevels(entity))


# create plot for landline subscriptions
land_plot <- ggplot(south_america_land) +
  geom_tile(aes(year, entity, fill = landline_subs)) +
  scale_fill_viridis_c(option = "magma", name = "Landline Subscriptions per 100 People") +
  scale_y_discrete(limits = rev(levels(south_america_land$entity))) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015), labels = c("1990", " ", "2000", " ", "2010", "")) +
  theme_minimal() + 
  labs(title = "Landline Subscriptions per 100 People", x = "YEAR", y = "") +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 12, face = "bold", vjust = -3, colour = "white", family = "roboto"),
    axis.text.y = element_text(size = 12, face = "bold", colour = "white", family = "roboto"),
    axis.text.x = element_text(size = 11, face = "bold", vjust = -1, colour = "white", family = "roboto"),
    axis.ticks.x = element_line(colour = "white"),
    axis.ticks.length.x = unit(0.2, "cm"),
    plot.title = element_text(face = "bold", size = 20, vjust = 2, hjust = 0.5, colour = "white", family = "roboto"),
    plot.subtitle = element_text(face = "bold", size = 14, vjust =4, colour = "white"),
    plot.background = element_rect(fill ="grey39" , colour = NA),
    panel.background = element_rect(fill ="grey39" , colour = NA),
    legend.position = "top", 
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold", colour = "white"),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm"),
    plot.margin = unit(c(2, 1 , 2, 1), "cm"))

# create mobile subscription data

mob_plot <- ggplot(south_america_mob) +
  geom_tile(aes(year, entity, fill = mobile_subs)) +
  scale_fill_viridis_c(option = "magma") +
  scale_y_discrete(limits = rev(levels(south_america_land$entity))) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015), labels = c("1990", " ", "2000", " ", "2010", "")) +
  theme_minimal() + 
  labs(title = "Mobile Subscriptions per 100 People", x = "YEAR", y = "") +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 12, face = "bold", vjust = -3, colour = "white", family = "roboto"),
    axis.text.y = element_text(size = 12, face = "bold", colour = "white", family = "roboto"),
    axis.text.x = element_text(size = 11, face = "bold", vjust = -1, colour = "white", family = "roboto"),
    axis.ticks.x = element_line(colour = "white"),
    axis.ticks.length.x = unit(0.2, "cm"),
    plot.title = element_text(face = "bold", size = 20, vjust = 2, hjust = 0.5, colour = "white", family = "roboto"),
    plot.subtitle = element_text(face = "bold", size = 14, vjust =4, colour = "white"),
    plot.background = element_rect(fill ="grey39" , colour = NA),
    panel.background = element_rect(fill ="grey39" , colour = NA),
    legend.position = "top", 
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold", colour = "white"),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm"),
    plot.margin = unit(c(2, 1 , 2, 1), "cm"))


# put plots on grid and save
plots <- plot_grid(land_plot, mob_plot)


ggsave("phone_subs.png",  width = 35, height =20, units = "cm")

