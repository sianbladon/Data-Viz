
library(tidyverse)
library(showtext)
library(ggtext)

# data from https://population.un.org/wpp/Download/Probabilistic/Fertility/

fr <- read.csv("fertility_rates.csv")
world_fr <- read.csv("world_fr.csv")

# change to long format
fr_l <- fr %>%
  pivot_longer(cols = 2:17, names_to = "year", values_to = "fr")

fr_l$year <- as.numeric(fr_l$year)

# change to long format then wide again (don't ask...)

world_fr_l <- world_fr %>%
  pivot_longer(cols = 2:17, names_to = "year", values_to = "fr")

world_fr_l$year <- as.numeric(world_fr_l$year)

world_fr_w <- world_fr_l %>%
  pivot_wider(names_from = "value", values_from = "fr")


font_add_google("Montserrat", "Montserrat")
showtext_auto()

ggplot() +
  geom_line(fr_l %>% filter(country != "WORLD"), mapping= aes(x = year, y = fr, group = country), colour = "#6d7e8a", size =0.25) +
  geom_ribbon(world_fr_w,, mapping = aes(x = year, ymin = lower_95, ymax = upper_95), alpha = 0.45, fill = "#f28482") +
  geom_line(world_fr_w, mapping= aes(x = year, y = median), colour = "#EC4946", size = 2) +
  scale_x_continuous(limits = c(2020, 2100), breaks = seq(2020, 2100, by = 5), labels = seq(2020, 2100, by = 5)) +
  scale_y_continuous(sec.axis = dup_axis(), limits = c(0.5, 7), breaks = seq(1, 7, by = 1), labels = seq(1, 7, by = 1)) +
  labs(y = "Fertility Rate (live births per woman) \n",
       title = "Global Future Fertility Rates \n",
       subtitle = "Each <span style='color:#6d7e8a;'><strong>grey</strong></span> line is the projected fertility rate of an individual country. <br><br>The <span style='color:#EC4946;'><strong>red</strong></span> line denotes the global average rate, with the 95% prediction interval shaded.<br>",
       caption = "\n Data from United Nations, Department of Economic and Social Affairs, Population Division (2019)") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(colour = "#2F4858"),
        axis.ticks.x = element_line(colour = "#2F4858"),
        axis.title.x = element_blank(),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 3),
        axis.title.y.right = element_blank(),
        text = element_text(family = "Montserrat", colour = "#2F4858"),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y.left = element_text(size = 12),
        axis.text.y.right = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_markdown(size = 16, lineheight = 1.2, hjust = 0.5),
        plot.caption = element_text(size = 8),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

ggsave("day_28_future.png", width = 12, height = 10)
