library(tidyverse)
library(lubridate)
library(showtext)

steps <- read.csv("aggregates_steps.csv")

# fill in na's for missing days - probably easier way to do this....

date <- seq(as.Date("2019-10-18"),as.Date("2021-04-23"), by = "day")
step <- 1

new <- tibble(date, step)

new <- new %>%
  left_join(steps, by = c("date" = "date_2")) 

new <- new %>%
  select(date, value) %>%
  mutate(value = replace_na(value, 0))

new$month <- format(as.Date(new$date), "%Y-%m")
new$day <- format(as.Date(new$date), "%d")


font_add_google("Montserrat", "Montserrat")
showtext_auto()


ggplot(new, aes(as.Date(date), y = 0.5)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(name = "Steps per Day", low = "#DFF6F4", high = "#2a9d8f") +
  scale_x_date(date_breaks = "3 months", date_labels = "%B %Y") +
  #scale_x_date(breaks = c("2019-10-18", "2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01", "2021-04-01"), labels = c("Sept 2019", "Jan 2020", "April 2020", "July 2020", "Oct 2020", "Jan 2021", "April 2021")) +
  labs(x = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#2F4858"),
        text = element_text(family = "Montserrat", colour = "white"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, colour = "white", vjust = -5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(colour = "white"),
        axis.ticks.length.x = unit(0.5, "cm"),
        axis.line.x = element_line(colour = "white"),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(2,"cm"),
        plot.margin = unit(c(1, 0.5, 1, 0), "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5, title.vjust = 2, ticks = FALSE))

ggsave("day_23_tiles.png", width = 11, height = 6)
