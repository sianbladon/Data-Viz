library(tidyverse)
library(showtext)
library(PupillometryR) # for geom_flat_violin

# data from here https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-08

ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

# change to long format
ipf_lifts_long <- ipf_lifts %>%
  select(name, sex, best3squat_kg, best3bench_kg, best3deadlift_kg) %>%
  pivot_longer(cols = starts_with("best3"),
               names_to = "lift",
               values_to = "weight") %>%
  filter(!is.na(weight)) %>%
  filter(weight > 0) %>%
  mutate(lift_no = case_when(lift == "best3squat_kg" ~ 3,
                             lift == "best3deadlift_kg" ~ 2,
                             lift == "best3bench_kg" ~ 1))
                             

font_add_google("Montserrat", "Montserrat")
showtext_auto()


ggplot(ipf_lifts_long, aes(x = lift, y = weight, fill = sex)) +
  geom_flat_violin(aes(fill = sex), position = position_nudge (x =.15, y = 0),
                   adjust = 2, trim = FALSE, alpha = .5, colour = NA) +
  geom_boxplot(aes(x = lift, y = weight, fill = sex), outlier.shape = NA, alpha = 0.6, width = 0.25, colour = "#2F4858") +
  scale_x_discrete(name = NULL, labels = c("Bench", "Deadlift", "Squat")) +
  scale_fill_manual(labels = c("Female", "Male"), values = c("#4B5381", "#36817F")) +
  labs(y = "Weight (kg)", title = "Open Powerlifting \n",
       subtitle = "Maximum weights lifted by Females and Males across the 3 disciplines. \n 
       Boxplots show the median, lower and upper quartiles.",
       caption = "Data from TidyTuesday 2019 - Week 41" ) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey"),
        #panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Montserrat"),
        plot.title = element_text(colour = "#2F4858", size = 16, face = "bold", lineheight = 1.5, hjust = 0.5),
        plot.subtitle = element_text(colour = "#2F4858", size = 14, lineheight = 1, hjust = 0.5),
        plot.caption = element_text(colour = "#2F4858", size = 10),
        plot.caption.position = "plot",
        legend.position = "top",
        legend.text = element_text(colour = "#2F4858", size = 12),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 12, colour = "#2F4858"),
        axis.title.x = element_text(size = 14, face = "bold", colour = "#2F4858", vjust = 0.5),
        axis.text.y = element_text(size = 14, face = "bold", colour = "#2F4858"))
  
ggsave("day_9_box.png", width = 10, height = 10)
