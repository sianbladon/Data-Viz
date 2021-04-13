library(tidyverse)
library(showtext)
library(patchwork)

coverage <- read.csv("universal_health_coverage_index.csv")
ifr <- read.csv("infant_mortality_rate.csv")
maternal <- read.csv("mat_mortality_rate.csv")

# add continent 
coverage <- coverage %>%
  mutate(continent = case_when(str_detect(parentName, "Africa") ~ "Africa",
                               str_detect(parentName, "Asia") ~ "Asia",
                               str_detect(parentName, "Europe") ~ "Europe",
                               parentName == "Northern America" | parentName == "Caribbean" | parentName == "Central America" ~ "North & Central America",
                               parentName == "South America" ~ "South America",
                               str_detect(parentName, "nesia") | parentName == "Australia and New Zealand" ~ "Oceania"))
                               
 ifr <- ifr %>%
  select(geoAreaCode, geoAreaName, value_2017) %>%
  rename(ifr_2017 = value_2017)

join <- coverage %>%
  left_join(ifr, by = c("geoAreaCode", "geoAreaName")) %>%
  rename(cov_2017 = value_2017)
  
maternal <- maternal %>%
  select(geoAreaCode, geoAreaName, value_2017) %>%
  rename(mr_2017 = value_2017)

join_cov_mr <- coverage %>%
  left_join(maternal, by = c("geoAreaCode", "geoAreaName"))
  
font_add_google("Montserrat", "Montserrat")
showtext_auto()

ifr <- ggplot(join, aes(x = cov_2017, y = ifr_2017, fill = continent)) +
  geom_point(size = 6, alpha = 0.65, shape = 21, colour = "#2F4858") +
  labs(x = "\n Essential Health Services Coverage Index",
       y = "Infant Mortality Rate \n (per 1,000 live births) \n") +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100), labels = c("0", "20", "40", "60", "80", "100")) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), labels = c("0", "20", "40", "60", "80", "100")) +
  scale_fill_manual(values = c("#83B692", "#F9ADA0", "#F9627D", "#645DD7", "#5B3758", "#8332AC")) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = "top",
        legend.text = element_text(size = 16, face = "bold"),
        text = element_text(family = "Montserrat", colour = "#2F4858"),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16, face = "bold")) +
  guides(fill = guide_legend(nrow = 2))
  
mr <- ggplot(join_cov_mr, aes(x = value_2017, y = mr_2017, fill = continent)) +
  geom_point(size = 6, alpha = 0.65, shape = 21, colour = "#2F4858") +
  labs(x = "\n Essential Health Services Coverage Index",
       y = "Maternal Mortality Rate \n (per 100,000 live births) \n ",
       caption = "Data from UN Sustainable Development Goals") +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100), labels = c("0", "20", "40", "60", "80", "100")) +
  scale_y_continuous(breaks = c(0, 200, 400, 600, 800, 1000, 1200), labels = c("0", "200", "400", "600", "800", "1000", "1200")) +
  scale_fill_manual(values = c("#83B692", "#F9ADA0", "#F9627D", "#645DD7", "#5B3758", "#8332AC")) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Montserrat", colour = "#2F4858"),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16, face = "bold"),
        plot.caption = element_text(size = 10))

p <- ifr / mr

p + plot_annotation(title="\n Relationship Between Essential Health  \n Service Coverage and Mortality Rates \n", 
                    subtitle = "Data presented for each country are from 2017 \n",
                    theme = theme(text = element_text('Montserrat',  colour = "#2F4858"),
                                  plot.title = element_text(face = "bold", size = 22, lineheight = 1.5, hjust = 0.5),
                                  plot.subtitle = element_text(size = 18, hjust = 0.5, lineheight = 1.5)))


ggsave("day_13_ifr.png", width = 11, height = 15)
