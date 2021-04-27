library(tidyverse)
library(showtext)

# data from https://www.gapminder.org/data/


income <- read.csv("gapminder_countries.csv")
education <- read.csv("owid_education_idx.csv")

ed_2017 <- education %>%
  select(country, "2017") %>%
  rename(index_2017 = "2017")

# join together
ed_2017 <- ed_2017 %>%
  left_join(income, by = c("country" = "name")) %>%
  mutate(index_2017_n = round(index_2017, digits = 2)) %>%
  filter(!is.na(World.bank..4.income.groups.2017)) %>%
  filter(!is.na(index_2017))

# reorder income group 
ed_2017$World.bank..4.income.groups.2017 <- factor(ed_2017$World.bank..4.income.groups.2017, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))

# calculate median and iqr
sum <- ed_2017 %>%
  group_by(World.bank..4.income.groups.2017) %>%
  summarise(x = quantile(index_2017, c(0.25, 0.5, 0.75)), q = c("p_25", "med", "p_75")) %>%
  ungroup()

# change to wide format
sum_l <- sum %>%
  pivot_wider(names_from = q, values_from = x)

font_add_google("Montserrat", "Montserrat")
showtext_auto()

ggplot(ed_2017, aes(x = index_2017, y = World.bank..4.income.groups.2017)) +
  geom_violin(aes(fill = World.bank..4.income.groups.2017, colour = World.bank..4.income.groups.2017), trim = FALSE, alpha = 0.8) +
  geom_point(sum_l, mapping = aes(x = med, y = World.bank..4.income.groups.2017), size = 5, colour = "#2F4858") +
  geom_segment(sum_l, mapping = aes(x = p_25, xend = p_75, y = World.bank..4.income.groups.2017, yend = World.bank..4.income.groups.2017), size = 1.5, colour = "#2F4858") +
  labs(x = "\n OWID Education Index", y = "",
       title = "OWID Education Index \n",
       subtitle = "The index is calculated from the mean years  of schooling \n and expected  years of schooling. \n Data shown are from 2017 with countries grouped by World Bank \n income level. \n The point represents the median value and the line shows \n the interquartile range. \n",
       caption = "\n FREE DATA FROM GAPMINDER.ORG, CC-BY LICENSE") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c("0%", "20%", "40%", "60%", "80%", "100%")) +
  scale_y_discrete(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income")) +
  scale_fill_manual(values = c("#83b692", "#f9627d","#8a9eff","#8332ac")) +
  scale_colour_manual(values = c("#83b692", "#f9627d","#8a9eff","#8332ac")) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Montserrat", colour = "#2F4858"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "#2F4858"),
        axis.text.y = element_text(size = 12, colour = "#2F4858", face = "bold"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0),
        plot.subtitle = element_text(size = 16, lineheight = 1.2, hjust = 0),
        plot.caption = element_text(size = 8),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank())


ggsave("day_27_education.png", width = 10, height = 10)
