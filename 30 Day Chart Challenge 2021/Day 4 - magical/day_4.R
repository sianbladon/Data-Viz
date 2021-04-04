
library(tidyverse)
library(treemapify)
library(showtext)

# data from https://www.kaggle.com/mokosan/lord-of-the-rings-character-data

lotr_words <- read.csv("WordsByCharacter.csv")

words_sum <- lotr_words %>%
  group_by(Race, Character) %>%
  summarise(total_words = sum(Words)) %>%
  ungroup()
  
 font_add_google("Montserrat", "Montserrat")
showtext_auto()

ggplot(words_sum, aes(area = total_words, subgroup = Race, subgroup2 = Character, 
                      label = Character, fill = Race)) +
  geom_treemap() +
  geom_treemap_subgroup2_border(colour = "white",  size = 0.5) +
  geom_treemap_subgroup_border(colour = "white", size = 2) +
  geom_treemap_subgroup2_text(colour = "white") +
  #geom_treemap_subgroup_text(colour = "white", fontface = "italic", place = "top") +
  scale_fill_manual(values = c("#6D7280", "#022D36", "#233A42", "#2C5A62","#011820", "#79867F", "#9AB6A0", "#9DA188", "#9C9175", "#5E4D40")) +
  labs(title = "Words Spoken by Lord of the Rings Characters \n",
       subtitle = "Area is proportional to the number of words spoken by each \n character in the film trilogy, grouped by race.\n",
       caption = "Data from https://www.kaggle.com/mokosan/lord-of-the-rings-character-data") +
  theme_minimal() +
  theme(text = element_text(family = "Montserrat"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text =  element_text(size = 12, colour = "#2F4858"),
        plot.title = element_text(size = 22,face = "bold", colour = "#2F4858"),
        plot.subtitle = element_text(size = 16, colour = "#2F4858"),
        plot.caption = element_text(size = 8, colour = "#2F4858"),
        plot.caption.position = "plot")

ggsave("day_4_treemap.jpeg", plot = last_plot(), width = 12, height = 10)
