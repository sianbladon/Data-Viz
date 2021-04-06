library(tidyverse)
library(ggalluvial)
library(ggrepel)
library(showtext)

# data from https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-11-19

nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")

birds <- nz_bird %>%
  filter(date >= "2019-11-06") %>%
  filter(!is.na(bird_breed))

top_10 <- birds %>%
  select(bird_breed) %>%
  group_by(bird_breed) %>%
  summarise(total_votes = n()) %>%
  ungroup() %>%
  arrange(desc(total_votes)) %>%
  top_n(n = 10)
  
  bird <- birds %>%
  select(date, bird_breed) %>%
  group_by(date, bird_breed) %>%
  summarise(total_votes = n()) %>%
  ungroup() %>%
  filter(bird_breed %in% top_10$bird_breed) %>%
  group_by(date) %>%
  mutate(tot_votes_rnd = sum(total_votes)) %>%
  mutate(votes_pct = round((total_votes/tot_votes_rnd)*100)) %>%
  ungroup()

bird_rank <- bird %>%
  group_by(date) %>%
  arrange(date, desc(votes_pct)) %>%
  mutate(rank = dense_rank(desc(votes_pct))) %>%
  ungroup()
  
font_add_google("Montserrat", "Montserrat")
showtext_auto()

# colours taken from the Manu package https://g-thomson.github.io/Manu/

ggplot(bird_rank, aes(x = date, stratum = rank, alluvium = bird_breed, y = votes_pct, fill = bird_breed)) +
  geom_stratum(reverse = TRUE, colour = "grey") +
  geom_alluvium(aes(fill = bird_breed), colour = "grey", reverse = TRUE, aes.bind = "flows") +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), labels = c("0 %", "20 %", "40 %", "60 %", "80 %", "100 %")) +
  scale_x_continuous(labels = c("Round 1", "Round 2", "Round 3", "Round 4", "Final")) +
  scale_fill_manual(aes(fill = bird_breed), name = "Bird Breed", values = c("#85BEDC",  "#287DAB", "#7C7189", "#CABEE9", "#647588", "#325756", "#7d9fc2", "#C582B2", "#51806a", "#4d5f8e", "#A092B7")) +
  geom_text(aes(label = ifelse(as.Date(date) == "2019-11-10", paste0(votes_pct, "%"), NA)),
            stat = "alluvium", size = 4) +
  labs(title = "New Zealand Bird of the Year 2019",
       subtitle = "Proportion of votes awarded to the most popular birds in the final 5 rounds of voting") +
  theme_minimal() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14, face = "bold", colour = "#2F4858"),
        plot.subtitle = element_text(size = 14, colour = "#2F4858", lineheight = 1.4),
        plot.caption = element_text(size = 10, colour = "#2F4858"),
        plot.title = element_text(size = 20,face = "bold", colour = "#2F4858", lineheight = 1.4),
        axis.text.x = element_text(size = 14, face = "bold", colour = "#2F4858"),
        legend.text = element_text(size = 10, colour = "#2F4858"),
        legend.title = element_text(size = 14, face = "bold", colour = "#2F4858"))

ggsave("day_6_sankey.jpeg", plot = last_plot(), width = 10, height = 8)
