
library(tidyverse)
library(showtext)

hugh <- hugh %>%
  mutate(seen = as.factor(seen),
         budget_mil = budget_usd/1000000,
         bo_mil = box_office_gross_usd/1000000)

font_add_google("Montserrat", "Montserrat")
showtext_auto()

ggplot(hugh, aes(x = budget_mil, y = bo_mil)) +
  geom_point(shape = 21, aes(size = imdb_rating, fill = seen), colour = "grey") +
  scale_size_continuous(range = c(2,9)) +
  labs(x = "Budget in Millions of US Dollars", y = "Box Office Gross in Millions of US Dollars",
       title = "The Films of Hugh Grant \n",
       subtitle = "Relationship between the budget and box office takings of Hugh Grant films. \n
       The size of each point is proportional to the film's IMDB rating \n") +
  scale_fill_manual(name = "Have I Seen It?", values = c("n" = "#FFB997", "y" = "#F67E7D", "f" = "#843B62"), labels = c("Yes, it's a favourite", "No", "Yes")) + 
  geom_text(hugh %>% filter(film %in% c("Two Weeks Notice", "Mickey Blue Eyes", "Music and Lyrics")), mapping = aes(x = budget_mil + 8, label = film), colour = "#F67E7D", size = 4, fontface = "bold") +
  geom_text(hugh %>% filter(film == "About a Boy"), mapping = aes(x = budget_mil + 6, y = bo_mil - 3, label = film), colour = "#F67E7D", size = 4, fontface = "bold") +
  geom_text(hugh %>% filter(film == "Four Weddings and a Funeral"), mapping = aes(x = budget_mil + 10, label = "Four Weddings and \n a Funeral"), colour = "#F67E7D", size = 4, fontface = "bold") +
  geom_text(hugh %>% filter(film == "Florence Foster Jenkins"), mapping = aes(x = budget_mil + 12, label = film), colour = "#F67E7D", size = 4, fontface = "bold") +
  geom_text(hugh %>% filter(film == "Did You Hear About the Morgans?"), mapping = aes(x = budget_mil + 8, label = "Did You Hear \n About the Morgans?"), colour = "#F67E7D", size = 4, fontface = "bold") +
  geom_text(hugh %>% filter(film == "Remains of the Day"), mapping = aes(y = bo_mil + 20, label = film), colour = "#F67E7D", size = 4, fontface = "bold") +
  geom_text(hugh %>% filter(film == "The Pirates! In an Adventure with Scientists!"), mapping = aes(x = budget_mil + 18, label = film), colour = "#F67E7D", size = 4, fontface = "bold") +
  geom_text(hugh %>% filter(film %in% c("Love Actually", "Paddington 2")), mapping = aes(x = budget_mil + 11, label = film), colour = "#843B62", size = 5, fontface = "bold") +
  geom_text(hugh %>% filter(film %in% "Notting Hill"), mapping = aes(x = budget_mil + 8, label = film), colour = "#843B62", size = 5, fontface = "bold") +
  geom_text(hugh %>% filter(film == "Sense and Sensibility"), mapping = aes(y = bo_mil + 20, label = film),  colour = "#843B62", size = 5, fontface = "bold") +
  geom_text(hugh %>% filter(film == "Bridget Jones: The Edge of Reason"), mapping = aes(x = budget_mil + 18, label = film), colour = "#843B62", size = 5, fontface = "bold") +
  geom_text(hugh %>% filter(film == "Bridget Jones's Diary"), mapping = aes(x = budget_mil + 10, label = "Bridget Jones's \n Diary"), colour = "#843B62", size = 5, fontface = "bold") +
  geom_text(hugh %>% filter(film == "The Gentlemen"), mapping = aes(y = bo_mil -20, label = film), size = 3.5, colour = "#FFB997", fontface = "bold") +
  geom_text(hugh %>% filter(film == "Cloud Atlas"), mapping = aes(y = bo_mil +20, label = film), size = 3.5, colour = "#FFB997", fontface = "bold") +
  geom_text(hugh %>% filter(film == "The Man from U.N.C.L.E"), mapping = aes(x = budget_mil +12, label = film), size = 3.5, colour = "#FFB997", fontface = "bold") +
  geom_text(hugh %>% filter(film %in% c("American Dreamz", "Small Time Crooks")), mapping = aes(x = budget_mil + 8, label = film), size = 3.5, colour = "#FFB997", fontface = "bold") +
  geom_text(hugh %>% filter(film == "Extreme Measures"), mapping = aes(y = bo_mil + 9, x = budget_mil + 7, label = film), size = 3.5, colour = "#FFB997", fontface = "bold") +
  geom_text(hugh %>% filter(film == "The Englishman Who Went Up a Hill But Came Down a Mountain"), mapping = aes(x = budget_mil + 24, label = film), size = 3.5, colour = "#FFB997", fontface = "bold") +
  geom_text(hugh %>% filter(film == "Restoration"), mapping = aes(x = budget_mil + 7, label = film), size = 3.5, colour = "#FFB997", fontface = "bold") +
  geom_text(hugh %>% filter(film == "An Awfully Big Adventure"), mapping = aes(y = bo_mil + 20, label = "An Awfully \n Big Adventure"), size = 3.5, colour = "#FFB997", fontface = "bold") +
  theme_minimal() +
  theme(legend.position = "top", 
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        text = element_text(family = "Montserrat", colour = "#2F4858"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  guides(size = FALSE)

ggsave("day_15.png", width = 12, height = 9)
