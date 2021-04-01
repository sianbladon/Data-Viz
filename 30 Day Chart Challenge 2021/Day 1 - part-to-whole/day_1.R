library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggtext)

# data from TidyTuesday 2021 week 11
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-03-09/readme.md

raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')

# filter to films after 1950 and add decade var

raw_clean <- raw_bechdel %>%
  filter(year >= 1950) %>%
  mutate(decade = (case_when(year >= 1950 & year < 1960 ~ "1950's",
                            year >= 1960 & year < 1970 ~ "1960's",
                            year >= 1970 & year < 1980 ~ "1970's",
                            year >= 1980 & year < 1990 ~ "1980's",
                            year >= 1990 & year < 2000 ~ "1990's",
                            year >= 2000 & year < 2010 ~ "2000's",
                            year >= 2010 ~ "2010's")))
 
 # summarise totals by decade and rating
 
summary <- raw_clean %>%
  group_by(decade, rating) %>%
  count() %>%
  ungroup()
  
# load font
font_add_google("Montserrat", "Montserrat")
showtext_auto()

# make plot 
ggplot(summary, aes(x = n, y = decade, fill = as.factor(rating))) +
  geom_bar(position = position_fill(reverse = TRUE), stat = "identity", width = 0.6) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0 %", "25 %", "50 %", "75 %", "100 %")) +
  scale_fill_manual(values = c("#F7E3AF", "#F7AF9D", "#C08497", "#B0D0D3")) +
  labs(title = "\nBechdel Test Scores of Films Through the Decades \n",
       subtitle = "A film scores <span style='color:#F7E3AF;'><strong>0</strong></span> for a fail, 
            <span style='color:#F7AF9D;'><strong>1</strong></span> if it has at least two (named) women in, <br>
            <span style='color:#C08497;'><strong>2</strong></span> if those women talk to each other, 
             and <span style='color:#B0D0D3;'><strong>3</strong></span> if they talk about something besides a man.",
       caption = "Data sourced via Bechdeltest.com API",
       x = "",
       y = "",
       legend = "") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        text = element_text(family = "Montserrat"),
        plot.title = element_text(size = 20,face = "bold", colour = "#2F4858"),
        axis.text.x = element_text(size = 14, face = "bold", colour = "#2F4858"),
        axis.text.y = element_text(size = 14, face = "bold", colour = "#2F4858"),
        legend.text = element_text(size = 14, face = "bold", colour = "#2F4858"),
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.minor.x = element_line(colour = "grey"),
        plot.subtitle = element_markdown(size = 16, face = "bold", colour = "#2F4858"),
        plot.caption = element_text(size = 8, colour = "#2F4858"))

ggsave("day_1_stacked_bar.jpeg", plot = last_plot(), width = 12, height = 10)
