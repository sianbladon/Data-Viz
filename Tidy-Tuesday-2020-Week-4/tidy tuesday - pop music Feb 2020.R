library(tidyverse)
library(lubridate)
library(ggplot2)
library(wesanderson)
library(PNWColors)
library(devtools)
library(PNWColors)
devtools::install_github("jakelawlor/PNWColors")
install.packages("nationalpark")


spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')


### truncate date to year only
popularity <- spotify_songs %>%
  mutate(release_date = ymd(track_album_release_date, truncated = 2L)) %>%
  mutate(release_year = year(release_date)) 

### set colour palette
pal <- pnw_palette("Cascades",6)  

### create boxplot/violin 
bv <- ggplot(popularity, aes(playlist_genre, track_popularity, fill = playlist_genre)) +
  geom_violin() + 
  geom_boxplot(width = .2, fill = "white")

bv + 
  scale_fill_manual(values = pal) +
  guides(fill = "none") +
  scale_x_discrete(name = "Genre", labels = c("EDM", "Latin", "Pop", "R&B", "Rap", "Rock")) +
  scale_y_continuous(name = "Popularity") +
  labs(title = "Popularity of Music Genres") +
  theme_minimal() +
  theme(axis.text.x = element_text(face="plain", color="Black", size=16, angle = 0), 
        axis.title.x = element_text(color = "Black", size = 18, angle = 0, face = "bold"),
        axis.title.y = element_text(color = "Black", size = 18, face = "bold"),
        axis.text.y = element_text(face="plain", color="Black", size=16, angle = 0),
        plot.background = element_rect(fill = "#f5f5f5"),
        plot.title = element_text(size=20, face="bold.italic", hjust = 0.5))
  

ggsave("PopbyGenre.png", dpi = 300, width = 12, height = 9, units = "in")

### summarise mean popularity per year 
pop <- popularity %>%
  group_by(playlist_genre, release_year) %>%
  summarise(mean_pop = mean(track_popularity))

### create line plot
l <- ggplot(pop, aes(x = release_year, y = mean_pop, color = playlist_genre)) +
  geom_point(size = 1) +
  geom_smooth(se=F, size = 1.5)

l +
  scale_color_manual(values = pal, name = "Genre", labels = c("EDM", "Latin", "Pop", "R&B", "Rap", "Rock")) +
  scale_x_continuous(name = "Year of Release") +
  scale_y_continuous(name = "Mean Popularity") +
  labs(title = "Popularity of Music Genres by Year of Release") +
  theme_minimal() + 
  theme(axis.text.x = element_text(face="plain", color="Black", size=16, angle = 0), 
        axis.title.x = element_text(color = "Black", size = 18, angle = 0, face = "bold"),
        axis.title.y = element_text(color = "Black", size = 18, face = "bold"),
        axis.text.y = element_text(face="plain", color="Black", size=16, angle = 0),
        plot.background = element_rect(fill = "#f5f5f5"),
        plot.title = element_text(size=20, face="bold.italic", hjust = 0.5)) 

ggsave("PopOverTime.png", dpi = 300, width = 12, height = 9, units = "in")