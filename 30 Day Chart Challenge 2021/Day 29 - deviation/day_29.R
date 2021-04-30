library(tidyverse)
library(showtext)
library(patchwork)


hi <- read.csv("hi.csv")

hi <- hi %>%
  mutate(diff = Index.value - 100) %>%
  arrange(diff) %>%
  mutate(Area.Name = fct_reorder(Area.Name, diff))

hi$Area.Name <- fct_reorder(hi$Area.Name, hi$diff)

hi$Area.Name <- as.factor(hi$Area.Name)

font_add_google("Montserrat", "Montserrat")
showtext_auto()

# filter for region and plot separately

north <- hi %>%
  filter(region == "YORKSHIRE AND THE HUMBER" | region == "NORTH EAST" | region == "NORTH WEST ")

n <- ggplot(north) +
  geom_col(north, mapping = aes(x = diff, y = Area.Name, fill = Index.value), colour = "#2F4858", size = 0.3) +
  geom_text(data = north %>% filter(diff < 0), mapping = aes(x = 0.5, y = Area.Name, label = Area.Name), hjust = "left", family = "Montserrat", size = 4.5, colour = "#2F4858", fontface = "bold") +
  geom_text(data = north %>% filter(diff > 0), mapping = aes(x = -0.5, y = Area.Name, label = Area.Name), hjust = "right", family = "Montserrat", size = 4.5, colour = "#2F4858", fontface = "bold") +
  scale_y_discrete(expand = c(0,0)) +
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 100) +
  scale_fill_gradientn(limits = c(86.4, 110.1), colours = c("#be8580", "#eddccd","#fafaeb","#439397","#336e7f"), name = "Health Index") +
  scale_x_continuous(limits = c(-14, 11), breaks = seq(-15, 10, by = 2.5), labels = c("85", "87.5", "90", "92.5", "95", "97.5", "100", "102.5", "105", "107.5", "110")) +
  labs(title = "YORKSHIRE AND THE NORTH\n") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16, vjust = -3, colour = "#2F4858"),
        axis.title.x = element_blank(),
        text = element_text(family = "Montserrat", colour = "#2F4858"),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0, 1, 0), "cm"))

mids_east <- hi %>%
  filter(region == "WEST MIDLANDS" | region == "EAST MIDLANDS" | region == "EAST")

m <- ggplot(mids_east) +
  geom_col(mids_east, mapping = aes(x = diff, y = Area.Name, fill = Index.value), colour = "#2F4858", size = 0.3) +
  geom_text(data = mids_east %>% filter(diff < 0), mapping = aes(x = 0.5, y = Area.Name, label = Area.Name), hjust = "left", family = "Montserrat", size = 5, colour = "#2F4858", fontface = "bold") +
  geom_text(data = mids_east %>% filter(diff > 0), mapping = aes(x = -0.5, y = Area.Name, label = Area.Name), hjust = "right", family = "Montserrat", size = 5, colour = "#2F4858", fontface = "bold") +
  scale_y_discrete(expand = c(0,0)) +
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 100) +
  scale_fill_gradientn(limits = c(86.4, 110.1), colours = c("#be8580", "#eddccd","#fafaeb","#439397","#336e7f"), name = "Health Index") +
  scale_x_continuous(limits = c(-14, 11), breaks = seq(-15, 10, by = 2.5), labels = c("85", "87.5", "90", "92.5", "95", "97.5", "100", "102.5", "105", "107.5", "110")) +
  labs(title = "MIDLANDS AND THE EAST\n") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16, vjust = -3, colour = "#2F4858"),
        axis.title.x = element_blank(),
        text = element_text(family = "Montserrat", colour = "#2F4858"),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0, 1, 0), "cm"))

south <- hi %>%
  filter(region == "SOUTH WEST" | region == "SOUTH EAST")

s <- ggplot(south) +
  geom_col(south, mapping = aes(x = diff, y = Area.Name, fill = Index.value), colour = "#2F4858", size = 0.3) +
  geom_text(data = south %>% filter(diff < 0), mapping = aes(x = 0.5, y = Area.Name, label = Area.Name), hjust = "left", family = "Montserrat", size = 5, colour = "#2F4858", fontface = "bold") +
  geom_text(data = south %>% filter(diff > 0), mapping = aes(x = -0.5, y = Area.Name, label = Area.Name), hjust = "right", family = "Montserrat", size = 5, colour = "#2F4858", fontface = "bold") +
  scale_y_discrete(expand = c(0,0)) +
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 100) +
  scale_fill_gradientn(limits = c(86.4, 110.1), colours = c("#be8580", "#eddccd","#fafaeb","#439397","#336e7f"), name = "Health Index") +
  scale_x_continuous(limits = c(-14, 11), breaks = seq(-15, 10, by = 2.5), labels = c("85", "87.5", "90", "92.5", "95", "97.5", "100", "102.5", "105", "107.5", "110")) +
  labs(title = "SOUTH\n") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16, vjust = -3, colour = "#2F4858"),
        axis.title.x = element_blank(),
        text = element_text(family = "Montserrat", colour = "#2F4858"),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0, 1, 0), "cm"))

london <- hi %>%
  filter(region == "LONDON")

l <- ggplot(london) +
  geom_col(london, mapping = aes(x = diff, y = Area.Name, fill = Index.value), colour = "#2F4858", size = 0.3) +
  geom_text(data = london %>% filter(diff < 0), mapping = aes(x = 0.5, y = Area.Name, label = Area.Name), hjust = "left", family = "Montserrat", size = 5, colour = "#2F4858", fontface = "bold") +
  geom_text(data = london %>% filter(diff > 0), mapping = aes(x = -0.5, y = Area.Name, label = Area.Name), hjust = "right", family = "Montserrat", size = 5, colour = "#2F4858", fontface = "bold") +
  scale_y_discrete(expand = c(0,0)) +
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 100) +
  scale_fill_gradientn(limits = c(86.4, 110.1), colours = c("#be8580", "#eddccd","#fafaeb","#439397","#336e7f"), name = "Health Index") +
  scale_x_continuous(limits = c(-14, 11), breaks = seq(-15, 10, by = 2.5), labels = c("85", "87.5", "90", "92.5", "95", "97.5", "100", "102.5", "105", "107.5", "110")) +
  labs(title = "LONDON\n") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16, vjust = -3, colour = "#2F4858"),
        axis.title.x = element_blank(),
        text = element_text(family = "Montserrat", colour = "#2F4858"),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0, 1, 0), "cm"))


plot <- (n + m)/(l+s)

plot + plot_annotation(title="\n Health Index for Local Authorities in England \n", 
                       subtitle = "The health index is a new measure created by the ONS, made up of 58 different indicators. \n A score of 100 in any year means health levels are equal to the health of England in 2015. \nA higher score means health is improving, and a lower score means health is declining. \nFor every 10 points higher or lower than 100, a score is 1 standard deviation above or below England’s 2015 score.\n",
                       caption = "\nData from Office for National Statistics",
                       theme = theme(text = element_text('Montserrat',  colour = "#2F4858"),
                                     plot.title = element_text(face = "bold", size = 30, hjust = 0.5, lineheight = 1.5),
                                     plot.subtitle = element_text(size = 24, hjust = 0.5, lineheight = 1.5),
                                     plot.caption = element_text(size = 16),
                                    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")))

ggsave("day_29_deviation.png", width = 20, height = 22) 
