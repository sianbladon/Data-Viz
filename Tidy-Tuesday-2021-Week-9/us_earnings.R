library(tidyverse)
library(ggalt)
library(patchwork)
library(ggtext)
library(showtext)


earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

# filter for all ages and ethnic origins, and calculate avg yearly earnings

earn_data <- earn %>%
  filter(age == "16 years and over" & ethnic_origin == "All Origins") %>%
  select(-ethnic_origin, -n_persons) %>%
  group_by(sex, race, age, year) %>%
  summarise(yr_avg = mean(median_weekly_earn)) %>%
  ungroup()

# reformat to wide and round numbers

earn_data_wide <- earn_data %>%
  pivot_wider(names_from = sex, values_from = yr_avg) %>%
  mutate(both_avg = round(`Both Sexes`), female_avg = round(Women), male_avg = round(Men)) %>%
  select(-`Both Sexes`, -Women, -Men) %>%
  mutate(year = as.factor(year))

# group data

white <- earn_data_wide %>%
  filter(race == "White")

black <- earn_data_wide %>%
  filter(race == "Black or African American")


# add font
font_add_google("Montserrat", "Montserrat")
showtext_auto()


# plot white

p1 <- ggplot() +
  geom_segment(data = white, aes(y = year, yend = year, x = 500, xend = 1150), colour = "#908B94",
               size = 0.15) +
  geom_dumbbell(data = white, aes(y = year, x = female_avg, xend = male_avg),
                size = 1, colour = "#908B94", size_x = 4, size_xend = 4,
                colour_x = "#7F557D", colour_xend = "#2a6f97") + 
  scale_x_reverse(breaks = c(500, 750, 1000), labels = c("$500", "$750", "$1000")) +
  scale_y_discrete(expand = expansion(mult = c(0.1, .2))) +
  geom_text(data = filter(white, year == "2020"),
            aes(x = female_avg, y = year, label = "Females"),
            colour = "#7F557D", size = 3, vjust = -2.5, hjust = 0.4, fontface = "bold",family = "Montserrat") +
  geom_text(data = filter(white, year == "2020"),
            aes(x = male_avg, y = year, label = "Males"),
            colour = "#2a6f97", size = 3, vjust = -2.5, fontface = "bold",family = "Montserrat") +
  geom_text(data = white, 
            aes(x = female_avg, y = year, label = female_avg),
            colour = "#7F557D", size = 3.5, vjust = 2, fontface = "bold",family = "Montserrat") +
  geom_text(data = white, 
            aes(x = male_avg, y = year, label = male_avg),
            colour = "#2a6f97", size = 3.5, vjust = 2, fontface = "bold",family = "Montserrat") +
  labs(y = "", x = "", title = "White") +
  theme_minimal() +
  theme(text = element_text(family = "Montserrat"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 14,face = "bold", colour = "#535657", hjust = 0.5, vjust = -3),
        axis.title.x = element_text(vjust = -2),
        axis.text.x = element_text(size = 12, face = "bold", colour = "#535657"),
        axis.text.y = element_blank(),
        #axis.text.y = element_text(size = 14, face = "bold", colour = "#535657"),
        axis.ticks.x = element_line(colour = "#b2b2b2"),
        panel.background = element_rect(fill = "#C6D6D1", colour = "#C6D6D1"),
        plot.background = element_rect(fill = "#C6D6D1", colour = "#C6D6D1"))        

# add annotation

p1 <- p1 + annotate("text", x = 500, y = "2017", label = 
                      "The gender pay \n gap is greater \n in the white \n population", 
                    colour = "#535657", family = "Montserrat", 
                    fontface = "bold", size = 3.9, hjust = "right")  

# plot black/african american

p2 <- ggplot() +
  geom_segment(data = black, aes(y = year, yend = year, x = 500, xend = 1150), colour = "#908B94",
               size = 0.15) +
  geom_dumbbell(data = black, aes(y = year, x = female_avg, xend = male_avg),
                size = 1, colour = "#908B94", size_x = 4, size_xend = 4,
                colour_x = "#7F557D", colour_xend = "#2a6f97") + 
  scale_x_continuous(breaks = c(500, 750, 1000), labels = c("$500", "$750", "$1000")) +
  scale_y_discrete(expand = expansion(mult = c(0.1, .2))) +
  geom_text(data = filter(black, year == "2020"),
            aes(x = female_avg, y = year, label = "Females"),
            colour = "#7F557D", size = 3, vjust = -2.5, hjust = 0.9, fontface = "bold",family = "Montserrat") +
  geom_text(data = filter(black, year == "2020"),
            aes(x = male_avg, y = year, label = "Males"),
            colour = "#2a6f97", size = 3, vjust = -2.5, hjust = 0.2, fontface = "bold",family = "Montserrat") +
  geom_text(data = black, 
            aes(x = female_avg, y = year, label = female_avg),
            colour = "#7F557D", size = 3.5, vjust = 2, fontface = "bold",family = "Montserrat") +
  geom_text(data = black, 
            aes(x = male_avg, y = year, label = male_avg),
            colour = "#2a6f97", size = 3.5, vjust = 2, fontface = "bold",family = "Montserrat") +
  theme_minimal() +
  labs(y = "", x = "", title = "Black or African American") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 14,face = "bold", colour = "#535657", hjust = 0.5, vjust = -3),
        text = element_text(family = "Montserrat"),
        axis.title.x = element_text(vjust = -2),
        axis.text.x = element_text(size = 12, face = "bold", colour = "#535657"),
        axis.text.y = element_text(size = 14, face = "bold", colour = "#535657", hjust = -1),
        axis.ticks.x = element_line(colour = "#b2b2b2"),
        panel.background = element_rect(fill = "#C6D6D1", colour = "#C6D6D1"),
        plot.background = element_rect(fill = "#C6D6D1", colour = "#C6D6D1"))    

# add annotation
p2 <- p2 + annotate("text", x = 1150, y = "2014", label = 
              "On average, weekly \n pay  is  lower for  black \n or African American people", 
              colour = "#535657", family = "Montserrat", 
              fontface = "bold", size = 3.9, hjust = "right")


# arrange together

final <- p1 + p2

# add title and subtitle

final_anno <- final + plot_annotation(title="Differences in Weekly Earnings by Gender and Race in the USA
                                      ",
                        subtitle = "Data shown are the median weekly earnings in US dollars ($) <br> 
                          for <span style='color:#2a6f97;'><strong>Males</strong></span> and <span style='color:#7F557D;'><strong>Females</strong></span> aged 16 and over.",
                        theme = theme(text = element_text('Montserrat',  colour = "#535657"),
                                      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                                      plot.subtitle = element_markdown(size = 14, hjust = 0.5),
                                      plot.background = element_rect(fill ="#C6D6D1", colour = "#C6D6D1"),
                                      plot.margin = unit(c(1.5, 0, 0, 0), "cm")))


ggsave('us_earnings.png', final_anno, width = 14, height = 8) 
