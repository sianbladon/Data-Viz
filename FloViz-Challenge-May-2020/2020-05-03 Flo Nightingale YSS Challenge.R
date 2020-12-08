###### Flo Nightingale Data Viz challenge
## May 2020

data <- read.csv("fn_data-1.csv")

library(zoo)
library(tidyverse)
library(viridis)
library(lubridate)
library(gganimate)
library(gifski)
library(av)
install.packages("av")
str(data)

## add empty bars at start/end? colours, legend, annotate
## clean month names
data <- data %>%
  mutate(Month = case_when(Month == "Apr " | Month == "Apr" ~ "Apr",
                           Month == "May " | Month == "May" ~ "May",
                           Month == "Jun " | Month == "Jun" ~ "Jun",
                           Month == "Jul " | Month == "Jul" ~ "Jul",
                           Month == "Aug " | Month == "Aug" ~ "Aug",
                           Month == "Sep " | Month == "Sep" ~ "Sep",
                           Month == "Oct " | Month == "Oct" ~ "Oct",
                           Month == "Nov " | Month == "Nov" ~ "Nov",
                           Month == "Dec " | Month == "Dec" ~ "Dec",
                           Month == "Jan " | Month == "Jan" ~ "Jan",
                           Month == "Feb " | Month == "Feb" ~ "Feb",
                           Month == "Mar " | Month == "Mar" ~ "Mar"))

## convert Month to month number
data <- data %>%
  mutate(Month = as.character(Month)) %>%
  mutate(month_no = match(Month, month.abb))

  

## sort date into one column

#data <- data %>%
#  mutate(year_month = paste(Year, month_no, sep = "-"),
#         month_year = paste(month_no, Year, sep = "-")) %>%
#  mutate(date = as.Date(as.yearmon(year_month)))
#year_month = paste(Year, month_no, sep = "-") ,
#month_year = paste(month_no, Year, sep = "-"))

### create numerical date column to order bars and text date column for labels
data <- data %>%
  mutate(year_month = paste(Year, month_no, sep = "-")) %>%
  mutate(date_text = as.character(as.yearmon(year_month)),
         date_num = as.Date(as.yearmon(year_month)))

#data <- data %>%
  #mutate(month_label = case_when(Month == "Apr" ~ "April",
                    #Month == "May" ~ "May",
                    #Month == "Jun" ~ "June",
                    #Month == "Jul" ~ "July",
                    #Month == "Aug" ~ "August",
                    #Month == "Sep" ~ "September",
                    #Month == "Oct" ~ "October",
                    #Month == "Nov" ~ "November",
                    #Month == "Dec" ~ "December",
                    #Month == "Jan" ~ "January",
                    #Month == "Feb" ~ "February",
                    #Month == "Mar" ~ "March"))

### transform data to long 

data <- data %>% 
  select(AMR_Zymotic_diseases, AMR_Wounds_injuries, AMR_Other, date_text, date_num, Year) %>%
  rename("zymotic_diseases" = AMR_Zymotic_diseases, "wounds_injuries" = AMR_Wounds_injuries, "other" = AMR_Other)

data_long <- data %>%
  pivot_longer(cols = c(zymotic_diseases, wounds_injuries, other), names_to = "deaths_type", values_to = "AMR")  %>%
  mutate(Year = as.factor(Year))

###### Animated Plot - repeat below for static without animation and inc last 2 lines
# stacked bar
#plot <- ggplot(data = data_long, aes(x = date, y = AMR, fill = deaths_type)) +
  geom_bar(stat = "identity") 

### circular stacked bar
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
nObsType <- nlevels(as.factor(data_long$deaths_type))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data_long)) )
colnames(to_add) <- colnames(data_long)

data_long <- rbind(data_long, to_add)
data_long <- rbind(to_add,data_long)

#data_long <- data_long %>% arrange(Year, date_num)
data_long$id <- rep( seq(1, nrow(data_long)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- data_long %>% group_by(id, date_num,date_text) %>% summarize(tot=sum(AMR))

number_of_bar <- nrow(label_data)
label_data$angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- 0
#label_data$hjust <- ifelse( angle < -90, 1, 0)
#label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data_long %>% 
  group_by(Year) %>%
  summarize(start=min(id), end=max(id)) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
#grid_data <- grid_data[-1,]
# for line 126 in brax max(data$id),5) grid_data$end),3

p <- ggplot(data_long) +      
  # Add the stacked bar
  geom_bar(aes(x = as.factor(id), y = AMR, fill = deaths_type), stat="identity", alpha=0.5) +
  #scale_fill_viridis(discrete = T) +
  scale_fill_manual(values = c("gray31", "red2", "steelblue3"), labels = c("Other", "Wounds", "Preventable Infection")) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 400, xend = start, yend = 400), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 600, xend = start, yend = 600), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 800, xend = start, yend = 800), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 1000, xend = start, yend = 1000), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 1200, xend = start, yend = 1200), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
 
   # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data_long$id) + 1,7), y = c(0, 200, 400, 600, 800, 1000, 1200), label = c("0", "200", "400", "600", "800", "1000", "1200") ,
                    color="grey66", size=4 , angle=0, fontface="bold", family = "Palatino", hjust=0.5) +
  
  labs(title = "Diagram of the Causes of Mortality in the Army in the East", 
       subtitle = "Data shown are annual rate of mortality per 1,000 during the Crimean \n war.  The rate of deaths attributed to preventable infection reduced \n considerably after the introduction of sanitation measures \n in April 1855.") +
  #ylim(-750,3000) +
  ylim(-200, 1400) +

  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x = id, y = tot + 200, label = date_text, hjust=hjust), 
            color="black",alpha=0.8, size=4, angle = label_data$angle, family = "Palatino", inherit.aes = FALSE ) +
  
theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, vjust = -6, face = "bold.italic", family = "Palatino"),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5, vjust = -9, family = "Palatino"),
    legend.position = c(0.3, 0.09),
    legend.justification = c("bottom"),
    legend.key.size = unit(c(0.4,0.4), "cm"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Palatino", face = "italic", size = 12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
    ,plot.background = element_rect(fill = "antiquewhite1")
    ,plot.margin = unit(c(0, 1,0,1), "cm")
    ,panel.background = element_rect(fill = "antiquewhite1", colour = "antiquewhite1")
    
  )  
p  

## animate
a <- p +
  transition_states(id, transition_length = 4, state_length = 2) +
  shadow_mark()

anim_save("floviz2.gif", animation = last_animation())

### tried saving as video but didn't work
final_vid <- animate(a, width = 400, height = 400, renderer = ffmpeg_renderer(format = "mov"))

final <- animate(a, width = 400, height = 400)  

gif_file <- save_gif(final(), width = 400, height = 400)
utils::browseURL(gif_file)
gif_file <- file.path(tempdir(), 'floviz2.gif')
save_gif(final, gif_file, width = 400, height = 400)

anim_save("floviz.mov", animation = last_animation())

  save_animation(a, "floviz.gif"))
#,plot.margin = unit(rep(-1,9), "cm") 

# Add base line information
geom_segment(data=base_data, aes(x = start, y = 2200, xend = end, yend = 2200), colour = "grey", alpha=0.8, size=0.4 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = 3000, label=Year), hjust=c(1,-0.25,0.5), vjust = c(1,0,2), colour = "grey", alpha=0.9, size=3, fontface="bold", inherit.aes = FALSE)

##### Static Plot ########


#------------------------
##### trash ##########
data <- data %>%
  select(-year, -year_)



data <- data %>%
  mutate(year = as.Date(Year, format = "%Y", origin = "1970-01-01"))

data <- data %>%
  mutate(year_ = as.Date.numeric(Year, format = "%Y", origin = lubridate::origin))
         