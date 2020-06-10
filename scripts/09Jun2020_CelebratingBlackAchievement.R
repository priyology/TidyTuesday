library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)
library(scales)

#get the data
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

#scope out the data
View(firsts)
View(science)

#how many occupations are there -- how many are unique
length(science$occupation_s)
unique(science$occupation_s)  #many held multiple occupations


#histogram of differnt job types
ggplot(science, aes(x = occupation_s)) + geom_histogram(stat = "count") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90))

#create a category to identify whether inventor or not
science$occupation_s <- tolower(science$occupation_s)
science$inventor <- ifelse(str_detect(science$occupation_s, "inventor"), "inventor", "not")


#create a dataframe for 
TL <- data.frame(birth = science$birth,
                 achievement = paste(science$name, ": ", science$occupation_s),
                 false_date = paste("01-", "01-", science$birth, sep = ""),
                 occupation = science$occupation_s,
                 inventor = science$inventor)

#remove NAs
TL <- na.omit(TL)

#confirm that TL$false_date reads as a date
TL$false_date <-  mdy(TL$false_date) #lubridate function
min(TL$false_date) #1731-01-01
max(TL$false_date) #1980-01-01
glimpse(TL)

#identifying whether or not someone is an inventor
inventor_levels <- c("inventor", "not")
inventor_colors <- c("#0070C0", "#00B050")

TL$inventor <- factor(TL$inventor, levels = inventor_levels, ordered = TRUE)

#set up positionality of timeline
positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions <- c(1, -1)

line_pos <- data.frame(
  "false_date" = unique(TL$false_date),
  "position" = rep(positions, length.out=length(unique(TL$false_date))),
  "direction" = rep(directions, length.out=length(unique(TL$false_date)))
)

TL <- merge(x = TL, y = line_pos, by = "false_date", all = TRUE)
TL <- TL[with(TL, order(false_date, inventor)), ]

text_offset <- 0.5

#data.frame containing all years

TL$year_count <- ave(TL$false_date == TL$false_date, TL$false_date, FUN=cumsum)
TL$text_position <- (TL$year_count * text_offset * TL$direction) + TL$position

year_date_range <- seq(min(TL$false_date), max(TL$false_date), by = 'year')

year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit = "year"),
    floor_date(year_date_range, unit = "year")
  ),  origin = "01-01-1970"
)

year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)


#timeline plot

TL_plot <- ggplot(TL,aes(x = false_date, y = 0, col = inventor, label = achievement)) + 
           labs(col = "Achievements") +
            scale_color_manual(values = inventor_colors, labels = inventor_levels, drop = FALSE) +
            theme_classic()

# Plot horizontal black line for timeline
TL_plot <- TL_plot + geom_hline(yintercept=0,  color = "black", size = 0.3)

# Plot vertical segment lines for milestones
TL_plot <- TL_plot + geom_segment(data = TL[TL$year_count == 1,], aes(y = position, yend = 0, xend = false_date), color = 'black', size = 0.2)

# Plot scatter points at zero and date
TL_plot <- TL_plot + geom_point(aes(y = 0), size = 3)

# Don't show axes, appropriately position legend
TL_plot <- TL_plot + theme(axis.line.y=element_blank(),
                           axis.text.y=element_blank(),
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(),
                           axis.ticks.y=element_blank(),
                           axis.text.x =element_blank(),
                           axis.ticks.x =element_blank(),
                           axis.line.x =element_blank(),
                           legend.position = "bottom"
)

# Show year text
TL_plot <- TL_plot + geom_text(data = year_df, aes(x = year_date_range, y = -0.2, label = year_format, fontface = "bold"), size = 2.5, color = 'black')


# Show text for each milestone
TL_plot <- TL_plot + geom_text(aes(y = text_position, label = achievement), size=2.5)

####


TL_plot

