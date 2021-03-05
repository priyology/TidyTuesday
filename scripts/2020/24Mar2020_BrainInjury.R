#### Question: How does injury mechanism vary with age & type? ====

library(tidyverse)
library(ggsci)
library(ggrepel)
library(patchwork)
library(ggtext)
library(gganimate)
library(gridExtra)

# Get the Data
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
View(tbi_age)

unique(tbi_age$injury_mechanism)


#### Filter out the weird age grouping: "0-17" ====
tbi_age2  <- filter(tbi_age, age_group != "0-17") %>%
  group_by(injury_mechanism) %>%
  filter(injury_mechanism != "Other unintentional injury, mechanism unspecified") %>% 
  filter(injury_mechanism != "Other or no mechanism specified")

#### Remova NAs ====
tbi_age2 <- na.omit(tbi_age2)
View(tbi_age2)

Deaths <- filter(tbi_age2, type == "Deaths")

EmergencyDept <- filter(tbi_age2, type == "Emergency Department Visit")

View(EmergencyDept)

Hospital <- filter(tbi_age2, type == "Hospitalizations")


#### Deaths Plot ====

DeathPlot <- ggplot(Deaths, aes(x = age_group, y = number_est, fill = injury_mechanism)) +
  geom_bar(stat = "identity") +
  scale_fill_simpsons() +
  labs(title = "Injury Resulted in Death", x = "Ages", y = "Estimated # of Injuries", fill = "Injury Mechanism") +
  theme(
    plot.title = element_text(size = 14, family = "bold", hjust = 1),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  theme_classic()
  
DeathPlot


#### Emergency Plot ====

EmergPlot <- ggplot(EmergencyDept, aes(x = age_group, y = number_est, fill = injury_mechanism)) +
  geom_bar(stat = "identity") +
  scale_fill_simpsons() +
  labs(title = "Injury Resulted in ER Visit", x = "Ages", y = "Estimated # of Injuries", fill = "Injury Mechanism") +
  theme(
    plot.title = element_text(size = 14, family = "bold", hjust = 1),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  theme_classic()

EmergPlot


#### Hospitalization Plot ====

HosPlot <- ggplot(Hospital, aes(x = age_group, y = number_est, fill = injury_mechanism)) +
  geom_bar(stat = "identity") +
  scale_fill_simpsons() +
  labs(title = "Injury Resulted in Hospitalization", x = "Ages", y = "Estimated # of Injuries", fill = "Injury Mechanism") +
  theme(
    plot.title = element_text(size = 14, family = "bold", hjust = 1),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  theme_classic()

HosPlot

grid.arrange(EmergPlot, HosPlot, DeathPlot, nrow = 3, 
             top = textGrob("Brain Injury Mechanisms by Age Group", 
                    gp = gpar(fontsize=20,font=3)))

#### Basic Plot of All Data

BrainInjury.Plot <- ggplot(tbi_age2, aes(x = age_group, y = number_est, fill = injury_mechanism)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ type) +
  theme_classic() +
  scale_fill_simpsons() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))

BrainInjury.Plot


#### Failplot.1: Circular Bar Chart ====
data <- tbi_age2

# Set a number of 'empty bar'
empty_bar <- 10

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Plot
p <- ggplot(data, aes(x = as.factor(id), y = number_est, fill = age_group)) +
  # This add the bars with a blue color
  geom_bar(stat="identity", alpha = 0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y= number_est + 10, label= age_group , hjust=hjust), color="black", fontface="bold", alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE)

p


#### Failplot.2: Nyssa's circular Bar chart ====

## select a few countries to label
labels_data <- tbi_age2 %>%
  group_by(age) %>%
  summarise(y = sum(number_est)) %>%
  arrange(desc(y))

labels_data

## Plot this mofo
p2 <- ggplot(tbi_age2) +
  # Add the stacked bar
  geom_bar(aes(x = age_group, y = number_est, fill = injury_mechanism), stat="identity", alpha=0.5) +
  # Add text showing the value of each 100/75/50/25 lines
  geom_hline(yintercept = c(400, 800, 1200), lty = 2, color = "grey" )+
  annotate("text", x = c(10,10,10), y = c(225, 425, 625), 
           label = c("200","400",expression(paste("1200 * 10", "2"^-1))), angle = 340) +
  scale_fill_uchicago() +
  geom_label_repel(data = labels_data, 
                   mapping = aes(x = injury_mechanism, y = y, label = injury_mechanism), 
                   segment.size = .5, nudge_y = 70, nudge_x = 2)+
  labs(caption = "Brain Injuries by age & mechanism")+
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.caption = element_text(hjust = 0.5, size = 16, vjust = 10),
    legend.title = element_blank()
  ) +
  coord_polar()

p2
