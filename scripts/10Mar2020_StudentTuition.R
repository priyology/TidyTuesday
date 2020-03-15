library(tidyverse)
library(ggplot2)
library(ggrepel)
library(scales)
library(geofacet)

#### Question: What % of each diversity 'category' is enrolled in each state ... without women / 2+ races? ====

#### load data ====
diversity_school <- read.csv("data/diversity_school.csv", header = TRUE)
View(diversity_school)
glimpse(diversity_school)

#### make variable 'category' a character variable & 'enrollment' an integer ====
diversity_school$category <- as.character(diversity_school$category)
diversity_school$state <- as.character(diversity_school$state)
diversity_school$enrollment <- as.double(diversity_school$enrollment)

#### Create a data.frame without total_enrollment column because it's unclear how that number was calculated ====
diversity_school <- diversity_school %>%
  select(name, state, category, enrollment)
View(diversity_school)
glimpse(diversity_school)

#### omit "women", "2+ races", "total minority" as a category in variable ====
category.enrollment <-  filter(diversity_school, category != "Women" & category != "Two Or More Races" & category != "Total Minority") %>%
  arrange(state) %>%
  ungroup %>%
  as.data.frame()

View(category.enrollment)

#### break up enrollment by state, this gives us how many individuals are enrolled that belong to each category ====
enrollment.state <- category.enrollment %>% 
 filter(state != "NA") %>% 
  group_by(state, category) %>%
  summarize(sum(enrollment)) %>% 
  ungroup %>%
  as.data.frame()

colnames(enrollment.state)[colnames(enrollment.state)=="sum(enrollment)"] <- "enrollment"
View(enrollment.state)

## create column w/ state abbreviations"

state.abbr <- state.abb[match(enrollment.state$state, state.name)]
enrollment.state <- cbind(state.abbr, enrollment.state)

View(enrollment.state)

#### Plot chart that shows enrollment of each category by state ====

Enrollment.Plot <- ggplot(enrollment.state, aes(category, enrollment, fill = category)) +
  geom_col() +
  geom_text(aes(category, enrollment, label = enrollment), hjust = -0.5, size = 3,
            position = position_dodge(width = 1),
            inherit.aes = TRUE) +
  coord_flip() +
  scale_y_continuous(name ="Enrollment Numbers", 
                     breaks=c(0, 100000)) +
  theme_classic() +
  theme(axis.text.x = element_blank()) +
  facet_geo(~ state.abbr) +
  labs(title = "How Enrollment Demographics Vary By State",
       subtitle = "Plotted using geom_col & geofacet",
       caption = "#TidyTuesday Take 2 Based on Advice received on Twitter")

Enrollment.Plot

png('fig_output/EnrollmentPlot_15Mar2020.png',width=1000,height=1000,units="px")

print(Enrollment.Plot)

dev.off()


##########################################


#### other random code from playing around: breakdown of data by women ==== 

#### merge the total_enrollment.state & enrollment.category date.frames & rename columns ====
state_category.enrollment<-merge(x=enrollment.state,y=category.enrollment, by="category", "state")

View(state_category.enrollment)




#### break up enrollment by category ====
enrollment.category <- diversity_school %>% 
  group_by(category, state) %>%
  summarise((sum(enrollment)))%>%
  ungroup %>%
  as.data.frame()

View(enrollment.category)

#### merge the total_enrollment.state & enrollment.category date.frames & rename columns ====
state_category.enrollment<-merge(x=total_enrollment.state,y=enrollment.category,by="state")

colnames(state_category.enrollment)[colnames(state_category.enrollment)=="(sum(unique(total_enrollment)))"] <- "State.Enrollment"
colnames(state_category.enrollment)[colnames(state_category.enrollment)=="(sum(enrollment))"] <- "Enrollment"

View(state_category.enrollment)

#### make variable 'Enrollment' an integer variable ====
state_category.enrollment$Enrollment <- as.integer(state_category.enrollment$Enrollment)

#### create a column for % enrollment per category by state ====
enrollment.percent <- state_category.enrollment %>%
  select(state, category) %>% 
  mutate(state_category.enrollment$Enrollment/ state_category.enrollment$State.Enrollment) %>% 
  pull()

View(enrollment.percent)

#### omit women as a category in variable ====
category.enrollment.eth <-  filter(state_category.enrollment, category != "Women") %>%
  arrange(desc(state)) %>%
  ungroup %>%
  as.data.frame()

View(category.enrollment.eth)

#### create a column for % enrollment per category by state w/o women ====
enrollment.percent.eth <- category.enrollment.eth %>%
  select(state, category) %>%
  mutate(category.enrollment.eth$Enrollment/category.enrollment.eth$State.Enrollment) %>% 
  pull()

enrollment.percent.eth

#### confirm length of 'enrollment.percent' and data.frame 'category.enrollment.eth' are identical ====
length(enrollment.percent.eth)
nrow(category.enrollment.eth)

#### create a new data.frame with state_category.enrollment & enrollment.percent for all categories within each state/university ====
category.enrollment.eth <- cbind(category.enrollment.eth, enrollment.percent.eth)


View(category.enrollment.eth)


#### create a column for % enrollment per institution ====
enrollment_percent <- diversity_school %>%
  mutate(enrollment / total_enrollment) %>% 
  pull()

#### create a new data.frame with diversity & enrollment_percent for all categories within each state/university ====
diversity_school <- cbind(diversity_school, enrollment_percent)

#### break up enrollment by state ====
total_enrollment.state <- diversity_school %>% 
  group_by(state) %>% 
  summarise((sum(unique(total_enrollment))))%>%
  ungroup %>%
  as.data.frame()

View(total_enrollment.state)

#### % women enrolled in institutions
women.enrollment <-  filter(diversity_school, category == "Women") %>%
 arrange(desc(state)) %>%
  ungroup %>%
  as.data.frame()

View(women.enrollment)

women.enrollment.Plot <- ggplot(data = women.enrollment, aes(x = name, y = enrollment_percent)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ state)
  theme_minimal()

women.enrollment.Plot
