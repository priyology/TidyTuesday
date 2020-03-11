library(tidyverse)
library(ggplot2)

diversity_school <- read.csv("data/diversity_school.csv", header = TRUE)
View(diversity_school)
glimpse(diversity_school)

diversity_school$category <- as.character(diversity_school$category)

enrollment_percent <- diversity_school %>%
  mutate(enrollment / total_enrollment) %>% 
  pull()

diversity_school <- cbind(diversity_school, enrollment_percent)

women.enrollment <-  filter(diversity_school, category == "Women") %>%
 arrange(desc(enrollment_percent)) %>%
  ungroup %>%
  as.data.frame()

View(women.enrollment)

head(women.enrollment)
tail(women.enrollment)

women.enrollment.Plot <- ggplot(data = women.enrollment, aes(x = name, y = enrollment_percent)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ state) +
  theme_minimal()

women.enrollment.Plot
