library(tidyverse)
library(gcookbook)
library(patchwork)
library(hrbrthemes)

# Get the Data
gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

as.data.frame(gdpr_text)
glimpse(gdpr_text)

as.data.frame(gdpr_violations)
glimpse(gdpr_violations)

#scope out dates
unique(gdpr_violations$date)
min(gdpr_violations$date) # 01/01/1970
max(gdpr_violations$date) # 12/20/2019
gdpr_violations$dates <- mdy(gdpr_violations$date)
gdpr_violations$dates

#figuring out the price
length(gdpr_violations$price) #250
options(scipen=999)
max(gdpr_violations$price) #50000000
min(gdpr_violations$price) #0
length(unique(gdpr_violations$price)) #120

#violations below 2.5 million euros
lowprice <- gdpr_violations %>% 
  filter(price < 2500000) %>% 
  filter(dates >= as.Date("2000-01-01") )

#violations above 2.5 million euros
highprice <- gdpr_violations %>% 
  filter(price > 2500000) %>% 
  filter(dates >= as.Date("2000-01-01") )


p1 <- ggplot(lowprice, aes(x = dates, y = name, color = name, size = price)) +
  geom_point() +
  guides(color = FALSE) +
  theme_classic() +
  labs(title="GDPR (General Data Protection Regulation) Violations in the European Union", subtitle = "Fines Less Than 2,500,000 \u20AC", x = "Date", y = "Country Where Violation Was Enforced") +
  theme_ft_rc() +
  scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y") 

p2 <- ggplot(highprice, aes(x = dates, y = name, color = name, size = price)) +
  geom_point() +
  guides(color = FALSE) +
  theme_classic() +
  labs(title=" ", subtitle = "Fines Greater Than 2,500,000 \u20AC", x = "Date", y = "Country Where Violation Occurred", caption = "@priyology") +
  theme_ft_rc() +
  scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y") 

FinalPlots <- p1 + p2

ggsave("fig_output/GDPR_21Apr2020_.png")
  