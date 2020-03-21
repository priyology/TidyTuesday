#### Question -- how often is "that's what she said" uttered across the series? ====


#### Install Packages for The Offic Data ("schrute") & text analysis ("tidytext", "tokenizers") ====

#install.packages("schrute")
#install.packages("tidytext")
#install.packages("tokenizers")
#install.packages("ggforce")

#### Load Packages ====

library(schrute)
library(tidytext)
library(tidyverse)
library(tokenizers)
library(ggforce)

#### Create data.frame & get a sense of the data ====
OfficeData <- theoffice
glimpse(OfficeData)
View(OfficeData)

#### isolate the phrase "that's what she said" ====

sentences <- tokenize_sentences(OfficeData$text)
sentences <- lower_string <- tolower(sentences)
OfficeData <- cbind(OfficeData, sentences)

FilterSheSaid <- OfficeData[grep("that's what she said", OfficeData$sentences), ]

#### How many times is "that's what she said uttered during each season ====
SheSaidCount <- FilterSheSaid %>% 
  group_by(season) %>%
  summarize(length(sentences)) %>%
  ungroup %>%
  as.data.frame()

colnames(SheSaidCount)[colnames(SheSaidCount)=="length(sentences)"] <- "num.utterances"

View(SheSaidCount)

#### merge the # of utterances with the original dataset ====
#FilterSheSaid <- merge(FilterSheSaid, SheSaidCount)

#View(FilterSheSaid)

#make sure the numerical characters are just numeric
FilterSheSaid$num.utterances <- as.integer(FilterSheSaid$num.utterances)
FilterSheSaid$season <- as.integer(FilterSheSaid$season)

glimpse(FilterSheSaid)


#### Plot the frequency of "that's what she said" per season ====

SheSaid.Plot <- ggplot(SheSaidCount, aes(num.utterances)) +
  geom_bar() +
  scale_x_discrete(name ="Season of 'The Office'", limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  scale_y_discrete(name="# of times a character said 'That's what she said.'", limits=c("0", "1", "2", "3", "4", "5", "6", "7")) +
  theme_classic()

print(SheSaid.Plot)
