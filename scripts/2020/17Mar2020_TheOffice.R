#### Question -- how often is "that's what she said" uttered across the series? ====


#### Install Packages for The Offic Data ("schrute") & text analysis ("tidytext", "tokenizers") ====

#install.packages("schrute")
#install.packages("tidytext")
#install.packages("tokenizers")
#install.packages("ggpubr")

#### Load Packages ====

library(schrute)
library(tidytext)
library(tidyverse)
library(tokenizers)
library(ggpubr)
library(fishualize)

#### Create data.frame & get a sense of the data ====
OfficeData <- theoffice
glimpse(OfficeData)
View(OfficeData)

#### isolate the phrase "that's what she said" ====

sentences <- tokenize_sentences(OfficeData$text)
sentences <- lower_string <- tolower(sentences)
OfficeData <- cbind(OfficeData, sentences)

FilterSheSaid <- OfficeData[grep("that's what she said", OfficeData$sentences), ]

FilterSheSaid$season <- as.integer(FilterSheSaid$season)

View(FilterSheSaid)
glimpse(FilterSheSaid)


#### Plot the frequency of "that's what she said" per season ====

png("fig_output/SheSaidPlot_21Mar2020.png")

SheSaid.Plot <- ggplot(FilterSheSaid, aes(x = season, fill = factor(season))) +
  geom_bar(width = 0.5) +
  scale_x_continuous(name = "Season", breaks = seq(0, 9, 1)) +
  scale_y_continuous(name="'That's what she said.' frequency", breaks = seq(0, 10, 1)) +
  scale_fill_fish_d(option = "Lepomis_megalotis") +
  guides(fill=FALSE) +
  ggtitle("# of Times A Character on 'The Office' said 'That's what she said.' ") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

print(SheSaid.Plot)
dev.off()


#### Plot the frequency of Michael saying "that's what she said" per season ====

SheSaid.Michael <- FilterSheSaid %>% 
filter(character == 'Michael') %>%
ungroup %>%
as.data.frame()

View(SheSaid.Michael)

png("fig_output/MichaelPlot21Mar2020.png")

Michael.Plot <- ggplot(SheSaid.Michael, aes(x = season, fill = factor(season))) +
  geom_bar(width = 0.5) +
  scale_x_continuous(name = "Season", breaks = seq(0, 9, 1)) +
  scale_y_continuous(name="'That's what she said.' frequency", breaks = seq(0, 10, 1)) +
  scale_fill_fish_d(option = "Hypsypops_rubicundus") +
  guides(fill=FALSE) +
  ggtitle("# of Times Michael from 'The Office' said 'That's what she said.' ") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

print(Michael.Plot)
dev.off()

#### Other Random code ==== 


#### How many times is "that's what she said uttered during each season ====
#SheSaidCount <- FilterSheSaid %>% 
# group_by(season) %>%
#summarize(length(sentences)) %>%
#ungroup %>%
#as.data.frame()

#colnames(SheSaidCount)[colnames(SheSaidCount)=="length(sentences)"] <- "num.utterances"

#View(SheSaidCount)

#### merge the # of utterances with the original dataset ====
#FilterSheSaid <- merge(FilterSheSaid, SheSaidCount)

#View(FilterSheSaid)

#make sure the numerical characters are just numeric
#FilterSheSaid$num.utterances <- as.integer(FilterSheSaid$num.utterances)

