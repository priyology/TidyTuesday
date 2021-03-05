library(tidyverse)
library(ggimage)
library(patchwork)
library(hrbrthemes)

villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')
as.data.frame(villagers)

#determine different factors for personality & spp.
unique(villagers$personality)
unique(villagers$species)

#data.frame for spp. counts
counts.sp <- villagers %>% 
group_by(species)%>% 
summarize(n = n()) %>% 
as.data.frame() %>% 
ungroup()

View (counts.sp)


        

counts.sp$images <- c("https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/crocodile_1f40a.png", #alligator
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/badger_1f9a1.png", #anteater
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/bear_1f43b.png", #bear
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/bird_1f426.png", #bird
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/ox_1f402.png", #bull
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/cat-face_1f431.png", #cat
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/rooster_1f413.png", #chicken
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/cow-face_1f42e.png", #cow
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/tiger-face_1f42f.png", #cub
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/deer_1f98c.png", #deer
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/dog_1f415.png", #dog
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/duck_1f986.png", #duck
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/eagle_1f985.png", #eagle
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/elephant_1f418.png", #elephant
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/frog_1f438.png", #frog
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/goat_1f410.png", #goat
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/gorilla_1f98d.png", #gorilla
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/hedgehog_1f994.png", #hamster
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/hippopotamus_1f99b.png", #hippo
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/horse_1f40e.png", #horse
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/kangaroo_1f998.png", #kangaroo
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/koala_1f428.png", #koala
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/lion_1f981.png", #lion
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/monkey_1f412.png", #monkey
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/rat_1f400.png", #mouse
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/octopus_1f419.png", #octopus
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/t-rex_1f996.png", #ostrich
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/penguin_1f427.png", #penguim
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/pig_1f416.png", #pig
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/rabbit_1f407.png", #rabbit
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/rhinoceros_1f98f.png", #rhino
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/ewe_1f411.png", #sheep
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/chipmunk_1f43f.png", #squirrel
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/tiger_1f405.png", #tiger
"https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/wolf_1f43a.png" #wolf
)


#data.frame for personality counts
counts.pers <- villagers %>% 
  group_by(personality) %>% 
  summarize(n = n()) %>% 
  as.data.frame() %>% 
  ungroup()

View(counts.pers)

counts.pers$images2 <- c("https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/pouting-cat_1f63e.png", #cranky
                         "https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/person-lifting-weights_light-skin-tone_1f3cb-1f3fb_1f3fb.png", #jock
                         "https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/sleepy-face_1f62a.png", #lazy
                         "https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/grinning-face_1f600.png", #normal
                         "https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/smiling-face_263a.png", #peppy
                         "https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/relieved-face_1f60c.png", #smug
                         "https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/cat-with-wry-smile_1f63c.png", #snooty
                         "https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/72/twitter/247/hugging-face_1f917.png" #uchi
)

#plot sp. counts
p1 <- ggplot(data = counts.pers, aes(x = reorder(personality, n), y = n)) + 
  geom_point() + 
  geom_image(aes(image = images2), size = 0.03) +
  theme_classic() + 
  theme_ft_rc() +
  theme(axis.text.x = element_text(angle = 45, hjust= 1))+ 
  labs(x = "Personality Type",
       y = "# of Villagers With a Given Personality",
       title ="# of Different Villagers With Certain Personalities in Animal Crossing",
       caption = "images provided by emojipedia")

#plot sp. counts
p2 <- ggplot(data = counts.sp, aes(x = reorder(species, -n), y = n)) + 
  geom_jitter() + 
  geom_image(aes(image = images), size = 0.03) +
  theme_classic() + 
  theme_ft_rc() +
  theme(axis.text.x = element_text(angle = 45, hjust= 1)) + 
  labs(x = "Species",
       y = "# of Villager Species",
       title ="# of Different Villager Species in Animal Crossing",
       caption = "@priyology")

  