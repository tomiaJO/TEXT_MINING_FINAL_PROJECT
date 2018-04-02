library(tidytext)
library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)

rm(list=ls())

tbbt_subtitles <- readRDS("./../Data/Text Mining Final Project/all_srts_df.rds")


full_episode_text <- tbbt_subtitles %>% 
  select(episode_number, season_number, Text) %>%
  mutate(episode_id = paste("s", season_number, 
                            "e", episode_number, sep = ""))

episode_order <- full_episode_text %>%
  select(season_number, episode_number, episode_id) %>%
  distinct() %>%
  arrange(season_number, episode_number) %>%
  select(episode_id) %>%
  as.vector()

##creates 1 row / episode format                       
full_episode_text <- full_episode_text %>%
  mutate(episode_id = factor(episode_id, levels = episode_order$episode_id)) %>%
  group_by(season_number, episode_id) %>% 
  summarize(episode_full_text = paste0(Text, collapse = " "))

words <- full_episode_text %>%
           unnest_tokens(output = word, input = episode_full_text)

xyz <- full_episode_text %>%
  unnest_tokens(output = bigram, input = episode_full_text, token = "ngrams", n = 2) %>%
  group_by(bigram) %>%
  summarize(count = n()) 
xyz %>%
  filter()
  filter(!str_detect(bigram, "to$")) %>%
  filter(!str_detect(bigram, "'t$")) %>%
  filter(!str_detect(bigram, "was")) %>%
  arrange(desc(count))

# data("stop_words")  
# 
# words <- words %>%
#            anti_join(stop_words, by = "word")

words_to_remove <- data.frame(word = c("â",
                                       "pos",
                                       "ª", "192,220",
                                       "192,210",
                                       "192,200",
                                       "2015",
                                       "1955",
                                       "missy",
                                       "leslie",
                                       "dennis",
                                       "www.addic7ed.com",
                                       "tvshowslatest.blogspot.com",
                                       "alex",
                                       "amy",
                                       "swsub.com",
                                       "leo",
                                       "lalita",
                                       "christy",
                                       "aa",
                                       "way.fr",
                                       "stan",
                                       "don",
                                       "ricardo",
                                       "oontz",
                                       "02",
                                       "elder_man",
                                       "emily",
                                       "claire",
                                       "2016",
                                       "2013",
                                       "stephanie",
                                       "abbott",
                                       "panchali",
                                       "winkle",
                                       "plimpton",
                                       "ã",
                                       "zack",
                                       "priya",
                                       "bernadette",
                                       #"shilly",
                                       #"shally",
                                       #"tondelaya"
                                       "amelia",
                                       "mandy",
                                       "2011",
                                       "43",
                                       "2014",
                                       "kotuwa",
                                       "fw",
                                       "lucy",
                                       "priya's",
                                       "ve",
                                       "duh",
                                       "bert"
                                       ), stringsAsFactors = F)
words <- words %>%
           anti_join(words_to_remove, by = "word")

words <- words %>%
           count(season_number, word, sort = TRUE) %>%
           bind_tf_idf(term = word, document = season_number, n)

words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(season_number) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = season_number)) +
  geom_col(show.legend = F) +
  labs(x=NULL, y = "tf-idf") +
  facet_wrap(~season_number, ncol = 3, scales = "free") + 
  coord_flip()
  
