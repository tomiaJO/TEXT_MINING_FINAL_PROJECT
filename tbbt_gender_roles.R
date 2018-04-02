library(tidytext)
library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)

https://github.com/juliasilge/women-in-film/blob/master/find_bigrams.Rmd

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
  summarize(episode_full_text = paste0(Text, collapse = " ")) %>%
  ungroup()

head(full_episode_text)

bigrams <- full_episode_text %>%
             unnest_tokens(output = bigram, 
                           input  = episode_full_text, 
                           token  = "ngrams", 
                           n      = 2)

#### import gender lexicon ####
gender_word_lexicon <- fread("./../Data/Text Mining Final Project/gender_word_lexicon.csv")

gender_words_regex <- paste0(gender_word_lexicon$word, collapse = '|')
gender_words_regex <- paste("^(", gender_words_regex, ") ", sep = "")

gender_bigrams <- bigrams %>%
                    filter(str_detect(bigram, gender_words_regex)) %>%
                    mutate(gender_word = str_split(bigram, pattern = " ", n = 2, simplify = TRUE)[,1],
                           word        = str_split(bigram, pattern = " ", n = 2, simplify = TRUE)[,2])

data("stop_words")
words_to_remove <- data.frame(word = c("s", "co",
                                       "farrah", "winkle",
                                       "maryann", "cooper",
                                       "wolowitz", "kripke",
                                       "cooper's", "nimoy",
                                       "barnett", "joel",
                                       "shh", "lee", "plimpton"))

gender_bigrams <- gender_bigrams %>%
                    anti_join(stop_words, by = "word") %>%
                    anti_join(words_to_remove, by = "word")
  
gender_bigrams %>% head()
gender_words <- gender_bigrams %>%
                    inner_join(gender_word_lexicon, by = c("gender_word" = "word")) %>%
                    select(gender, word)
  
  
gender_words <- gender_words %>%
                    count(gender, word, sort = TRUE) %>%
                    bind_tf_idf(term = word, document = gender, n)

gender_words %>% nrow()
gender_words$word %>% unique()

gender_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(n = ifelse(gender == "female", -1*n, n)) %>%
  mutate(word = reorder(word, -n)) %>%
  group_by(gender) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = gender)) +
  geom_col(show.legend = F) +
  labs(x=NULL, y = "tf-idf") +
  #facet_wrap(~gender, ncol = 2, scales = "free") + 
  coord_flip()

