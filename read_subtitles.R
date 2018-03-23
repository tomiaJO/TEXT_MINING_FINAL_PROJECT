#devtools::install_github("fkeck/subtools")
library(subtools)
library(tidytext)
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)

rm(list=ls())

##TODO: additional loop across seasons
dirPath  <- "./../Data/Text Mining Final Project/Season1"

srt_list <- list.files(dirPath)

number_of_episodes <- length(srt_list)
list_srt_df        <- vector(mode = "list", length = number_of_episodes) 

episode_number_list <- c(1:number_of_episodes)

for(episode_num in episode_number_list) {
  filePath <- paste(dirPath, srt_list[[episode_num]], sep = "/")
  srt      <- subtools::read.subtitles(file = filePath,
                                       format = "subrip",
                                       encoding = "latin1")
  
  srt_df <- srt$subtitles %>%
              select(-ID) %>%
              mutate(episode_title  = srt_list[[episode_num]],
                     episode_number = episode_num,
                     season_number  = 1) %>%
              mutate(start_in_sec   = period_to_seconds(hms(Timecode.in)),
                     end_in_sec     = period_to_seconds(hms(Timecode.out))) 
  
  last_start_in_sec <- srt_df$start_in_sec %>% max()
  last_end_in_sec   <- srt_df$end_in_sec   %>% max()
  
  srt_df <- srt_df %>%
              mutate(relative_start = start_in_sec / last_start_in_sec)
  
  list_srt_df[[episode_num]] <- srt_df
}

srt_df <- do.call("rbind", list_srt_df)


## check episode lengths to ensure nothing is dropped
srt_df %>%
  group_by(episode_number) %>%
  summarize(line_count  = n(),
            last_second = max(start_in_sec, na.rm = T)) %>%
  ggplot(     aes(x = episode_number)) +
    geom_line(aes(y = line_count))     +
    geom_line(aes(y = last_second))


## basic word count viz
data(stop_words)

srt_df %>%
  select(episode_number, Text) %>%
  unnest_tokens(input = Text, output = word) %>%
  anti_join(stop_words) %>%
  count(episode_number, word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~episode_number, scales = "free")


sentiments_afinn <- get_sentiments("afinn") %>% rename("sentiment_afinn_score" = "score")
sentiments_bing  <- get_sentiments("bing")  %>% rename("sentiment_bing"        = "sentiment")
sentiments_nrc   <- get_sentiments("nrc")   %>% rename("sentiment_nrc"         = "sentiment")

words_w_sentiments <- srt_df %>%
                        unnest_tokens(input = Text, output = word) %>%
                        left_join(sentiments_afinn, by = "word") %>%
                        left_join(sentiments_bing,  by = "word") %>%
                        left_join(sentiments_nrc,   by = "word")
  
