#devtools::install_github("fkeck/subtools")
library(subtools)
library(tidytext)
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)

rm(list=ls())

number_of_seasons  <- 9
list_episode_df    <- vector(mode = "list", length = number_of_seasons) 
season_number_list <- c(1:number_of_seasons) 

##TODO: additional loop across seasons
for(season_num in season_number_list) {
  
  dirPath  <- paste("./../Data/Text Mining Final Project/Season", season_num, sep = "")
  srt_list <- list.files(dirPath)

  number_of_episodes <- length(srt_list)
  list_srt_df        <- vector(mode = "list", length = number_of_episodes) 
  episode_number_list <- c(1:number_of_episodes)
  
  print(paste("Season ", season_num, ": ", number_of_episodes, sep = ""))
  
  for(episode_num in episode_number_list) {
    filePath <- paste(dirPath, srt_list[[episode_num]], sep = "/")
    srt      <- subtools::read.subtitles(file = filePath,
                                         format = "subrip",
                                         encoding = "latin1")
    
    srt_df <- srt$subtitles %>%
      select(-ID) %>%
      mutate(episode_title  = srt_list[[episode_num]],
             episode_number = episode_num,
             season_number  = season_num) %>%
      mutate(start_in_sec   = period_to_seconds(hms(Timecode.in)),
             end_in_sec     = period_to_seconds(hms(Timecode.out))) 
    
    last_start_in_sec <- srt_df$start_in_sec %>% max()
    last_end_in_sec   <- srt_df$end_in_sec   %>% max()
    
    srt_df <- srt_df %>%
      mutate(relative_start = start_in_sec / last_start_in_sec)
    
    list_srt_df[[episode_num]] <- srt_df
  }
  
  list_episode_df[[season_num]] <- do.call("rbind", list_srt_df)
}

all_srts_df <- do.call("rbind", list_episode_df)



## check episode lengths to ensure nothing is dropped
all_srts_df %>%
  group_by(season_number, episode_number) %>%
  summarize(line_count  = n(),
            last_second = max(start_in_sec, na.rm = T)) %>%
  ggplot(    aes(x = episode_number)) +
    geom_bar(aes(y = -1 * line_count),  stat = "identity", fill = "orange") +
    geom_bar(aes(y = last_second),      stat = "identity", fill = "blue") +
    facet_wrap(~season_number, scales = "free")

all_srts_df %>%
  fwrite("./../Data/Text Mining Final Project/all_subtitles_df.csv")

all_srts_df %>%
  save(file="./../Data/Text Mining Final Project/all_srts_df.Rda")

# all_srts_df %>% 
#   filter(season_number == 3 & episode_number == 4)




########################
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
    facet_wrap(~episode_number, scales = "free_y")


########################
## playing with sentiments
sentiments_afinn <- get_sentiments("afinn") %>% rename("sentiment_afinn_score" = "score")
sentiments_bing  <- get_sentiments("bing")  %>% rename("sentiment_bing"        = "sentiment")
sentiments_nrc   <- get_sentiments("nrc")   %>% rename("sentiment_nrc"         = "sentiment")

words_w_sentiments <- srt_df %>%
                        unnest_tokens(input = Text, output = word) %>%
                        left_join(sentiments_afinn, by = "word") %>%
                        left_join(sentiments_bing,  by = "word") %>%
                        left_join(sentiments_nrc,   by = "word")
  
words_w_sentiments %>%
  filter(!is.na(sentiment_afinn_score)) %>%
  mutate(index = start_in_sec %/% 300) %>%
  group_by(episode_number, index) %>%
  summarize(sentiment = sum(sentiment_afinn_score)) %>%
  ggplot(aes(x = index, y = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~episode_number)
