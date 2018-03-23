#devtools::install_github("fkeck/subtools")
library(subtools)
library(tidytext)
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)

rm(list=ls())


dirPath <- "./../Data/Text Mining Final Project/Season1"
srt_list <- list.files(dirPath)

filePath <- paste(dirPath, srt_list[[1]], sep = "/")
srt <- subtools::read.subtitles(filePath)

srt_df <- srt$subtitles %>%
            select(-ID) %>%
            mutate(episode_title = str_list[[1]],
                   srt_df        = 1) %>%
            mutate(start_in_sec  = period_to_seconds(hms(Timecode.in)),
                   end_in_sec    = period_to_seconds(hms(Timecode.out))) 

last_start_in_sec <- srt_df$start_in_sec %>% 
                       max()
last_end_in_sec   <- srt_df$end_in_sec   %>% 
                       max()



## basic word count viz
data(stop_words)

srt_df %>%
  select(Text) %>%
  unnest_tokens(input = Text, output = word) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
    geom_col() +
    coord_flip()


