library(tidytext)
library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)
library(caret)

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
                       
full_episode_text <- full_episode_text %>%
                       mutate(episode_id = factor(episode_id, levels = episode_order$episode_id)) %>%
                       group_by(episode_id) %>% 
                       summarize(episode_full_text = paste0(Text, collapse = " "))

#########################
## just using first names
first_name <- c("sheldon", "leonard", "penny",
                "howard", "amy", "raj", "bernadette",
                "stuart", "zack", "rajesh", "emily",
                "leslie", )

##show example with simply "raj" vs "sheldon" in the blog!

first_names <- c("howard",
                "penny",
                "leonard",
                "sheldon",
                "^raj[e']?",
                "^(sh)?amy", 
                "^bern[ias]",
                "stuart",
                "zack",
                "emil[ey]",
                "leslie",
                "barry")


fnames_in_episodes <- full_episode_text %>%
                        unnest_tokens(output = word, input = episode_full_text) %>%
                        filter(str_detect(word, first_name))

unique(fnames_in_episodes$word)

fnames_in_episodes %>% filter(word == "raj'll")


