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

##creates 1 row / episode format                       
full_episode_text <- full_episode_text %>%
                       mutate(episode_id = factor(episode_id, levels = episode_order$episode_id)) %>%
                       group_by(episode_id) %>% 
                       summarize(episode_full_text = paste0(Text, collapse = " "))


##extracts only the names
##TODO: show example with simply "raj" vs "sheldon" in the blog!
first_names_for_regex <- c("howard",
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

first_names_regex <- paste0(first_names_for_regex, collapse = '|')
first_names_regex <- paste("(", first_names_regex, ")", sep = "")

fnames_in_episodes <- full_episode_text %>%
                        unnest_tokens(output = word, input = episode_full_text) %>%
                        filter(str_detect(word, first_names_regex))

## Standardize names to a common format per name
first_names <- c("Howard",
                 "Penny",
                 "Leonard",
                 "Sheldon",
                 "Raj",
                 "Amy", 
                 "Bernadette",
                 "Stuart",
                 "Zack",
                 "Emily",
                 "Leslie",
                 "Barry")

first_names_mapping <- data.table(regex = first_names_for_regex,
                                  name  = first_names)

len_first_names_mapping <- first_names_mapping %>% nrow()
fnames_in_episodes$name <- ""

for(i in c(1:len_first_names_mapping)) {
  fnames_in_episodes <- fnames_in_episodes %>%
                          mutate(name = ifelse(str_detect(word, first_names_mapping[i, regex]), 
                                               first_names_mapping[i, name], 
                                               name))
}

##
head(fnames_in_episodes)

##one row / episode format for prediction (predictors = # of names appearing)
episodes <- fnames_in_episodes %>%
              select(-word) %>%
              group_by(episode_id, name) %>%
              summarize(name_appear_count = n()) %>%
              tidyr::spread(key = name, value = name_appear_count, fill = 0)


##reading in imdb scores
imdb_scores <- fread("./../Data/Text Mining Final Project/imdb_scores.csv")
imdb_scores <- imdb_scores %>%
                 tidyr::separate(col    = `Episode title`,
                                 into   = c("episode_number", "episode_title"),
                                 #sep    = ".",
                                 remove = TRUE,
                                 extra  = "merge",
                                 fill   = "warn") %>%
                 mutate(episode_number = as.integer(episode_number)) %>% 
                 mutate(episode_id = paste("s", Season, 
                                           "e", episode_number, sep = ""))


head(imdb_scores)
imdb_scores %>% nrow()
episodes %>% nrow()
imdb_scores_name_counts <- imdb_scores %>%
  inner_join(name_counts, by = c("season_number", "episode_number")) %>%
  mutate(episode_id = paste("s", season_number, 
                            "e", episode_number, sep = "")) %>%
  select(-season_number, -episode_number, -episode_title)

