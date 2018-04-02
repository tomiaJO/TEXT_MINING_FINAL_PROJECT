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
##TODO: add stephanie, missy, alex, plimpton, priya, lucy, bert

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
imdb_scores <- fread("./../Data/Text Mining Final Project/imdb_scores.csv") %>%
                 tidyr::separate(col    = `Episode title`,
                                 into   = c("episode_number", "episode_title"),
                                 #sep    = ".",
                                 remove = TRUE,
                                 extra  = "merge",
                                 fill   = "warn") %>%
                 mutate(episode_number = as.integer(episode_number)) %>% 
                 mutate(episode_id = paste("s", Season, 
                                           "e", episode_number, sep = "")) %>%
                 rename("Episode" = "episode_number") %>%
                 rename("Title" = "episode_title")  %>%
                 mutate(episode_id = factor(episode_id, levels = episode_order$episode_id))

#df for prediction
episodes_w_scores <- imdb_scores %>%
                       inner_join(episodes, by = "episode_id") %>%
                       select(-Season, -Episode, -Title)


##let's predict
training_ratio <- 0.75

set.seed(93) #for reproducibility
train_indices <- createDataPartition(y = episodes_w_scores[["IMDB Score"]],
                                     times = 1,
                                     p = training_ratio,
                                     list = FALSE)

data_train <- episodes_w_scores[train_indices, ]
data_test  <- episodes_w_scores[-train_indices, ]

episodes_w_scores %>% nrow()
data_train %>% nrow()
data_test %>% nrow()

#fitting a model
train_control <- trainControl(method = "cv", number = 5)

glimpse(episodes_w_scores)

set.seed(93)
glm_fit <- train(`IMDB Score` ~ . -episode_id,
                 method = "glm",
                 data = data_train,
                 trControl = train_control,
                 preProcess = c("center", "scale"))

#####################################################
##sidenote for evaluating performance
glm_predictions <- predict.train(glm_fit, newdata = data_test)
test_truth      <- data_test$`IMDB Score` 

actual_vs_predicted <- cbind(predictions = glm_predictions, actual = test_truth) %>% data.frame()
RMSE <- function(x, true_x) sqrt(mean((x - true_x)^2))

#RMSE vs stddev
RMSE(actual_vs_predicted$predictions, actual_vs_predicted$actual)
sd(data_test$`IMDB Score`)

#plotting pred vs actual
actual_vs_predicted %>%
  ggplot(aes(x= actual, y= predictions)) + 
    geom_point() + 
    geom_smooth(method = 'lm')
#####################################################

#let's analyze those coefficients we are here for!
coefficients <- coef(glm_fit$finalModel)[-1]
coefficients <- data.frame(Name      = names(coefficients), 
                           beta      = coefficients,
                           row.names = NULL) %>%
                mutate(Name = reorder(Name, beta))

##TODO: tidy up
coefficients %>%
  ggplot(aes(x = Name, y = beta)) +
  geom_bar(stat = "identity") +
  coord_flip()


episodes_w_scores %>%
  ggplot(aes(x = Sheldon, y = `IMDB Score`)) +
  geom_point() +
  geom_smooth(method = 'lm')

episodes_w_scores %>%
  ggplot(aes(x = Emily, y = `IMDB Score`)) +
  geom_point() +
  geom_smooth(method = 'lm')


imdb_scores %>%
  ggplot(aes(x = Episode, y = `IMDB Score`)) +
  geom_point(alpha = 0.1) +
  facet_grid(~Season)

##TODO: chr progress by season, episode. progress = # of mentions
##TODO: top3 vs bottom 3 episodes