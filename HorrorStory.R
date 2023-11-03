##########################################
###  Text Analysis of Horror Stories   ###
###          Happy Halloween           ###
###             10.10.2023             ###
###            Rosemary Pang           ###
##########################################

## loading packages
library(rvest)
library(dplyr)
library(stm)
library(tidyverse)
library(quanteda)
library(quanteda.dictionaries)
library(quanteda.sentiment)
library(quanteda.textplots)
library(ggplot2)

## Acquiring the data
url <- "https://thoughtcatalog.com/chrissy-stockton/2020/09/scary-stories/"
read_html(url)

css_selector <- "p"

story <- url %>% 
          read_html() %>%
          html_nodes(css = css_selector) %>%
          html_text()

story <- story[-c(1,730:735)]

story <- as.data.frame(story)

# Read in cleaned data
story <- read_csv("/Users/mpang/Desktop/story_clean.csv")%>%
  select(-1)

## We only look into the main content of horror stories
## Data pre-processing
story_corpus <- corpus(story$content)
# remove punctuation, numbers, symbols, etc, and to lower case
story_token <- tokens(story_corpus,
                   remove_punct = T,
                   remove_symbols = T,
                   remove_numbers = T,
                   remove_url = T,
                   split_hyphens = F,
                   include_docvars = T) %>%
  tokens_tolower()
# remove stopwords
story_token <- tokens_select(story_token,
                             pattern=stopwords("en"),
                             selection="remove")

# lemmatization
story_token_lem <- tokens_replace(story_token,
                                  pattern=lexicon::hash_lemmas$token,
                                  replacement = lexicon:: hash_lemmas$lemma)

# create document feature matrix
storyDfm <- story_token_lem %>%
                   dfm()


##### Let the game begin #####
# Find top features of horror stories
topfeatures(storyDfm,50)

# Many top features doesn't have much contextual meanings, such as go, get, one...
# try trim based on the overall frequency
smaller_dfm <- dfm_trim(storyDfm, max_termfreq = 250)
smaller_dfm <- dfm_trim(smaller_dfm, max_docfreq = .5, docfreq_type = "prop")

# Make wordcloud
set.seed(1234)
textplot_wordcloud(smaller_dfm, min_count = 40,
                   random_order = FALSE)

# Try a co-occurrence graph using window 
story_fcm <- fcm(story_token_lem,context = "window", window=2)
story_Features <- names(topfeatures(story_fcm, 30))
story_small_fcm <- fcm_select(story_fcm, pattern = story_Features, selection = "keep")
textplot_network(story_small_fcm, vertex_size = 2)

# Make co-occurrence graph without window, which makes more sense
smaller_fcm <- fcm(smaller_dfm)
myFeatures <- names(topfeatures(smaller_fcm,30))
# retain only those top features as part of our matrix
even_smaller_fcm <- fcm_select(smaller_fcm, pattern = myFeatures, selection = "keep")
dim(even_smaller_fcm)
# compute size weight for vertices in network
size <- log(colSums(even_smaller_fcm))
# create plot
textplot_network(even_smaller_fcm, vertex_size = size/ max(size) * 3)




## What are top haunted places, people, and things they saw
# Convert fullDfm into data frame
full_df <- convert(storyDfm,to="data.frame")
# Locations that horror story happens
location <- c("apartment","basement","bathroom","cabin",
              "campus","farm","field","forest","boat","fence",
              "dorm","hallway","highway","hill","hospital",
              "hotel","kitchen","lake","military","motel",
              "park","mine","patio","railroad","road","school",
              "stair","street","tower","tunnel","wood","yard")
full_location <- full_df[,location]

full_location2 <- as.data.frame(t(full_location)) 

full_location2 <- full_location2 %>% 
  mutate(count = rowSums(.))

full_location2 <- tibble::rownames_to_column(full_location2, "location")

full_top_location <- head(full_location2 %>% arrange(desc(count)),10)


ggplot(full_top_location, aes(x=location, y=count)) +
  geom_bar(stat="identity")

# When you see things
time <- c("afternoon","autumn","halloween","morning",
              "night","november","october","pm","sunday","weekend")
full_time <- full_df[,time]

full_time2 <- as.data.frame(t(full_time)) 

full_time2 <- full_time2 %>% 
  mutate(count = rowSums(.))

full_time2 <- tibble::rownames_to_column(full_time2, "time")

top_time <- head(full_time2 %>% arrange(desc(count)),5)

ggplot(top_time, aes(x=time, y=count)) +
  geom_bar(stat="identity")

# People who experience horror story
people <- c("aunt","dad","daughter","farmer","father","friend",
          "boyfriend","cousin","ex-wife","girlfriend",
          "grandad","grandfather","grandma","grandmother",
          "grandpa","grandparent","mom","parent","sister",
          "son","student","teacher","doctor","uncle","wife",
          "husband","mother")
full_people <- full_df[,people]

full_people2 <- as.data.frame(t(full_people)) 

full_people2 <- full_people2 %>% 
  mutate(count = rowSums(.))

full_people2 <- tibble::rownames_to_column(full_people2, "people")
#combine same people
full_people3$count <- c(10,0,9,11,72,129,5,10,1,14,0,34,
                        0,34,0,13,0,33,47,29,9,2,1,35,21,12,119)
  
top_people <- head(full_people3 %>% arrange(desc(count)),10)

ggplot(top_people, aes(x=people, y=count)) +
  geom_bar(stat="identity")


# What you see or hear
things <- c("freak","dead","demon","ghost","goblin","gurgle",
            "boom","man","bell","bowl","intruder","knock",
            "mirror","noise","scream","shadow","sing",
            "soldier","spirit","spook","thing","vision",
            "whisper")
full_things <- full_df[,things]

full_things2 <- as.data.frame(t(full_things)) 

full_things2 <- full_things2 %>% 
  mutate(count = rowSums(.))

full_things2 <- tibble::rownames_to_column(full_things2, "things")

top_things <- head(full_things2 %>% arrange(desc(count)),10)

ggplot(top_things, aes(x=things, y=count)) +
  geom_bar(stat="identity")


### Detect most horrifying stories using sentiment analysis
## use DFM
story_sen <- storyDfm %>%
  dfm_lookup(data_dictionary_NRC)
head(story_sen,10)

df_sen <- convert(story_sen,to="data.frame")
ggplot(df_sen)+
  geom_histogram(aes(x=fear))+
  theme_bw()

# use text
story_sen_text <- liwcalike(story_corpus,data_dictionary_NRC)

ggplot(story_sen_text)+
  geom_histogram(aes(x=fear))+
  theme_bw()

quantile(story_sen_text$fear)
# the 75% quantile of fear is 1.97. Let's look into stories with fear scores equal to or greater than 2
fear_corpus <- story_corpus[which(story_sen_text$fear >= 2)]

### Work with fear_corpus
# remove punctuations, numbers, symbols, etc, and to lower case
fear_token <- tokens(fear_corpus,
                     remove_punct = T,
                     remove_symbols = T,
                     remove_numbers = T,
                     remove_url = T,
                     split_hyphens = F,
                     include_docvars = T) %>%
  tokens_tolower()
# remove stopwords
fear_token <- tokens_select(fear_token,
                            pattern=stopwords("en"),
                            selection="remove")

# lemmatization
fear_token_lem <- tokens_replace(fear_token,
                                 pattern=lexicon::hash_lemmas$token,
                                 replacement = lexicon:: hash_lemmas$lemma)

# create document feature matrix
fearDfm <- fear_token_lem %>%
  dfm()
# Make wordcloud
set.seed(1234)
textplot_wordcloud(fearDfm, min_count = 20,
                   random_order = FALSE)
# Make co-occurrence graph without window, which makes more sense
fear_fcm <- fcm(fearDfm)
myFeatures <- names(topfeatures(fear_fcm,30))
# retain only those top features as part of our matrix
even_smaller_fcm <- fcm_select(fear_fcm, pattern = myFeatures, selection = "keep")
dim(even_smaller_fcm)
# compute size weight for vertices in network
size <- log(colSums(even_smaller_fcm))
# create plot
textplot_network(even_smaller_fcm, vertex_size = size/ max(size) * 3)


# Topic modeling
# full dfm (doesn't make sense)
full_topic <- stm(storyDfm, K = 5,
                  verbose = FALSE, init.type = "Spectral")
plot(full_topic,type="summary")

# top fear dfm
cor_topic_model <- stm(fearDfm, K = 5,
                       verbose = FALSE, init.type = "Spectral")
plot(cor_topic_model,type="summary")
