
# Prerequisites -----------------------------------------------------------

# packages
library(tidyverse)
library(tidytext)
library(harrypotter)

# example data (harry potter)
ps_df <- tibble(
  chapter = seq_along(philosophers_stone),
  text    = philosophers_stone
)

# exercise data
url <- "https://raw.githubusercontent.com/kwartler/text_mining/master/bos_airbnb_1k.csv"
reviews <- read_csv(url)


# tf-idf ------------------------------------------------------------------

# computing tf-idf
ps_df %>%
  unnest_tokens(word, text) %>%
  count(chapter, word) %>%
  bind_tf_idf(word, chapter, n) %>%
  arrange(chapter, desc(tf_idf))

# term frequency in airbnb reviews
reviews %>%
  select(property_type, comments) %>%
  filter(property_type %in% c("Apartment", "House")) %>%
  unnest_tokens(word, comments, token = "ngrams", n = 2) %>%
  separate(word, into = c("word1", "word2"), sep = " ") %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word
  ) %>%
  unite(word, word1, word2) %>%
  count(property_type, word, sort = TRUE) %>%
  group_by(property_type) %>%
  top_n(10) %>%
  ggplot(aes(drlib::reorder_within(word, n, property_type, sep = "."), n)) +
  geom_col() +
  facet_wrap(~ property_type, scales = "free") +
  coord_flip()

# Your Turn!
reviews %>%
  select(property_type, comments) %>%
  filter(property_type %in% c("Apartment", "House")) %>%
  unnest_tokens(word, comments, token = "ngrams", n = 2) %>%
  separate(word, into = c("word1", "word2"), sep = " ") %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word
  ) %>%
  unite(word, word1, word2) %>%
  count(property_type, word, sort = TRUE) %>%
  bind_tf_idf(word, property_type, n) %>%
  group_by(property_type) %>%
  top_n(10) %>%
  ggplot(aes(drlib::reorder_within(word, tf_idf, property_type, sep = "."), tf_idf)) +
  geom_col() +
  facet_wrap(~ property_type, scales = "free") +
  coord_flip()

# What if we want to understand the unique differences between the top 4 most
# reviewed neighborhoods
reviews %>%
  count(neighbourhood_cleansed, sort = TRUE)

# bi-gram term frequency of these neighborhoods  
reviews %>%
  select(neighborhood = neighbourhood_cleansed, comments) %>%
  filter(neighborhood %in% c("Jamaica Plain", "South End",
                                       "Dorchester", "East Boston")) %>%
  unnest_tokens(word, comments, token = "ngrams", n = 2) %>%
  separate(word, into = c("word1", "word2"), sep = " ") %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word
  ) %>%
  unite(word, word1, word2) %>%
  count(neighborhood, word, sort = TRUE) %>%
  group_by(neighborhood) %>%
  top_n(5) %>%
  ggplot(aes(drlib::reorder_within(word, n, neighborhood, sep = "."), n)) +
  geom_col() +
  facet_wrap(~ neighborhood, scales = "free") +
  coord_flip()

# Your turn!
reviews %>%
  select(neighborhood = neighbourhood_cleansed, comments) %>%
  filter(neighborhood %in% c("Jamaica Plain", "South End",
                             "Dorchester", "East Boston")) %>%
  unnest_tokens(word, comments, token = "ngrams", n = 2) %>%
  separate(word, into = c("word1", "word2"), sep = " ") %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word
  ) %>%
  unite(word, word1, word2) %>%
  count(neighborhood, word, sort = TRUE) %>%
  bind_tf_idf(word, neighborhood, n) %>%
  group_by(neighborhood) %>%
  top_n(5) %>%
  ggplot(aes(drlib::reorder_within(word, tf_idf, neighborhood, sep = "."), tf_idf)) +
  geom_col() +
  facet_wrap(~ neighborhood, scales = "free") +
  coord_flip()


# Word networks -----------------------------------------------------------


