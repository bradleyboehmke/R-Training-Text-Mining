
# Prerequisites -----------------------------------------------------------

## packages we'll use
library(tidyverse)
library(tidytext)

## data we'll use
airbnb <- read_rds("data/airbnb.rds")


# Warm-up Exercises -------------------------------------------------------

# 1. What is the most common name in the host_name column?
airbnb %>% 
  select(host_name) %>%
  mutate(host_name = str_to_lower(host_name)) %>%
  count(host_name, sort = TRUE)

# 2. Filter out all observations that advocate for no shoes in their house_rules 
airbnb %>%
  filter(!str_detect(house_rules, regex("no shoes", ignore_case = TRUE)))

# 3. Find and plot the top 10 most commonly used words in the description field
airbnb %>% 
  select(id, description) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col() +
  coord_flip()

# 4. Find and plot the top 10 most commonly used bi-grams in the description field
airbnb %>% 
  select(id, description) %>%
  unnest_tokens(word, description, token = "ngrams", n = 2) %>%
  separate(word, into = c("word1", "word2"), sep = " ") %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word
  ) %>%
  unite(word, word1, word2, sep = " ") %>%
  count(word) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col() +
  coord_flip()




