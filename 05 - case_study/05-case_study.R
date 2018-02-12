
# Prerequisites -----------------------------------------------------------

# package prereqs
library(tidyverse)
library(tidytext)

# data prereq
airbnb <- read_rds("data/airbnb.rds")


# Task 1 ------------------------------------------------------------------

airbnb %>%
  select(id, description) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
