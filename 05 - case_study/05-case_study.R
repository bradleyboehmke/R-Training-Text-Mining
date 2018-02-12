airbnb <- read_rds("data/airbnb.rds")

airbnb %>%
  select(id, description) %>%
  unnest_tokens(words, description)

airbnb %>%
  select(id, description) %>%
  unnest_tokens(words, description, token = "ngrams", n = 2)