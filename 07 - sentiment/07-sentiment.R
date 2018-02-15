
# Prerequisites -----------------------------------------------------------

library(tidyverse)
library(tidytext)
library(harrypotter)


# Sentiment lexicons ------------------------------------------------------

# sentiment words are provided in one data set
sentiments

# but come from three different well-established lexicons
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


# Basic sentiment analysis ------------------------------------------------

# lets tidy the philosophers_stone book
ps_df <- tibble(
  chapter = seq_along(philosophers_stone),
  text    = philosophers_stone
) %>%
  unnest_tokens(word, text)

# notice how "boy" and "proud" have more than one feeling
ps_df %>%
  inner_join((get_sentiments("nrc")))

# we can see the book is slightly more negative than positive and "sadness" and
# "anger" are the top emotions identified
ps_df %>%
  inner_join((get_sentiments("nrc"))) %>%
  count(sentiment, sort = TRUE)

# we get a similar result with the Bing lexicon
ps_df %>%
  inner_join((get_sentiments("bing"))) %>%
  count(sentiment, sort = TRUE)

# YOUR TURN!
# Using the AFINN lexicon, can you rank-order the chapters by sentiment score?
ps_df %>%
  inner_join((get_sentiments("afinn"))) %>%
  group_by(chapter) %>%
  summarise(score = sum(score)) %>%
  arrange(desc(score))


# What if we want more granularity? ---------------------------------------

# we can break up our book by apprx page (250-300 words per page)
ps_df %>%
  mutate(
    word_count = 1:n(),
    page = word_count %/% 275 + 1
    )

# we can add onto this to get overall page sentiment
page_sent <- ps_df %>%
  mutate(
    word_count = 1:n(),
    page = word_count %/% 275 + 1
  ) %>%
  inner_join(get_sentiments("bing")) %>%
  count(page, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# and plot
ggplot(page_sent, aes(page, sentiment, fill = sentiment > 0)) +
  geom_col(show.legend = FALSE)

# YOUR TURN!
# Compare the Bing and AFINN sentiment for deathly_hallows.  Do they differ?
dh_df <- tibble(
  chapter = seq_along(deathly_hallows),
  text    = deathly_hallows
) %>%
  unnest_tokens(word, text) %>%
  mutate(
    word_count = 1:n(),
    page = word_count %/% 275 + 1
  ) 

dh_bing <- dh_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(page, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(
    sentiment = positive - negative,
    lexicon = "Bing"
    ) %>%
  select(page, sentiment, lexicon)

dh_afinn <- dh_df %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(page) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(lexicon = "AFINN")

rbind(dh_bing, dh_afinn) %>%
  ggplot(aes(page, sentiment, color = sentiment > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ lexicon, ncol = 1)

rbind(dh_bing, dh_afinn) %>%
  group_by(lexicon) %>%
  filter(sentiment > quantile(sentiment, probs = .995) | sentiment < quantile(sentiment, probs = .005))


