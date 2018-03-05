
# Prerequisites -----------------------------------------------------------

library(tidyverse)
library(tidytext)
library(harrypotter)


# Creating structure ------------------------------------------------------

# some text we want to analyze
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text

# organizing into a tibble
tibble(
  index = seq_along(text),
  token = text
)

# philosopher's stone is just a bigger text document
philosophers_stone

# but we can still organize into a tibble the same way
text_tb <- tibble(
  chapter = seq_along(philosophers_stone),
  text    = philosophers_stone
)
text_tb

# YOUR TURN!
# 1. Organize harrypotter::deathly_hallows into a tibble
# 2. Organize harrypotter::chamber_of_secrets into a tibble
# 3. Organize harrypotter::goblet_of_fire into a tibble
tibble(
  chapter = seq_along(deathly_hallows),
  text    = deathly_hallows
)

# lets remove everything in our environment
rm(list = ls())

# we often have multiple documents we want to store together
df1 <- philosophers_stone
df2 <- chamber_of_secrets
df3 <- prisoner_of_azkaban
df4 <- goblet_of_fire
df5 <- order_of_the_phoenix
df6 <- half_blood_prince
df7 <- deathly_hallows

# list all objects with "df" in their name
ls(pattern = "df")

# get all objects with "df" in their name and hold them as a list
books <- mget(ls(pattern = "df"))

titles <- c(
  "Philosopher's Stone", "Chamber of Secrets", 
  "Prisoner of Azkaban", "Goblet of Fire", 
  "Order of the Phoenix", "Half-Blood Prince",
  "Deathly Hallows"
  )

series <- tibble()

for(i in seq_along(books)) {
  org <- tibble(
    book    = titles[i],
    chapter = seq_along(books[[i]]),
    text    = books[[i]]
  )
  
  series <- rbind(series, org)
}

series

# YOUR TURN!
files <- list.files(path = "data", pattern = "\\.txt")
all_3 <- tibble()

for(i in seq_along(files)) {
  
  name <- files[i]
  path <- paste0("data/", name)
  data <- read_tsv(path, col_names = FALSE) %>%
    mutate(file = name) %>%
    select(file, text = X1)
  all_3 <- rbind(all_3, data)
  
}

all_3
  

# Unnesting ---------------------------------------------------------------

# let's go back and organize our philosopher's stone text
text_tb <- tibble(
  chapter = seq_along(philosophers_stone),
  text    = philosophers_stone
)

# unnesting our text
text_tb %>% unnest_tokens(word, text)
text_tb %>% unnest_tokens(word, text, token = "sentences")
text_tb %>% unnest_tokens(word, text, token = "lines")
text_tb %>% unnest_tokens(word, text, token = "ngrams", n = 2)

# YOUR TURN!
# Unnest the deathly_hallows text into single words and bi-grams



# Stop Words --------------------------------------------------------------

# tidytext provides stop words from three lexicons
stop_words

# frequency analysis shows that stop words dominate
text_tb %>% 
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE)

# we can use anti_join to remove all stop words in our data set 
text_tb %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

# YOUR TURN!
# Unnest the deathly_hallows book and remove the stop words.


# Frequency Analysis ------------------------------------------------------

# let's get the top 10 contextual words in philosopher's stone
text_tb %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  top_n(10)

# we can plot these
text_tb %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col() +
  coord_flip()

# what about the top 10 bi-grams?
text_tb %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

# remove bi-grams with stop words
text_tb %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word
  ) %>% 
  count(word1, word2, sort = TRUE)

# if we want to visualize bi-grams
text_tb %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word
    ) %>% 
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  top_n(10) %>%
  ggplot(aes(reorder(bigram, n), n)) +
  geom_col() +
  coord_flip()

# YOUR TURN!
# Find the most common tri-grams in deathly_hallows
tibble(
  chapter = seq_along(deathly_hallows),
  text    = deathly_hallows
) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word,
    !word3 %in% stop_words$word
  ) %>%
  count(word1, word2, word3, sort = TRUE) %>%
  unite(trigram, word1:word3, sep = " ") %>%
  top_n(10) %>%
  ggplot(aes(reorder(trigram, n), n)) +
  geom_col() +
  coord_flip()



# Relationships -----------------------------------------------------------

# what about comparing across the entire series
series %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  count(book, word, sort = TRUE)

# problem is, we have many different sized documents
map(books, str_count) %>% map_dbl(sum)

# comparing pairwise correlation within a document
ph_cor <- tibble(
  title   = "Philosopher's Stone",
  chapter = seq_along(philosophers_stone),
  text    = philosophers_stone
) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(n() >= 50) %>%
  widyr::pairwise_cor(word, chapter, sort = TRUE)

filter(ph_cor, item1 == "harry")

# comparing across two documents
df1 <- tibble(
  title   = "Philosopher's Stone",
  chapter = seq_along(philosophers_stone),
  text    = philosophers_stone
) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

df2 <- tibble(
  title   = "Deathly Hallows",
  chapter = seq_along(deathly_hallows),
  text    = deathly_hallows
) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

combined <- df1 %>%
  rbind(df2) %>%
  filter(str_detect(word, "[a-z']+")) %>%
  count(title, word) %>%
  group_by(title) %>%
  mutate(pct = n / sum(n)) %>%
  select(-n) %>%
  spread(title, pct) %>%
  na.omit() %>%
  mutate(delta = abs(`Deathly Hallows` - `Philosopher's Stone`))
  
# we can plot the output
ggplot(combined, aes(x = `Philosopher's Stone`, y = `Deathly Hallows`, color = delta)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.2, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE) +
  scale_x_log10("Philosopher's Stone", labels = scales::percent) +
  scale_y_log10("Deathly Hallows", labels = scales::percent) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position = "none")

# or compute correlation
cor.test(combined$`Deathly Hallows`, combined$`Philosopher's Stone`)

# sometimes proportions are better
# 1. clean
clean_tokens <- series %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# 2. compute percent of word use across all novels
series_pct <- clean_tokens %>%
  count(word, sort = TRUE) %>%
  transmute(word, all_words = n / sum(n))

# 3. compute percent of word use within each novel
comp_prop <- clean_tokens %>%
  count(book, word, sort = TRUE) %>%
  group_by(book) %>%
  mutate(book_words = n / sum(n)) %>%
  inner_join(series_pct) %>%
  ungroup()

# 4. plot
comp_prop %>%
  mutate(
    delta = abs(all_words - book_words),
    book = factor(book, levels = titles)
    ) %>%
  group_by(book) %>%
  top_n(100, wt = delta) %>%
  ggplot(aes(x = all_words, y = book_words, color = delta)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE) +
  scale_x_log10("Proportion of series", labels = scales::percent) +
  scale_y_log10("Proportion of book", labels = scales::percent) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~ book, ncol = 2) +
  theme(legend.position = "none")


# we can also use this information to assess the correlation between the books
comp_prop %>%
  group_by(book) %>%
  summarise(rho = cor(book_words, all_words)) %>%
  arrange(desc(rho))




