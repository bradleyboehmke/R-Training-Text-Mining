
# Prerequisites -----------------------------------------------------------

# packages
library(tidyverse)
library(tidytext)
library(harrypotter)
library(tm)


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


# Word Networks -----------------------------------------------------------

# convert to a document term matrix
ps_dtm <- ps_df %>%
  unnest_tokens(word, text) %>%
  count(chapter, word) %>%
  cast_dtm(chapter, word, n)

tm::findAssocs(ps_dtm, "wand", .9)

# Your Turn!
# find words most correlated with "izzy"
reviews %>%
  select(review_id, comments) %>%
  unnest_tokens(word, comments) %>%
  count(review_id, word) %>%
  cast_dtm(review_id, word, n) %>%
  findAssocs("izzy", .25)

# find words most correlated with "rooftop deck"
reviews %>%
  select(review_id, comments) %>%
  unnest_tokens(word, comments, token = "ngrams", n = 2) %>%
  separate(word, into = c("word1", "word2"), sep = " ") %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word
  ) %>%
  unite(word, word1, word2, sep = " ") %>%
  count(review_id, word) %>%
  cast_dtm(review_id, word, n) %>%
  findAssocs("rooftop deck", .15)

# we can use this information to create a word network

library(qdap)

# filter for shorter reviews
short_reviews <- reviews %>% 
  mutate(words = str_count(comments)) %>% 
  filter(words < 50) %>%
  select(review_id, comments, neighborhood = neighbourhood_cleansed) 

word_network_plot(text.var = short_reviews$comments)

# word networks are primarily useful for small text or text without much 
# word diversity




# Cluster Analysis --------------------------------------------------------

library(factoextra)

# resume files
url <- "https://raw.githubusercontent.com/kwartler/text_mining/master/1yr_plus_final4.csv"
resumes <- read_csv(url)

# create a scaled document term matrix with our resume data
resumes_dtm <- resumes %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "[[:digit:]]")) %>%
  count(num, word) %>%
  cast_dtm(num, word, n) %>%
  scale()

# what does our dtm look like?
dim(resumes_dtm)
resumes_dtm[1:5, 1:5]

# we can measure the "similarity" between each document based on word usage
distance1 <- get_dist(resumes_dtm)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# identify 3 clusters
k3 <- kmeans(resumes_dtm, centers = 3, nstart = 25)
str(k3)
table(k3$cluster)

# is there an optimal number of clusters?
set.seed(123)
fviz_nbclust(resumes_dtm, kmeans, method = "wss")
fviz_nbclust(resumes_dtm, kmeans, method = "silhouette")


# let's look at a different type of clustering - Spherical k-means clustering
library(skmeans)
library(clue)

sk3 <- skmeans(resumes_dtm, 3, m = 1.2, control = list(nruns = 5, verbose = TRUE))
str(sk3)
table(sk3$cluster)
silhouette(sk3) %>% plot()

# let's loop through and adjust the k and m and assess median silhouette
tuning_grid <- expand.grid(
  k = 2:10,
  m = seq(1, 2, by = 0.1),
  silhouette = NA
)

for (i in 1:nrow(tuning_grid)) {
  model <- skmeans(
    resumes_dtm, tuning_grid[i, 1], 
    m = tuning_grid[i, 2], 
    control = list(nruns = 5))
  
  tuning_grid[i, 3] <- median(silhouette(model)[, 3])
}

tuning_grid %>% filter(silhouette == max(silhouette))
sk2 <- skmeans(resumes_dtm, 2, m = 1.2, control = list(nruns = 5, verbose = TRUE))
sk2_results <- t(cl_prototypes(sk2))
sort(sk2_results[, 1], decreasing = TRUE)[1:10]
sort(sk2_results[, 2], decreasing = TRUE)[1:10]

# Your Turn - Part 1!
# can you import and combine, the 10 articles for the 10 authors in the
# data/news_articles folder?
files <- list.files(path = "data/news_articles") %>% 
  map_chr(~ paste0("data/news_articles/", ., "/")) %>%
  map(~ paste0(., list.files(path = ., pattern = "\\.txt"))) %>%
  unlist()

articles <- tibble()

for(i in seq_along(files)) {
  # import article
  text <- read_file(files[i]) %>%
    as_tibble() %>%
    mutate(id = i) %>%
    select(id, text = value)
  
  # combine
  articles <- rbind(articles, text)
}

# Your Turn - Part 2!
# can you now tidy this data set and prepare for cluster analysis?

articles_dtm <- articles %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(id, word) %>%
  filter(
    !str_detect(word, "[[:digit:]]"),
    n > 1
    ) %>%
  cast_dtm(id, word, n) %>%
  scale()

dim(articles_dtm)
articles_dtm[1:5, 1:5]

# Your Turn - Part 3!
# Choose any of the cluster analysis approaches and apply. How many clusters
# do you find?
tuning_grid <- expand.grid(
  k = 2:9,
  m = seq(1, 2, by = 0.1),
  silhouette = NA
)

for (i in 1:nrow(tuning_grid)) {
  model <- skmeans(
    articles_dtm, tuning_grid[i, 1], 
    m = tuning_grid[i, 2], 
    control = list(nruns = 5))
  
  tuning_grid[i, 3] <- median(silhouette(model)[, 3])
}

tuning_grid %>% filter(silhouette == max(silhouette))
sk8 <- skmeans(articles_dtm, 8, m = 1.1, control = list(nruns = 5, verbose = TRUE))
sk8_df <- t(cl_prototypes(sk8))

sk8_df %>%
  as_tibble() %>%
  mutate(word = row.names(sk8_df)) %>%
  gather(k, silhouette, -word) %>%
  group_by(k) %>%
  top_n(10) %>%
  ggplot(aes(silhouette, reorder(word, silhouette))) +
  geom_point() +
  facet_wrap(~ k, scales = "free_y")



# Topic Modeling ----------------------------------------------------------

# package for topic modeling
library(topicmodels)

# What are the main topics or themes in the Harry Potter Series?

# combine all books
titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

series <- tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(
    chapter = seq_along(books[[i]]),
    text    = books[[i]]
    ) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}

series$book <- factor(series$book, levels = rev(titles))
series


# first we turn into a document term matrix
df_dtm <- series %>%
  anti_join(stop_words) %>%
  unite(document, book, chapter) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

# LDA on satisfaction categories
levels_lda <- LDA(df_dtm, k = 7, control = list(seed = 1234))
levels_lda

# get per-topic-per-word probabilities
levels_topics <- tidy(levels_lda, matrix = "beta")
levels_topics %>% arrange(desc(beta))

# top 10 terms within each topic
levels_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 2) +
  coord_flip()

# per-document-per-topic probabilities  
levels_gamma <- tidy(levels_lda, matrix = "gamma")
levels_gamma

levels_gamma %>%
  separate(document, into = c("book", "chapter"), sep = "_") %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ book, ncol = 2)

# DO NOT RUN IN CLASS ----> takes about 15 min
# find optimal number of topics
# install.packages("ldatuning")
library(ldatuning)

result <- FindTopicsNumber(
  df_dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)


# Your Turn - Part 1!
# can you import and combine, the 10 articles for the 10 authors in the
# data/news_articles folder?
files <- list.files(path = "data/news_articles") %>% 
  map_chr(~ paste0("data/news_articles/", ., "/")) %>%
  map(~ paste0(., list.files(path = ., pattern = "\\.txt"))) %>%
  unlist()

articles <- tibble()

for(i in seq_along(files)) {
  # import article
  text <- read_file(files[i]) %>%
    as_tibble() %>%
    mutate(id = i) %>%
    select(id, text = value)
  
  # combine
  articles <- rbind(articles, text)
}

# Your Turn - Part 2!
# can you now tidy this data set and prepare for topic modeling?

articles_dtm <- articles %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(id, word) %>%
  filter(
    !str_detect(word, "[[:digit:]]"),
    n > 1
  ) %>%
  cast_dtm(id, word, n)

# Your Turn - Part 3!
# Perform topic modeling.

## Step 1: identify number of topics
result <- FindTopicsNumber(
  articles_dtm,
  topics = seq(from = 2, to = 50, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

## Step 2: assess 24 topics
articles_lda <- LDA(articles_dtm, k = 24, control = list(seed = 1234))

## Step 3: assess per-topic-per-word probabilities
### get per-topic-per-word probabilities
articles_lda %>% 
  tidy(matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 4) +
  coord_flip()

### per-document-per-topic probabilities  
### visual one
articles_lda %>%
  tidy(matrix = "gamma") %>%
  group_by(document) %>%
  top_n(1, wt = gamma) %>%
  ggplot(aes(reorder(document, topic), topic)) +
  geom_point()

### visual two: illustrates potential topic misallocation
rank <- articles_lda %>%
  tidy(matrix = "gamma") %>%
  group_by(document) %>%
  top_n(1, wt = gamma) %>%
  ungroup() %>%
  mutate(rank = 1:n()) %>%
  select(document, rank)

articles_lda %>%
  tidy(matrix = "gamma") %>%
  left_join(rank) %>%
  ggplot(aes(reorder(document, rank), topic, alpha = gamma)) +
  geom_point()



