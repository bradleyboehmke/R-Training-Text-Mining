
# Prerequisites -----------------------------------------------------------

# package prereqs
library(tidyverse)
library(tidytext)
library(magrittr)

# data prereq
airbnb <- read_rds("data/airbnb.rds")


# Task 1 ------------------------------------------------------------------

# 1 & 2: unnest into single words, remove stop words, identify and visualize the 
#        most commonly used words to describe the markets
task1_df <- airbnb %>%
  select(property_type, id, description) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) %>%
  filter(
    property_type %in% c("Apartment", "House"),
    str_detect(word, "[[:alpha:]]")
    ) %>%
  count(property_type, word, sort = TRUE) %>%
  group_by(property_type) %>%
  mutate(pct = n / sum(n))

# 3: visualize the top 10 most common words
task1_df %>%
  top_n(10) %>%
  ungroup() %>%
  arrange(pct) %>%
  mutate(rank = 1:n()) %>%
  ggplot(aes(reorder(word, rank), pct, fill = property_type)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(NULL, labels = scales::percent) +
  coord_flip() +
  facet_wrap(~ property_type, scales = "free_y")

# 4: visualize and calculate similarity in word use
task1_df %>%
  select(-n) %>%
  spread(property_type, pct) %>%
  na.omit() %>%
  mutate(delta = abs(Apartment - House)) %>%
  ggplot(aes(Apartment, House)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = .15, size = 2.5, width = .1, height = .1) +
  scale_x_log10(labels = scales::percent) +
  scale_y_log10(labels = scales::percent)
  
# compute the correlation in word use
task1_df %>%
  select(-n) %>%
  spread(property_type, pct) %>%
  na.omit() %$%
  cor.test(Apartment, House)

# visualize the most common words between the two
task1_df %>%
  select(-n) %>%
  spread(property_type, pct) %>%
  na.omit() %>%
  mutate(delta = abs(Apartment - House)) %>%
  top_n(-100, wt = delta) %>%
  ggplot(aes(Apartment, House)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_point(alpha = .15, size = 2.5) +
  geom_text(aes(label = word), position = position_jitter(width = .3, height = .3)) +
  scale_x_log10(labels = scales::percent) +
  scale_y_log10(labels = scales::percent)


# visualize the 100 words that are used by both property types but are most distinct
# to each property type
task1_df %>%
  select(-n) %>%
  spread(property_type, pct) %>%
  na.omit() %>%
  mutate(delta = abs(Apartment - House)) %>%
  top_n(100, wt = delta) %>%
  ggplot(aes(Apartment, House)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_point(alpha = .15, size = 2.5) +
  geom_text(aes(label = word)) +
  scale_x_log10(labels = scales::percent) +
  scale_y_log10(labels = scales::percent)



# Task 2 ------------------------------------------------------------------

# most commonly provided amenities in the Boston market

airbnb %>%
  filter(market == "Boston") %>%
  select(id, amenities) %>%
  unnest_tokens(word, amenities) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

airbnb %>%
  select(amenities) %>%
  filter(str_detect(amenities, "Detector"))

airbnb %>%
  filter(market == "Boston") %>%
  select(id, amenities) %>%
  mutate(amenities = str_replace_all(amenities, " ", "")) %>%
  unnest_tokens(word, amenities) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

# how many properties are kid friendly
kid_df <- airbnb %>%
  select(price, amenities) %>%
  mutate(kids = str_detect(amenities, regex("kid friendly", ignore_case = TRUE)))

kid_df %>%
  count(kids) %>%
  mutate(pct = n / sum(n))
  
# price difference between properties that are kid friendly
kid_price <- kid_df %>%
  mutate(
    price = str_replace_all(price, "(\\$)|(,)|(\\..*)", ""),
    price = str_trim(price) %>% as.numeric()
    )

p1 <- ggplot(kid_price, aes(price, kids)) +
  ggridges::geom_density_ridges(alpha = .5) +
  scale_x_log10(labels = scales::dollar)

p2 <- ggplot(kid_price, aes(kids, price)) +
  geom_boxplot() +
  scale_y_log10(labels = scales::dollar) +
  coord_flip()

gridExtra::grid.arrange(p1, p2, ncol = 1)

t.test(log(price) ~ kids, data = kid_price)


