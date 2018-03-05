
# Prerequisites -----------------------------------------------------------

# packages used
library(tidyverse)
library(harrypotter)
library(titanic)


# Regular Expression Functions --------------------------------------------

# we'll use this data throughout
harrypotter::philosophers_stone

# str_* functions
str_detect(philosophers_stone, "Harry")
str_count(philosophers_stone, "Harry")
str_extract(philosophers_stone, "Harry")
str_extract_all(philosophers_stone, "Harry")
str_locate_all(philosophers_stone, "Harry")


# YOUR TURN!
## Take 5 minutes to explore the various str_* functions



# Regular Expression Syntax -----------------------------------------------

# searching for multiple words
str_count(philosophers_stone, "Harry  Potter")
str_count(philosophers_stone, "Harry | Potter")
str_count(philosophers_stone, "ye(s|ah)")
str_count(philosophers_stone, "boy")
str_count(philosophers_stone, regex("boy", ignore_case = TRUE))


# YOUR TURN!
## How many times are "Mr" and "Mrs" used in philosophers_stone?



# anchors
str_count(deathly_hallows, "^Harry")
str_count(philosophers_stone, regex("end$", ignore_case = TRUE))


# YOUR TURN!
## Extract all elements in deathly_hallows that start with "Harry"



# special patterns
str_extract(philosophers_stone, "Harry.")
str_count(philosophers_stone, "Harry")
str_extract(philosophers_stone, "[1|4]")
str_extract(philosophers_stone, ".[yz].")


# YOUR TURN!
## How many times is the word “Harry” get followed by a word that starts with a 
## vowel in philosophers_stone?



# repetition
str_extract(philosophers_stone, "[aeiou]{4}")
str_extract(philosophers_stone, "[aeiou]{3,}")
str_extract(philosophers_stone, "[aeiou]{3,4}")


# YOUR TURN!
## 1. Without computer support, what is this finding: 
## str_count(philosophers_stone, regex("((no[[:punct:]])[ ]){3}", ignore_case = TRUE))


## 2. Extract the 25 characters that precede and follow the use of “Harry" in 
##    philosophers_stone



# Regular Expressions within Data Frames ----------------------------------

# data we'll use
airbnb <- read_rds("data/airbnb.rds")

# combining dplyr with str_*
airbnb %>% 
  select(name) %>%
  mutate(character_count = str_count(name))

airbnb %>% 
  select(name) %>%
  mutate(first_five = str_sub(name, start = 1, end = 5),
         last_five = str_sub(name, start = -5))

airbnb %>% 
  select(host_name) %>%
  mutate(lower_case = str_to_lower(host_name),
         upper_case = str_to_upper(host_name))


# YOUR TURN!
## 1. What is the average number of characters used in the name column?  What 
##    about the description column?


## 2. What is the most common name in the host_name column?



# filtering data frames
airbnb %>%
  select(name) %>%
  filter(str_detect(name, regex(“charming”, ignore_case = TRUE)))


# YOUR TURN!
## 1. Using the house_rules column, how many observations (aka hosts) advocate for 
##    “no shoes”?


## 2. How would you filter out these observations?


# cleaning
airbnb %>%
  select(name) %>%
  count(str_extract(name, "^[^A-Za-z0-9]+"), sort = TRUE)

airbnb %>%
  select(name) %>%
  mutate(
    name = str_replace_all(name, "[^A-Za-z0-9]+", " "),
    name = str_replace_all(name, "[[:punct:]]+", " "),
    name = str_trim(name),
    name = str_to_lower(name)
  ) %>%
  count(str_extract(name, "^[A-Za-z0-9]+"), sort = TRUE)


# Challenge ---------------------------------------------------------------

# In the Kaggle competition for predicting Titanic survivors, the most important 
# predictor variable ended up being the passenger’s title (i.e. Mr., Mrs., Miss., 
# Master). Using the titanic::titanic_train data, extract the passengers title 
# and create a new feature named "Title".

titanic <- titanic::titanic_train %>% as_tibble()


  
