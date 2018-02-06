
# Prerequisites -----------------------------------------------------------

# packages used
library(tidyverse)

# data used
airbnb <- read_rds("data/airbnb.rds")




airbnb %>%
  select(name) %>%
  filter(str_detect(name, ignore.case("charming")))

airbnb %>%
  select(name) %>%
  filter(str_detect(name, "(C|c)harming|(C|c)ute"))

airbnb %>%
  select(name) %>%
  filter(str_detect(name, regex("charming|cute", ignore_case = TRUE)))


# Cleaning ----------------------------------------------------------------

# we want to know what the most common first word is
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

titanic <- titanic::titanic_train %>% as_tibble()

titanic %>%
  mutate(Title = str_replace_all(Name, "(.*, )|(\\..*)", ""))
  
