
# Prerequisites -----------------------------------------------------------

# packages used
library(nycflights13)
library(tidyverse)

# data used
flights
mpg


# Data manipulation & transformation with dplyr ---------------------------

# filtering data
filter(flights, month == 1)
filter(flights, month == 1, day == 1)

# your turn



# selecting variables
select(flights, year, month, day)
select(flights, year:day)
select(flights, ends_with("time"))

# your turn



# arranging your data
arrange(flights, dep_delay)
arrange(flights, desc(dep_delay))

# your turn



# create new variables
flights_sml <- select(
  flights,
  year:day,
  ends_with("delay"),
  distance, 
  air_time
)

mutate(
  flights_sml,
  gain = arr_delay - dep_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours
)

# your turn



# summarizing your data
summarize(flights, dep_delay_mean = mean(dep_delay, na.rm = TRUE))
summarize(
  flights,
  dep_delay_mean = mean(dep_delay, na.rm = TRUE),
  dep_delay_sd = sd(dep_delay, na.rm = TRUE),
  n = n())

# summarize grouped data
by_day <- group_by(flights, month)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

# your turn



# streamlining your analysis
customer <- read_csv("data/CustomerData.csv")
customer %>%
  filter(!is.na(Gender)) %>%
  group_by(Gender, Region) %>%
  summarize(Avg_spend = mean(CardSpendMonth, na.rm = TRUE)) %>%
  arrange(desc(Avg_spend))

# your turn




# ggplot for visualization ------------------------------------------------

# univariate geoms
ggplot(data = mpg, aes(x = hwy)) +
  geom_histogram()

ggplot(data = mpg, aes(x = hwy)) +
  geom_freqpoly()

ggplot(data = mpg, aes(x = hwy)) +
  geom_density()

ggplot(data = mpg, aes(x = class)) +
  geom_bar()

# bivariate geoms
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point()

ggplot(data = mpg, aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg, aes(x = class, y = hwy)) +
  geom_violin()

# your turn



# adding more attributes and dimensions
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue")

ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

ggplot(data = txhousing, aes(x = volume, y = median)) + 
  geom_point(alpha = .25) +
  scale_y_continuous(name = "Median Sales Price", labels = scales::dollar) +
  scale_x_log10(name = "Total Sales Volume", labels = scales::comma) +
  ggtitle("Texas Housing Sales",
          subtitle = "Sales data from 2000-2010 provided by the TAMU real estate center")

ggplot(data = txhousing, aes(x = volume, y = median)) + 
  geom_point(alpha = .25)  +
  scale_x_log10() +
  geom_smooth(method = "lm") +
  facet_wrap(~ month)
