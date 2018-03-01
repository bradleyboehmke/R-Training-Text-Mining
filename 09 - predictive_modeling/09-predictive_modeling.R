
# Prerequisites -----------------------------------------------------------

# packages
library(tidyverse)
library(tidytext)
library(glmnet)
library(pROC)

# data
url <- "https://raw.githubusercontent.com/kwartler/text_mining/master/all_3k_headlines.csv"
headlines <- read_csv(url)


# Prepare data ------------------------------------------------------------

# assign about 70% to training and 30% to test
set.seed(123)

headlines <- headlines %>%
  mutate(partition = sample(c("Train", "Test"), 3000, prob = c(.7, .3), replace = TRUE))

# clean and prepare document term matrix
headlines_tidy <- headlines %>%
  unnest_tokens(word, headline) %>%
  anti_join(stop_words) %>%
  count(url, partition, y, word) %>%
  mutate(id = paste(url, partition, sep = "_"))

# convert to a DTM matrix
headlines_matrix <-  headlines_tidy %>% 
  cast_dtm(id, word, n) %>%
  as.matrix()

# training split
train.x <- headlines_matrix %>%
  as_data_frame() %>%
  mutate(id = row.names(headlines_matrix)) %>%
  filter(str_detect(id, "_Train")) %>%
  select(-id) %>%
  as.matrix() %>%
  Matrix::Matrix(sparse = TRUE)

train.y <- headlines_tidy %>%
  filter(partition == "Train") %>%
  distinct(id, y) %>%
  .$y

dim(train.x)
train.x[1:5, 1:5]
table(train.y)

# YOUR TURN!
## ou created the training split, now can you do the same thing to create the 
## test split?
test.x <- headlines_matrix %>%
  as_data_frame() %>%
  mutate(id = row.names(headlines_matrix)) %>%
  filter(str_detect(id, "_Test")) %>%
  select(-id) %>%
  as.matrix() %>%
  Matrix::Matrix(sparse = TRUE)

test.y <- headlines_tidy %>% 
  filter(partition == "Test") %>%
  distinct(id, y) %>%
  .$y

dim(test.x)
test.x[1:5, 1:5]
table(test.y)

# Modeling ----------------------------------------------------------------

# apply Lasso model
set.seed(123)

cv.lasso <- cv.glmnet(
  x = train.x,
  y = as.factor(train.y),
  alpha = 1,
  family = "binomial",
  nfolds = 10,
  intercept = FALSE,
  type.measure = "class"
)

# plot misclassification error
plot(cv.lasso)

# predict lasso
pred.lasso <- predict(cv.lasso, train.x, type = "class", s = cv.lasso$lambda.1se)

# area under the curve
auc.lasso <- roc(train.y, as.numeric(pred.lasso))
auc.lasso
plot(auc.lasso)

# confusion matrix
confusion <- table(pred.lasso, train.y)
confusion

## overall accuracy
sum(diag(confusion)) / sum(confusion)


# YOUR TURN!
# apply Ridge model
set.seed(123)

cv.ridge <- cv.glmnet(
  x = train.x,
  y = as.factor(train.y),
  alpha = 0,
  family = "binomial",
  nfolds = 10,
  intercept = FALSE,
  type.measure = "class"
)

# plot misclassification error
plot(cv.ridge)

# predict ridge
pred.ridge <- predict(cv.ridge, train.x, type = "class", s = cv.ridge$lambda.1se)

# area under the curve
auc.ridge <- roc(train.y, as.numeric(pred.ridge))
auc.ridge
plot(auc.ridge)

# confusion matrix
confusion <- table(pred.ridge, train.y)
confusion

## overall accuracy
sum(diag(confusion)) / sum(confusion)

# Let's assess how an elastic net compares
tuning <- expand.grid(
  alpha    = seq(0, 1, by = .01),
  accuracy = NA
)

for(i in seq_along(tuning$alpha)) {
  
  set.seed(123)
  
  cv <- cv.glmnet(
    x = train.x,
    y = as.factor(train.y),
    alpha = tuning$alpha[i],
    family = "binomial",
    nfolds = 10,
    intercept = FALSE,
    type.measure = "class"
  )
  
  pred <- predict(cv, train.x, type = "class", s = cv$lambda.1se)
  confusion <- table(pred, train.y)
  tuning$accuracy[i] <- sum(diag(confusion)) / sum(confusion)
}

ggplot(tuning, aes(alpha, accuracy)) +
  geom_line() +
  ylim(c(0, 1))

tuning %>% filter(lambda == .5)

# let's predict on the test set
cv <- cv.glmnet(
  x = train.x,
  y = as.factor(train.y),
  alpha = .5,
  family = "binomial",
  nfolds = 10,
  intercept = FALSE,
  type.measure = "class"
)

pred.test <- predict(cv, test.x, type = "class", s = cv$lambda.min)

# area under the curve
auc <- roc(test.y, as.numeric(pred.test))
auc
plot(auc)

# confusion matrix
confusion <- table(pred.test, test.y)
confusion

## overall accuracy
sum(diag(confusion)) / sum(confusion)

plot(auc.lasso, main = "Black = Train, Red = Test")
plot(auc, add = TRUE, col = "red", lty = 2)

# finding the most impactful words
glmnet.coef <- as.matrix(coef(cv, s = "lambda.min"))
glmnet.coef <- tibble(
  words = row.names(glmnet.coef),
  coeff = glmnet.coef[, 1]
) %>%
  arrange(desc(coeff)) %>%
  mutate(words = fct_inorder(words))

summary(glmnet.coef$coeff)
ggplot(glmnet.coef, aes(coeff)) +
  geom_density()

# top 10 positive and negative words
rbind(
  top_n(glmnet.coef, 10),
  top_n(glmnet.coef, -10)
) %>%
  ggplot(aes(x = coeff, y = words)) +
  geom_segment(aes(yend = words, xend = 0)) +
  geom_point(aes(color = coeff > 0), size = 2)

# convert to probabilities
glmnet_coef <- glmnet.coef %>%
  mutate(
    probability = arm::invlogit(coeff),
    word_id     = row_number(words)
    )

ggplot(glmnet_coef, aes(probability, word_id)) +
  geom_point(size = .5)

