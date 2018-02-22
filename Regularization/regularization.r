library(ggplot2)
library(glmnet)
library(tm)
library(boot)

set.seed(1)
x <- seq(-10, 10, by = 0.01)
y <- 1 - x ^ 2 + rnorm(length(x), 0, 5)
ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(se = FALSE)

x.squared <- x ^ 2
ggplot(data.frame(XSquared = x.squared, Y = y), aes(x = XSquared, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

summary(lm(y ~ x))$r.squared
summary(lm(y ~ x.squared))$r.squared

set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)
df <- data.frame(X = x, Y = y)
ggplot(df, aes(x = X, y = Y)) + geom_point()

summary(lm(Y ~ X, data = df))
ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

df <- transform(df, X2 = X ^ 2)
df <- transform(df, X3 = X ^ 3)
summary(lm(Y ~ X + X2 + X3, data = df))
summary(lm(y ~ poly(x, degree = 14), data = df))

poly.fit <- lm(y ~ poly(x, degree = 1), data = df)
df <- transform(df, PredictedY = predict(poly.fit))
ggplot(df, aes(x = X, y = Y)) + geom_point() + geom_line()

set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

n <- length(x)

indices <- sort(sample(1:n, round(0.5 * n)))
training.x <- x[indices]
training.y <- y[indices]

test.x <- x[-indices]
test.y <- y[-indices]

training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

rmse <- function(y, h) {
  return(sqrt(mean((y - h) ^ 2)))
}

performance <- data.frame()

for (d in 1:12) {
  poly.fit <- lm(Y ~ poly(X, degree = d), data = training.df)
  performance <- rbind(performance, 
                       data.frame(Degree = d,
                                   Data = 'Training',
                                   RMSE = rmse(training.y, predict(poly.fit))))
  performance <- rbind(performance, 
                       data.frame(Degree = d,
                                  Data = 'Test',
                                  RMSE = rmse(test.y, predict(poly.fit,
                                                              newdata = test.df))))
}

ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) + 
  geom_point() +
  geom_line()

lm.fit <- lm(y ~ x)
model.complexity <- sum(coef(lm.fit) ^ 2)
l2.model.complexity <- sum(coef(lm.fit) ^ 2)
l1.model.complexity <- sum(abs(coef(lm.fit)))

library(glmnet)

set.seed(1)
x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

#x <- matrix(x) w książce jest błąd
x <- as.matrix(cbind(x, x))
glmnet(x, y)

set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)
n <- length(x)
indices <- sort(sample(1:n, round(0.5 * n)))

training.x <- x[indices]
training.y <- y[indices]

test.x <- x[-indices]
test.y <- y[-indices]

df <- data.frame(X = x, Y = y)
training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

rmse <- function(y, h) {
  return(sqrt(mean((y - h) ^ 2)))
}

glmnet.fit <- with(training.df, glmnet(poly(X, degree = 10), Y))
lambdas <- glmnet.fit$lambda
performance <- data.frame()  

for (lambda in lambdas) {
  performance <- rbind(performance,
                       data.frame(Lambda = lambda,
                                  RMSE = rmse(test.y, with(test.df, predict(glmnet.fit, poly(X, 
                                                                                             degree = 10), s = lambda)))))
}

ggplot(performance, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line()
best.lambda <- with(performance, Lambda[which(RMSE == min(RMSE))])
glmnet.fit <- with(df, glmnet(poly(X, degree = 10), Y))
coef(glmnet.fit, s = best.lambda)

ranks <- read.csv('data/oreilly.csv', stringsAsFactors = FALSE)

documents <- data.frame(doc_id = ranks$IP_Family, text = ranks$Long.Desc.)
row.names(documents) <- 1:nrow(documents)

corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

dtm <- DocumentTermMatrix(corpus)

x <- as.matrix(dtm)
y <- rev(1:100)
set.seed(1)

performance <- data.frame()

for (lambda in c(0.1, 0.25, 0.5, 1, 2, 5)) {
  for (i in 1:50) {
    indices <- sample(1:100, 80)
    training.x <- x[indices, ]
    training.y <- y[indices]
    
    test.x <- x[-indices, ]
    test.y <- y[-indices]
    
    glm.fit <- glmnet(training.x, training.y)
    predicted.y <- predict(glm.fit, test.x, s = lambda)
    rmse <- sqrt(mean((predicted.y - test.y) ^ 2))
    
    performance <- rbind(performance, 
                         data.frame(Lambda = lambda,
                                    Iteration = i,
                                    RMSE = rmse))
  }
}

ggplot(performance, aes(x = Lambda, y = RMSE)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point')

y <- rep(c(1, 0), each = 50)
regularized.fit <- glmnet(x, y, family = 'binomial')
predict(regularized.fit, newx = x, s = 0.001)

ifelse(predict(regularized.fit, newx = x, s = 0.001) > 0, 1, 0)

inv.logit(predict(regularized.fit, newx = x, s = 0.0001))

set.seed(1)
performance <- data.frame()

for (i in 1:250) {
  indices <- sample(1:100, 80)
  training.x <- x[indices, ]
  training.y <- y[indices]
  
  test.x <- x[-indices, ]
  test.y <- y[-indices]
  
  for (lambda in c(0.0001, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.5, 0.1))
  {
    glm.fit <- glmnet(training.x, training.y, family = 'binomial')
    predicted.y <- ifelse(predict(glm.fit, test.x, s = lambda) > 0, 1, 0)
    error.rate <- mean(predicted.y != test.y)
    
    performance <- rbind(performance,
                         data.frame(Lambda = lambda,
                                    Iteration = i,
                                    ErrorRate = error.rate))
  }
}

ggplot(performance, aes(x = Lambda, y = ErrorRate)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point') +
  scale_x_log10()
