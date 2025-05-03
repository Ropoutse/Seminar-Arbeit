library(textdata)
library(dplyr)
library(readr)
library(tidytext)
library(sentimentr)
library(MASS)
library(car)

joe <- read.csv("C:/Users/User/Dropbox/statistik/seminar statistik/archive/hashtag_joebiden.csv")
trump <- read.csv("C:/Users/User/Dropbox/statistik/seminar statistik/archive/hashtag_donaldtrump.csv")
joe1 <- data.frame(joe$tweet, joe$likes, joe$retweet_count,joe$user_followers_count)
trump1 <-  data.frame(trump$tweet, trump$likes, trump$retweet_count, trump$user_followers_count)
names(joe1) <- c("Tweets", "Likes", "Retweet", "Followers")
names(trump1) <- c("Tweets", "Likes", "Retweet", "Followers")

complete <- bind_rows(joe1, trump1) %>% distinct()

### Data cleaning
complete1 <- complete[complete$Likes != "0.0" | complete$Retweet != 0, ]
complete1$Likes <- as.numeric(complete1$Likes)
complete1 <- complete1[complete.cases(complete1$Likes), ]
complete1$Followers <- as.numeric(complete1$Followers)
complete1 <- complete1[complete.cases(complete1$Followers), ]

rm(trump)
rm(joe)
rm(complete)

summary(complete1)

Y.LR <- complete1[complete1$Likes >1 &complete1$Retweet !=0,]

start_time <- Sys.time()
mytext <- get_sentences(Y.LR)
Y.LR.sent <-sentiment_by(mytext)
end_time <- Sys.time()
end_time - start_time
summary(Y.LR.sent)

Y.LR$SqrtFollowers <- sqrt(Y.LR$Followers)
Y.LR$sent <- Y.LR.sent$ave_sentiment
Y.LRb<-Y.LR[-1509,]

Y.LR$loglikes<-log(Y.LR$Likes)
Y.LR$logretweet <- log(Y.LR$Retweet)
Y.LR$logfollowers<- log(Y.LR$Followers)



Yl  <- Y.LRb$Likes
Xl <- data.matrix(data.frame(Y.LRb$Retweet,Y.LRb$SqrtFollowers, Y.LRb$sent))
modelllg <- glm(Yl~Xl, family = "poisson")
summary(modelllg)
par(mfrow=c(2,2))
plot(modelllg)
par(mfrow=c(1,1))

Yl1 <- Y.LR$loglikes
Xl1 <- data.matrix(data.frame(Y.LR$Retweet,Y.LR$SqrtFollowers, Y.LR$sent))
modelllg1 <- lm(Yl1~Xl1)
summary(modelllg1)
par(mfrow=c(2,2))
plot(modelllg1)
par(mfrow=c(1,1))
durbinWatsonTest(modelllg1)

Yr <- Y.LR$Retweet
Xr <- data.matrix(data.frame(Y.LR$Likes,Y.LR$Followers, Y.LR.sent$ave_sentiment))
modellr <- lm(Yr~Xr)
summary(modellr)
par(mfrow=c(2,2))
plot(modellr)
par(mfrow=c(1,1))

library(MASS)

Ylb <- Y.LRb$Likes
boxcox_mod <- boxcox(Ylb ~ Y.LRb$Retweet + Y.LRb$SqrtFollowers + Y.LRb$sent, data = Y.LRb)
best_lambda <- boxcox_mod$x[which.max(boxcox_mod$y)]

Ylbo <- ((Ylb^best_lambda-1)/best_lambda)
Xl <- data.matrix(data.frame(Y.LRb$Retweet,Y.LRb$SqrtFollowers, Y.LRb$sent))
modelllbo <- lm(Ylbo~Xl)
summary(modelllbo)
par(mfrow=c(2,2))
plot(modelllbo)
par(mfrow=c(1,1))



sort(Yr, decreasing = T)
sort(Yl, decreasing = T)
sort(Y.LR$Followers, decreasing = T)

Y.LR.b <- Y.LR[Y.LR$Likes < 50000 & Y.LR$Retweet < 10000,]
which(Y.LR$Likes > 50000 | Y.LR$Retweet > 10000,)



Yl.b <- Y.LR.b$Likes
Xl.b <- data.matrix(data.frame(Y.LR.b$Retweet,Y.LR.b$Followers, Y.LR.sent.b$ave_sentiment))
modelll.b <- lm(Yl.b~Xl.b)
summary(modelll.b)
par(mfrow=c(2,2))
plot(modelll)
par(mfrow=c(1,1))


Yr.b <- Y.LR.b$Retweet
Xr.b <- data.matrix(data.frame(Y.LR.b$Likes,Y.LR.b$Followers, Y.LR.sent.b$ave_sentiment))
modellr.b <- lm(Yr.b~Xr.b)
summary(modellr.b)
par(mfrow=c(2,2))
plot(modellr.b)
par(mfrow=c(1,1))


Yl.b <- Y.LR.b$Likes
Xl.b <- data.matrix(data.frame(Y.LR.b$Retweet,Y.LR.b$SqrtFollowers, Y.LR.b$sent))
modelllg.b <- glm(Yl.b~Xl.b, family = "poisson")
summary(modelllg.b)
par(mfrow=c(2,2))
plot(modelllg.b)
par(mfrow=c(1,1))


which(Y.LR$Likes=="1509")
