library(textdata)
library(dplyr)
library(readr)
library(tidytext)
library(sentimentr)
library(MASS)
library(car)
library(lme4)
library(MuMIn)
library(effects)
library(ggplot2)
library(sjPlot)
library(lmerTest)
joe <- read.csv("C:/Users/User/Dropbox/statistik/seminar statistik/archive/hashtag_joebiden.csv")
trump <- read.csv("C:/Users/User/Dropbox/statistik/seminar statistik/archive/hashtag_donaldtrump.csv")

joe1 <- data.frame(joe$tweet, joe$likes, joe$retweet_count,
                   joe$user_followers_count,joe$user_id)
trump1 <- data.frame(trump$tweet, trump$likes, trump$retweet_count,
                     trump$user_followers_count, trump$user_id)
names(joe1) <- c("Tweets", "Likes", "Retweet", "Followers","user_id")
names(trump1) <- c("Tweets", "Likes", "Retweet", "Followers", "user_id")
complete <- bind_rows(joe1, trump1) %>% distinct()
complete.sum <- complete
complete.sum$Likes <- as.numeric(complete.sum$Likes)
complete.sum$Followers <- as.numeric(complete.sum$Followers)
complete.sum <- na.omit(complete.sum)
summary(complete.sum)

complete$Likes <- as.numeric(complete$Likes)
complete1 <- complete[complete$Likes > 0 & complete$Retweet > 0, ]
complete1 <- complete1[complete.cases(complete1$Likes), ]
complete1$Followers <- as.numeric(complete1$Followers)
complete1 <- complete1[complete.cases(complete1$Followers), ]
mytext <- get_sentences(complete1)
complete1.sent <-sentiment_by(mytext)
complete1$sent <- complete1.sent$ave_sentiment


complete1$recretweet <- 1-(complete1$Retweet^-1)
complete1$reclikes <- 1-(complete1$Likes^-1)
complete1$logfollowers<- log(complete1$Followers+1)
complete1$scalelogfollowers<-scale(complete1$logfollowers)
complete1$scloglik <- scale(log(complete1$Likes))
complete1$sclogret<- scale(log(complete1$Retweet))
complete1$pos<-if_else(complete1$sent>0,complete1$sent, 0)
complete1$neg<-if_else(complete1$sent<0,(complete1$sent)*-1, 0)
summary(complete1)
unique(complete1$user_id)

mod.ret=lmerTest::lmer(recretweet~pos+neg+scloglik+scalelogfollowers+(1|user_id), data=complete1)
mod.lik=lmerTest::lmer(reclikes~pos+neg+sclogret+scalelogfollowers+(1|user_id),data=complete1)
eff.ret <- as.data.frame(Effect(c("pos","neg"),mod.ret, xlevels=10))
eff.ret$intensity <- eff.ret$pos-eff.ret$neg
mlm_plot.ret <- ggplot(eff.ret, aes(x = intensity, y = fit,))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(size = 1)+
  labs(x = "Sentiment", y = "Number of Retweets 1-(reciprocal)")+
  ggtitle("Sentiment vs retweet")+
  geom_ribbon(alpha=0.1, aes(ymin=lower,ymax=upper))
mlm_plot.ret
eff.lik <- as.data.frame(Effect(c("pos","neg"),mod.lik, xlevels=10))
eff.lik$intensity <- eff.lik$pos-eff.lik$neg
mlm_plot.lik <- ggplot(eff.lik, aes(x = intensity, y = fit,))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(size = 1)+
  labs(x = "Sentiment", y = "Number of Likes 1-(reciprocal)")+
  ggtitle("Sentiment vs likes")+
  geom_ribbon(alpha=0.1, aes(ymin=lower,ymax=upper))
mlm_plot.lik
r.squaredGLMM(mod.ret)
summary(mod.ret)

mod.ret=lmerTest::lmer(recretweet~pos+neg+scloglik+scalelogfollowers+(1|user_id), data=complete1)
mod.lik=lmerTest::lmer(reclikes~pos+neg+sclogret+scalelogfollowers+(1|user_id),data=complete1)
r.squaredGLMM(mod.lik)
summary(mod.lik)
tab_model(mod.ret, digits=5, digits.re = 5, digits.p=3, emph.p=F, show.se=T)
tab_model(mod.lik, digits=5, digits.re = 5, digits.p=3, emph.p=F, show.se=T)
qqnorm(resid(mod.ret), main = "Q-Q Plot for the retweets model")
qqline(resid(mod.ret), col = "red")

qqnorm(resid(mod.lik), main = "Q-Q Plot for the likes model")
qqline(resid(mod.lik), col = "red")

complete1$loglik <- log(complete1$Likes)
complete1$logret <-log(complete1$Retweet)

ret.lm <-lm(recretweet~pos+neg+loglik+logfollowers, data=complete1)
lik.lm <-lm(reclikes~pos+neg+logret+logfollowers, data=complete1)

lmtest::bptest(ret.lm)
plot(fitted(ret.lm),resid(ret.lm))


mod.retlog <- lmer(recretweet~pos+neg+loglik+logfollowers+(1|user_id), data=complete1)
mod.liklog <- lmer(reclikes~pos+neg+logret+logfollowers+(1|user_id), data=complete1)


par(mfrow = c(1, 3))
qqnorm(resid(lik.lm), main = "Base linear regression model")
qqline(resid(lik.lm), col = "red")
qqnorm(resid(mod.liklog), main = "Linear mixed effects model with log covariables")
qqline(resid(mod.liklog), col = "red")
qqnorm(resid(mod.lik), main = "Linear mixed effects model with scaled covariables")
qqline(resid(mod.lik), col = "red")
par(mfrow = c(1, 1))



par(mfrow = c(1, 3))
qqnorm(resid(ret.lm), main = "Base linear regression model")
qqline(resid(ret.lm), col = "red")
qqnorm(resid(mod.retlog), main = "Linear mixed effects model with log covariables")
qqline(resid(mod.retlog), col = "red")
qqnorm(resid(mod.ret), main = "Linear mixed effects model with scaled covariables")
qqline(resid(mod.ret), col = "red")
par(mfrow = c(1, 1))



AIC(ret.lm, mod.ret, mod.retlog)
AIC(lik.lm, mod.lik, mod.liklog)
