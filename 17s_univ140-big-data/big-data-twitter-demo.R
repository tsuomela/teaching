# R script for big data UNIV 140 Class, Spring 2017

library(twitteR)
source('tokens.R')
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)


# request data from Twitter
delta.tweets = searchTwitter('@delta', n=1500)

length(delta.tweets)
class(delta.tweets)


tweet = delta.tweets[[1]]
class(tweet)
tweet$getScreenName()
tweet$getText()

# apply function across tweets to get the text
delta.text = lapply(delta.tweets, function(t) t$getText())
head(delta.text, 5)

hu.liu.pos <-scan('positive-words.txt', what='character', comment.char=';')
hu.liu.neg <-scan('negative-words.txt', what='character', comment.char=';')

pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait','waiting', 'epicfail', 'mechanical')

delta.scores = score.sentiment(delta.text, pos.words,
                               neg.words, .progress='text')
