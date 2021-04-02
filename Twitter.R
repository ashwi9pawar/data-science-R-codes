library(twitteR)
library(ROAuth)
library(base64enc)
library(httpuv)
library(SnowballC)
library(tm)
library(syuzhet)

cred <- OAuthFactory$new(consumerKey='Idi8AXgk1YVbd8KwaOjEL3IXO', # Consumer Key (API Key)
                         consumerSecret='WdQkKrqJZbfGctHpxsPy16e6ZKXzQIU1oJQrT549382t9noFp2', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")


setup_twitter_oauth("Idi8AXgk1YVbd8KwaOjEL3IXO", # Consumer Key (API Key)
                    "WdQkKrqJZbfGctHpxsPy16e6ZKXzQIU1oJQrT549382t9noFp2", #Consumer Secret (API Secret)
                    "1374348926497615874-8y2iKFNcsk25Ba4e2CjXYUMUIRdSyA",  # Access Token
                    "Yw3XPMzp9pUgsHoJvMEFK266iHvyWFue4G65BotJxbAts")  #Access Token Secret



Tweets <- userTimeline('elonmusk', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "Tweets.csv",row.names = F)

getwd()
 

handleTweets <- searchTwitter('spaceX', n = 10000)
handleTweetsDF <- twListToDF(handleTweets)
dim(handleTweetsDF)
View(handleTweetsDF)
handleTweetsMessages <- unique(handleTweetsDF$text)
handleTweetsMessages <- as.data.frame(handleTweetsMessages)
write.csv(handleTweetsDF, "muskHandleTweets.csv")

#sentimental analysis

head(TweetsDF$text)

#remove hashtags, url etc
tweetsdf2 <- gsub("http.*","",TweetsDF$text)
tweetsdf2 <- gsub("http.*","",tweetsdf2)
tweetsdf2 <- gsub("#.*","",tweetsdf2)
tweetsdf2 <- gsub("@.*","",tweetsdf2)
head(tweetsdf2)

#get the emotion score for each of the tweets.
word.df <- as.vector(tweetsdf2)

emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(tweetsdf2, emotion.df) 
head(emotion.df2)

sent.value <- get_sentiment(word.df)
most.positive <- word.df[sent.value == max(sent.value)]
most.positive

most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative 

#segregating positive and negative tweets
positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)

negative.tweets <- word.df[sent.value < 0] 
head(negative.tweets)

neutral.tweets <- word.df[sent.value == 0] 
head(neutral.tweets)
