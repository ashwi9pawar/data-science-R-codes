
library(rvest)
library(XML)
library(magrittr)

# Amazon Reviews extracting

aurl <- "https://www.amazon.in/Apple-MacBook-Air-13-3-inch-Integrated/product-reviews/B073Q5R6VR/ref=cm_cr_arp_d_paging_btm_3?showViewpoints=1&pageNumber"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"apple.txt",row.names = F)

#Emotion mining

library(syuzhet)
library(lubridate)
library(gplot2)
library(scales)
library(dplyr)
library(reshape2)

txt = readLines("C:/Users/Admin/Documents/apple.txt")
txt <- iconv(txt, "UTF-8")

x <- get_nrc_sentiment(txt)
head(x,n=50)

get_nrc_sentiment('happy')
get_nrc_sentiment('boring')

get_sentiment('boring', method = "afinn")
get_sentiment('happy', method = "afinn")

example <- get_sentences(txt)
nrc_data <- get_nrc_sentiment(example)
nrc_data

#bar plot for emotion mining
windows()
barplot(colSums(nrc_data), las = 1, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')


sentiment_vector <- get_sentiment(example, method = "bing")
sentiment_afinn <- get_sentiment(example, method = "afinn")
sentiment_nrc <- get_sentiment(example, method = "nrc")

sum(sentiment_afinn)
mean(sentiment_afinn)
summary(sentiment_afinn)

windows()
plot(sentiment_vector, type='l', main = 'Plot trajectory', xlab = 'Narative time')
abline(h=0, color='red')

plot(sentiment_vector,type='h',main="Example Plot trajectory")

#Shape smoothing and normalisation using fourier based transform
ft_values <- get_transformed_values(sentiment_vector, low_pass_size = 3, 
                                    x_reverse_len = 100, padding_factor = 2,
                                    scale_vals = TRUE, scale_range = FALSE)

plot(ft_values, type="l", main = "reviewsusing transformed values",
     xlab="Narrative Time", ylab="Emotional Valence", col="red")

#most negative and positive reviews
negative <- example[which.min(sentiment_vector)]
negative

positive <- example[which.max(sentiment_vector)]
positive
