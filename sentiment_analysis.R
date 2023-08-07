# sentiment analysis

################################################################################
# Task 3: Sentiment analysis
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)

## 1. Find any reviews data from Kaggle.com.
### import data
reviews <- read.csv("hotel_reviews.csv", header = T, encoding = "UTF-8")
set.seed(1234)
reviews <- reviews[sample(nrow(reviews), 2500), ]
## write.csv(reviews, "extract_reviews.csv", row.names = F)   ## export data because the raw data cannot upload to UKMFOLIO
## reviews <- read.csv("extract_reviews.csv", header = T)
str(reviews)


### Objective 1: To perform the sentiment scores of the hotel reviews on TripAdvisor using different lexicons, 
###             including syuzhet, bing, AFINN, and nrc.
### 2. Perform the analysis such as obtaining sentiment scores using different lexicon. 
#### sentiment scores
##### syuzhet sentiment scores
text_reviews <- reviews$Review
syuzhet_vector <- get_sentiment(text_reviews, method ="syuzhet")
summary(syuzhet_vector)

##### bing sentiment scores
bing_vector <- get_sentiment(text_reviews, method ="bing")
summary(bing_vector)

##### afinn sentiment scores
afinn_vector <- get_sentiment(text_reviews, method ="afinn")
summary(afinn_vector)

##### nrc sentiment scores
nrc_vector <- get_sentiment(text_reviews, method ="nrc")
summary(nrc_vector)



### Objective 2: To visualise the emotional classification of the hotel reviews on TripAdvisor through barplots.
### 2. Find the most common positive and negative words, perform emotion classification and other related analyses.
#### perform emotion classification
d <- get_nrc_sentiment(text_reviews)

#### visualise
td <- data.frame(t(d))
td_new <- data.frame(rowSums(td[1:2500])) 
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2 <- td_new[1:10,]

##### plot 1
quickplot(sentiment, data = td_new2, weight = count, geom = "bar", 
          fill = sentiment, ylab = "count") + 
  ggtitle("Sentiments") + 
  guides(fill="none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### plot 2
barplot(
  sort(colSums(prop.table(d[, 1:10]))), 
  horiz = T, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)



### Objective 3: To visualise the most common positive and negative words in the hotel reviews on TripAdvisor through barplot and wordcloud.
library(textdata)
library(tidyr)
library(tidytext)
library(stringr)
library(tidyverse)

head(reviews$Review, 3)
reviews$Review <- str_remove_all(reviews$Review, "\\d+")
head(reviews$Review, 3)


#### cleaned data by removing the stopwords, and own-defining words
custome_stopwords <- bind_rows(tibble(word = c("n't", "a.m.", 
                                               "am", "a.m", 
                                               "p.m.", "pm", 
                                               "p.m.", "hotel", 
                                               "stayed", "stay", 
                                               "stays"), 
                                      lexicon = c("custom", "custom", 
                                                  "custom", "custom", 
                                                  "custom", "custom", 
                                                  "custom", "custom", 
                                                  "custom", "custom",
                                                  "custom")), 
                               stop_words)

tidy_reviews <- reviews %>% unnest_tokens(word, Review) %>% anti_join(custome_stopwords)
head(tidy_reviews)

#### Visualise all words in the reviews through wordcloud
library(wordcloud)
options(repr.plot.width = 15, repr.plot.height = 10)
tidy_reviews %>% count(word) %>% with(wordcloud(word, n, max.words = 50, 
                                                random.order = F, 
                                                colors = brewer.pal(8, "Dark2")))



#### Visualise top 15 most positive and negative words through barplot
bing_word_counts <- tidy_reviews %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = T) %>% ungroup 

bing_word_counts %>% group_by(sentiment) %>% 
  slice_max(n, n = 15) %>% 
  ungroup() %>% mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word, fill = sentiment)) + 
  geom_col(show.legend = F) +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment", y = NULL)



#### Visualise top 15 most positive and negative words through wordcloud
library(reshape2)
tidy_reviews %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = T) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("seagreen", "coral"), max.words = 100)



#### Objective 4: To visualise the overall performance of the hotels on TripAdvisor based on the ratings through barplot.
sentiment_rating <- tidy_reviews %>% inner_join(get_sentiments("bing")) %>%
  count(Rating, sentiment) %>% spread(sentiment, n) %>%
  mutate(overall_sentiment = positive - negative)
sentiment_rating

ggplot(sentiment_rating, aes(x = Rating, y = overall_sentiment, fill = as.factor(Rating))) +
  geom_bar(stat = "identity") + 
  labs(title = "Overall sentiment based on rating", 
       x = "rating", y = "overall sentiment", fill = "rating") +
  guides(fill="none")