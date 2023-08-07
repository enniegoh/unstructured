# Data Mining
# Lyrics analysis

# Import dataset
song <- read.csv("lyrics.csv", header = T, strip.white = T)
head(song)
str(song)



# Total of artist
artist_name <- unique(song$artist)
artist_name



# Total number of albums and tracks
## Albums
albums_name <- unique(song$album)
total_albums <- length(albums_name)

## Tracks
tracks_name <- unique(song$track_title)
total_tracks <- length(tracks_name)

# Years
years <- unique(song$year)

# Summarise
total_list <- list(years = years, albums_name = albums_name, total_albums = total_albums, tracks_name = tracks_name, 
                   total_tracks = total_tracks)
total_list



# Data cleaning
## Convert lyrics to corpus text
library(tm)
library(SnowballC)
lyrics_corpus <- Corpus(VectorSource(song$lyric))
class(lyrics_corpus)
inspect(lyrics_corpus)


## define white space
white_space <- content_transformer(function(x, pattern) gsub(pattern, "", x))
white_space



## remove punctuation
lyrics_corpus <- tm_map(lyrics_corpus, removePunctuation)
inspect(lyrics_corpus)

## remove special characters
lyrics_corpus <- tm_map(lyrics_corpus, white_space, "\u0005")
lyrics_corpus <- tm_map(lyrics_corpus, white_space, "\u0011")
lyrics_corpus <- tm_map(lyrics_corpus, white_space, "\u0017")


## remove extra unnecessary spaces
lyrics_corpus <- tm_map(lyrics_corpus, stripWhitespace)
inspect(lyrics_corpus)



## Convert the lyrics to all lowercase
lyrics_corpus <- tm_map(lyrics_corpus, content_transformer(tolower))   ### Sys.setlocale("LC_ALL", "C") to run
inspect(lyrics_corpus)



# Remove numbers
lyrics_corpus <- tm_map(lyrics_corpus, removeNumbers)
inspect(lyrics_corpus)



# Remove stopwords
lyrics_corpus <- tm_map(lyrics_corpus, removeWords, stopwords("english"))
inspect(lyrics_corpus)


## Remove redundant words
lyrics_corpus <- tm_map(lyrics_corpus, white_space, "mmm")
lyrics_corpus <- tm_map(lyrics_corpus, white_space, "oohoohoohoohooh")
lyrics_corpus <- tm_map(lyrics_corpus, white_space, "haa")
lyrics_corpus <- tm_map(lyrics_corpus, white_space, "ahaah")
lyrics_corpus <- tm_map(lyrics_corpus, white_space, "ohoh")
lyrics_corpus <- tm_map(lyrics_corpus, white_space, "ohohoh")
lyrics_corpus <- tm_map(lyrics_corpus, white_space, "haahah")
lyrics_corpus <- tm_map(lyrics_corpus, white_space, "ohohahh")
lyrics_corpus <- tm_map(lyrics_corpus, white_space, "huh")
lyrics_corpus <- tm_map(lyrics_corpus, white_space, "eheeheeheeheeheeheeheeh")


# text stemming
lyrics_corpus <- tm_map(lyrics_corpus, stemDocument)
inspect(lyrics_corpus)



# document-term matrix
lyrics_matrix <- TermDocumentMatrix(lyrics_corpus)
lyrics_matrix

lyrics_terms <- as.matrix(lyrics_matrix)
lyrics_terms




# frequency table for words
word_table <- sort(rowSums(lyrics_terms), decreasing = T)
word_table


# arrange the document-term matrix
arrange_matrix <- data.frame(word = names(word_table), freq = word_table)
arrange_matrix
arrange_matrix$word[grep("babi", arrange_matrix$word)] <- "baby"
arrange_matrix$word[grep("beauti", arrange_matrix$word)] <- "beauty"
arrange_matrix$word[grep("everi", arrange_matrix$word)] <- "every"
arrange_matrix$word[grep("sorri", arrange_matrix$word)] <- "sorry"
arrange_matrix$word[grep("fli", arrange_matrix$word)] <- "fly"
arrange_matrix$word[grep("realli", arrange_matrix$word)] <- "really"
arrange_matrix$word[grep("everybodi", arrange_matrix$word)] <- "everybody"
arrange_matrix$word[grep("cri", arrange_matrix$word)] <- "cry"
arrange_matrix$word[grep("stori", arrange_matrix$word)] <- "story"
arrange_matrix$word[grep("nobodi", arrange_matrix$word)] <- "nobody"
arrange_matrix$word[grep("crazi", arrange_matrix$word)] <- "crazy"
arrange_matrix$word[grep("lucki", arrange_matrix$word)] <- "lucky"
arrange_matrix$word[grep("memori", arrange_matrix$word)] <- "memory"
arrange_matrix$word[grep("happi", arrange_matrix$word)] <- "happy"
arrange_matrix$word[grep("pretti", arrange_matrix$word)] <- "pretty"
arrange_matrix$word[grep("mani", arrange_matrix$word)] <- "many"
arrange_matrix$word[grep("citi", arrange_matrix$word)] <- "city"
arrange_matrix$word[grep("somebodi", arrange_matrix$word)] <- "somebody"
arrange_matrix$word[grep("easi", arrange_matrix$word)] <- "easy"
arrange_matrix$word[grep("busi", arrange_matrix$word)] <- "busy"
arrange_matrix$word[grep("bodi", arrange_matrix$word)] <- "body"
arrange_matrix$word[grep("funni", arrange_matrix$word)] <- "funny"
arrange_matrix$word[grep("parti", arrange_matrix$word)] <- "party"
arrange_matrix$word[grep("readi", arrange_matrix$word)] <- "ready"
arrange_matrix$word[grep("daddi", arrange_matrix$word)] <- "daddy"
arrange_matrix$word[grep("marri", arrange_matrix$word)] <- "marry"
arrange_matrix$word[grep("tragedi", arrange_matrix$word)] <- "tragedy"
arrange_matrix$word[grep("worri", arrange_matrix$word)] <- "worry"
arrange_matrix$word[grep("raini", arrange_matrix$word)] <- "rainy"
arrange_matrix$word[grep("carri", arrange_matrix$word)] <- "carry"
arrange_matrix$word[grep("rosi", arrange_matrix$word)] <- "rosy"
arrange_matrix$word[grep("famili", arrange_matrix$word)] <- "family"
arrange_matrix$word[grep("alreadi", arrange_matrix$word)] <- "already"
arrange_matrix$word[grep("holi", arrange_matrix$word)] <- "holy"
arrange_matrix$word[grep("nasti", arrange_matrix$word)] <- "nasty"
arrange_matrix$word[grep("cori", arrange_matrix$word)] <- "cory"
arrange_matrix$word[grep("balconi", arrange_matrix$word)] <- "balcony"
arrange_matrix$word[grep("empti", arrange_matrix$word)] <- "empty"
arrange_matrix$word[grep("juli", arrange_matrix$word)] <- "july"
arrange_matrix$word[grep("graviti", arrange_matrix$word)] <- "gravity"
arrange_matrix$word[grep("gari", arrange_matrix$word)] <- "gary"
arrange_matrix$word[grep("lightbodi", arrange_matrix$word)] <- "lightbody"
arrange_matrix$word[grep("butterfli", arrange_matrix$word)] <- "butterfly"
arrange_matrix





# wordcloud
library(wordcloud)
library(RColorBrewer)
set.seed(123)

wordcloud(words = arrange_matrix$word, freq = arrange_matrix$freq, min.freq = 1, max.words = 200, random.order = F,
                    color = brewer.pal(8, "Dark2"))
library(ggplot2)
ggplot(data = arrange_matrix[1:30, ], aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "pink") + xlab("") + ylab("Frequency") + 
  ggtitle("Top 30 Frequent Used Words In Taylor Swift Songs") + coord_flip() +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.border = element_blank())




# Word association
# low association
findAssocs(lyrics_matrix, terms = c("know", "like", "dont"), corlimit = 0.05)
findAssocs(lyrics_matrix, terms = c("know", "like", "dont"), corlimit = 0.1)
findAssocs(lyrics_matrix, terms = c("know", "like", "dont"), corlimit = 0.2)
findAssocs(lyrics_matrix, terms = c("know", "like", "dont"), corlimit = 0.25)

# high association
findAssocs(lyrics_matrix, terms = c("know", "like", "dont"), corlimit = 0.7)


# Word association for word with frequency >= 50
findAssocs(lyrics_matrix, terms = findFreqTerms(lyrics_matrix, lowfreq = 50), corlimit = 0.3)
findAssocs(lyrics_matrix, terms = findFreqTerms(lyrics_matrix, lowfreq = 50), corlimit = 0.7)



# sentiment analysis
library(sentimentr)
library(syuzhet)
emotion <- get_nrc_sentiment(song$lyric)
emotion


emotion2 <- data.frame(t(emotion))
td <- data.frame(rowSums(emotion2))
head(td)


names(td)[1] <- "count"
td_new <- cbind(sentiment = rownames(td), td)
head(td_new)


rownames(td_new) <- NULL
head(td_new)
quickplot(sentiment, data = td_new, weight = count, geom = "bar",
          fill = sentiment, ylab = "count") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())



# install.packages("tidytext")
library(tidytext)
# install.packages("textdata")
library(textdata)
library(tidyverse)
count_bigrams <- function(dataset){
  dataset %>% unnest_tokens(bigram, lyric, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = T)
}

song_bigrams <- song %>% count_bigrams()
head(song_bigrams)

library(ggraph)
library(igraph)
library(grid)
visualize_bigrams <- function(bigrams){
  set.seed(123)
  a <- arrow(type = "closed", length = unit(0.1, "inches"))
  bigrams %>% graph_from_data_frame() %>% ggraph(layout = "fr") + geom_edge_link(aes(edge_alpha = n),
                                                                                 show.legend = F, arrow = a) +
    geom_node_point(color = "purple", size = 4) + 
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
    ggtitle("The Network Graph of Bigrams") + theme_void() + theme(plot.title = element_text(hjust = 0.5))
  }
song_bigrams %>% filter(n > 3, !str_detect(word1, "d"), !str_detect(word2, "d")) %>%
  visualize_bigrams()



tidy_song <- song %>% unnest_tokens(word, lyric)
words_count <- tidy_song %>% count(track_title)
song_counts <- tidy_song %>% left_join(words_count, by = "track_title") %>%
  rename(total_words = n)
song_sentiment <- tidy_song %>% inner_join(get_sentiments("nrc"), by = "word")
song_sentiment %>% count(word, sentiment, sort = T) %>% group_by(sentiment) %>% top_n(n = 10) %>%
  ungroup() %>% ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_col(show.legend = F) + facet_wrap(~ sentiment, scales = "free") + 
  xlab("Sentiments") + ylab("Scores") + 
  ggtitle("Top 10 Used Words in Expressing Different Emotions and Sentiments") +
  coord_flip() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                       panel.border = element_blank())



song_sentiment %>% count(track_title, sentiment, sort = T) %>% 
  group_by(sentiment) %>% top_n(n = 5) %>%
  ggplot(aes(x = reorder(track_title, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = F) + 
  facet_wrap(~ sentiment, scales = "free") + 
  xlab("Sentiments") + ylab("Scores") + 
  ggtitle("Top 5 Songs That are Associated With Different Emotions and Sentiments") +
  coord_flip() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                       panel.border = element_blank())


library(reshape2)
set.seed(123)
tidy_song %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = T) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#6d86f8", "#f86d6d"), max.words = 250, res = 100)



library(circlize)
grid.col = c("2006" = "#dbe600", "2008" = "#0054e6", 
             "2010" = "#00e668", "2012" = "#9d00e6", 
             "2014" = "#00c0e6", "2017" = "#00e6c7", 
             "anger" = "grey", "anticipation" = "grey", 
             "disgust" = "grey", "fear" = "grey", 
             "joy" = "grey", "sadness" = "grey", 
             "surprise" = "grey", "trust" = "grey")

year_emotion <- song_sentiment %>% filter(!sentiment %in% c("positive", "negative")) %>%
  count(sentiment, year) %>% group_by(year, sentiment) %>%
  summarise(sentiment_sum = sum(n)) %>% ungroup()

circos.clear()

circos.par(gap.after = c(rep(6, length(unique(year_emotion[[1]])) - 1), 15,
                         rep(6, length(unique(year_emotion[[2]])) - 1), 15))

chordDiagram(year_emotion, grid.col = grid.col,
             transparency = 0.2)
title("The Relationship Between Different Emotions and Song Release Years", line = -1)
legend("bottom", pch = 15, legend = names(grid.col[1:6]), col = grid.col, horiz = T, bty = "n", inset = c(0.3, 0.05))




#########################################END##############################################
#########################################END##############################################
#########################################END##############################################


##########################################
words = c(stopwords("english"))
tidy_song <- song %>% unnest_tokens("words", lyric, token = "words")
tidy_song_clean <- tidy_song %>% anti_join(stop_words, by = c("words" = "word"))
word_freq <- tidy_song_clean %>% inner_join(get_sentiments("bing"), by = c("words" = "word")) %>%
  count(words, sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(polarity = positive- negative) %>%
  filter(abs(polarity) >= 9) %>% mutate(pos_or_neg = ifelse(polarity > 0, "positve", "negative"))
# Plot polarity vs. (words reordered by polarity), filled by pos_or_neg
ggplot(word_freq, aes(x = reorder(words, polarity),y =  polarity, fill = pos_or_neg)) +
  geom_col() + 
  labs(
    x = "words",
    title = "Sentiment Word Frequency"
  )+
  # Rotate text and vertically justify
  theme(axis.text.x = element_text(angle = 55)) 
##################################################################################


###############################################################################
# Bi-grams
install.packages("gridExtra")
install.packages("RWeka")

library(gridExtra)
library(RWeka)

tokenizer <- function(x){
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

unigram_lyrics_matrix <- lyrics_matrix

bigram_dtm <- DocumentTermMatrix(lyrics_corpus, control = list(tokenize = tokenizer))

bigram_dtm_m <- as.matrix(bigram_dtm)

freq <- colSums(bigram_dtm_m)

bi_words <- names(freq)

wordcloud(bi_words, freq, max.words = 500,
          random.color = F, c(4, 0, 4),
          col = terrain.colors(length(bi_words), alpha = 0.9), rot.per = 0.3)
library(grid)
visualize_bigrams <- function(bigrams) {
  set.seed(123)
  a <- arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    ggtitle("Network graph of bigrams") +
    theme_void()
}

install.packages("ggraph")
library(ggraph)
library(igraph)
bi_words %>% 
  visualize_bigrams()
