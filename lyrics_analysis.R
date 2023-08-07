# lyrics analysis


library(tm)
library(textstem)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)

lyrics <- read.csv("lyrics_unstructured.csv", header = T, strip.white = T)
str(lyrics)
data.frame(artist = unique(lyrics$artist), song = unique(lyrics$song), year = unique(lyrics$year), theme = unique(lyrics$theme))

######################################################Friendship#################################################################
# to corpus
corpus_lyrics_friendship <- Corpus(VectorSource(lyrics$lyrics[lyrics$theme == "friendship"]))
class(corpus_lyrics_friendship)
inspect(corpus_lyrics_friendship)

# replace with white space
white_space <- content_transformer(function(x, pattern) gsub(pattern, "", x))
white_space

# remove punctuation
corpus_lyrics_friendship <- tm_map(corpus_lyrics_friendship, removePunctuation)
inspect(corpus_lyrics_friendship)

# lowercase
corpus_lyrics_friendship <- tm_map(corpus_lyrics_friendship, content_transformer(tolower))
inspect(corpus_lyrics_friendship)

# remove stopwords
corpus_lyrics_friendship <- tm_map(corpus_lyrics_friendship, removeWords, stopwords("english"))
inspect(corpus_lyrics_friendship)
corpus_lyrics_friendship <- tm_map(corpus_lyrics_friendship, white_space, "oh")
corpus_lyrics_friendship <- tm_map(corpus_lyrics_friendship, white_space, "hh")
corpus_lyrics_friendship <- tm_map(corpus_lyrics_friendship, white_space, "gonna")
corpus_lyrics_friendship <- tm_map(corpus_lyrics_friendship, white_space, "cause")
inspect(corpus_lyrics_friendship)

# remove excessive white spaces
corpus_lyrics_friendship <- tm_map(corpus_lyrics_friendship, stripWhitespace)
inspect(corpus_lyrics_friendship)

# lemmatise
corpus_lyrics_friendship <- tm_map(corpus_lyrics_friendship, lemmatize_strings)
inspect(corpus_lyrics_friendship)

# dtm
document_term_matrix_friendship <- TermDocumentMatrix(corpus_lyrics_friendship)
document_term_matrix_friendship

matrix_dtm_friendship <- as.matrix(document_term_matrix_friendship)
matrix_dtm_friendship

# frequency of terms
word_freq_friendship <- sort(rowSums(matrix_dtm_friendship), decreasing = T)
word_freq_friendship_df <- data.frame(word = names(word_freq_friendship), freq = word_freq_friendship)
head(word_freq_friendship_df)

# wordcloud
set.seed(1000)
wordcloud(words = word_freq_friendship_df$word, 
          freq = word_freq_friendship_df$freq, 
          min.freq = 1, max.words = 300, 
          random.order = F, color = brewer.pal(8, "Dark2"))


######################################################Romance###############################################################
# to corpus
corpus_lyrics_romance <- Corpus(VectorSource(lyrics$lyrics[lyrics$theme == "romance"]))
class(corpus_lyrics_romance)
inspect(corpus_lyrics_romance)

# remove punctuation
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, removePunctuation)
inspect(corpus_lyrics_romance)

# lowercase
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, content_transformer(tolower))
inspect(corpus_lyrics_romance)

# remove stopwords
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, removeWords, stopwords("english"))
inspect(corpus_lyrics_romance)
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, white_space, "im")
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, white_space, "youll")
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, white_space, "ill")
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, white_space, "oh")
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, white_space, "cause")
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, white_space, "just")
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, white_space, "theres")
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, white_space, "said")
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, white_space, "say")
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, white_space, "ive")
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, white_space, "thats")
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, white_space, "theyre")
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, white_space, "tryna")
inspect(corpus_lyrics_romance)

# remove excessive white spaces
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, stripWhitespace)
inspect(corpus_lyrics_romance)

# lemmatise
corpus_lyrics_romance <- tm_map(corpus_lyrics_romance, lemmatize_strings)
inspect(corpus_lyrics_romance)

# dtm
document_term_matrix_romance <- TermDocumentMatrix(corpus_lyrics_romance)
document_term_matrix_romance

matrix_dtm_romance <- as.matrix(document_term_matrix_romance)
matrix_dtm_romance

# frequency of terms
word_freq_romance <- sort(rowSums(matrix_dtm_romance), decreasing = T)
word_freq_romance_df <- data.frame(word = names(word_freq_romance), freq = word_freq_romance)
head(word_freq_romance_df)

# wordcloud
set.seed(1000)
wordcloud(words = word_freq_romance_df$word, 
          freq = word_freq_romance_df$freq, 
          min.freq = 1, max.words = 200, 
          scale=c(2.9,-0.1), random.order = F, 
          color = brewer.pal(8, "Dark2"))

###################################################Heartbreak############################################################
# to corpus
corpus_lyrics_heartbreak <- Corpus(VectorSource(lyrics$lyrics[lyrics$theme == "heartbreak"]))
class(corpus_lyrics_heartbreak)
inspect(corpus_lyrics_heartbreak)

# remove punctuation
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, removePunctuation)
inspect(corpus_lyrics_heartbreak)

# lowercase
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, content_transformer(tolower))
inspect(corpus_lyrics_heartbreak)

# remove stopwords
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, removeWords, stopwords("english"))
inspect(corpus_lyrics_heartbreak)
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, white_space, "theres")
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, white_space, "will")
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, white_space, "ill")
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, white_space, "see")
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, white_space, "dont")
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, white_space, "ive")
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, white_space, "us")
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, white_space, "wont")
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, white_space, "youre")
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, white_space, "gonna")
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, white_space, "cant")
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, white_space, "couldve")
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, white_space, "whoa")
inspect(corpus_lyrics_heartbreak)

# remove excessive white spaces
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, stripWhitespace)
inspect(corpus_lyrics_heartbreak)

# lemmatise
corpus_lyrics_heartbreak <- tm_map(corpus_lyrics_heartbreak, lemmatize_strings)
inspect(corpus_lyrics_heartbreak)

# dtm
document_term_matrix_heartbreak <- TermDocumentMatrix(corpus_lyrics_heartbreak)
document_term_matrix_heartbreak

matrix_dtm_heartbreak <- as.matrix(document_term_matrix_heartbreak)
matrix_dtm_heartbreak

# frequency of terms
word_freq_heartbreak <- sort(rowSums(matrix_dtm_heartbreak), decreasing = T)
word_freq_heartbreak_df <- data.frame(word = names(word_freq_heartbreak), freq = word_freq_heartbreak)
head(word_freq_heartbreak_df)

# wordcloud
set.seed(1234)
wordcloud(words = word_freq_heartbreak_df$word, 
          freq = word_freq_heartbreak_df$freq, 
          scale=c(4,0.3), min.freq = 1, 
          max.words = 300, random.order = F, 
          color = brewer.pal(8, "Dark2"))

