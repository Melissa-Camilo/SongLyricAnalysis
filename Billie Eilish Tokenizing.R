library(textreadr)
library(dplyr)
library(stringr)
library(wordcloud)
library(tidytext)
library(ggplot2)
library(reshape2)
library(tidyr)
library(textdata)


#Loadinf file 
NLP <- read_document(file="Billie Eilish.docx")

#Setting into appropiate formats 
a <- 1 #how many observations to you have (text)
b <- 13 #how many variables do you have (songs)
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- NLP[i*b+z-b]
  }#closing z loop
}#closing i loop
my_txt1 <- my_df$V1
my_txt2 <- my_df$V2
my_txt3 <- my_df$V3
my_txt4 <- my_df$V4
my_txt5 <- my_df$V5
my_txt6 <- my_df$V6
my_txt7 <- my_df$V7
my_txt8 <- my_df$V8
my_txt9 <- my_df$V9
my_txt10 <- my_df$V10
my_txt11<- my_df$V11
my_txt12 <- my_df$V12
my_txt13 <- my_df$V13

mydf1 <- data_frame(line=1, text=my_txt1)

mydf2 <- data_frame(line=1, text=my_txt2)

mydf3 <- data_frame(line=1, text=my_txt3)

mydf4 <- data_frame(line=1, text=my_txt4)

mydf5 <- data_frame(line=1, text=my_txt5)

mydf6 <- data_frame(line=1, text=my_txt6)

mydf7 <- data_frame(line=1, text=my_txt7)
mydf8 <- data_frame(line=1, text=my_txt8)

mydf9 <- data_frame(line=1, text=my_txt9)

mydf10 <- data_frame(line=1, text=my_txt10)

mydf11 <- data_frame(line=1, text=my_txt11)

mydf12 <- data_frame(line=1, text=my_txt12)

mydf13 <- data_frame(line=1, text=my_txt13)


#Tokenization
data(stop_words)
frequencies_tokens_nostop1 <- mydf1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)

stop_words

frequencies_tokens_nostop2 <- mydf2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
frequencies_tokens_nostop3 <- mydf3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
frequencies_tokens_nostop4 <- mydf4 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
frequencies_tokens_nostop5 <- mydf5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
frequencies_tokens_nostop6 <- mydf6 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
frequencies_tokens_nostop7 <- mydf7 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
frequencies_tokens_nostop8 <- mydf8 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)

frequencies_tokens_nostop9 <- mydf9 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
frequencies_tokens_nostop10 <- mydf10 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
frequencies_tokens_nostop11 <- mydf11 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
frequencies_tokens_nostop12 <- mydf12 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
frequencies_tokens_nostop13 <- mydf13 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)



#TFIDF
combined_b <-bind_rows(
  mutate(frequencies_tokens_nostop1, song = "Bad Guy"),
  mutate(frequencies_tokens_nostop2, song = "xanny"),
  mutate(frequencies_tokens_nostop3, song = "You Should See Me in a Crown"),
  mutate(frequencies_tokens_nostop4, song = "all the good girls go to hell"),
  mutate(frequencies_tokens_nostop5, song = "Wish You Were Gay"),
  mutate(frequencies_tokens_nostop6, song = "when the party's over"),
  mutate(frequencies_tokens_nostop7, song = "8"),
  mutate(frequencies_tokens_nostop8, song = "My Strange Addiction"),
  mutate(frequencies_tokens_nostop9, song = "Bury a Freind"),
  mutate(frequencies_tokens_nostop10, song = "ilomilo"),
  mutate(frequencies_tokens_nostop11, song = "listen before i go"),
  mutate(frequencies_tokens_nostop12, song = "I Love You"),
  mutate(frequencies_tokens_nostop13, song = "Good Bye"),
)

Billie_combined <- combined_b%>%
  bind_tf_idf(word,song,n)

Billie_combined %>%
  arrange(desc(tf_idf))

Billie_combined %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(song) %>%
  top_n(4) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=song))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~song, ncol=2, scales="free")+
  coord_flip()


Billie_combined %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)


Billie_combined %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

bing_counts <- Billie_combined %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


afinn_sent <- Billie_combined %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value))

afinn_sent



###########################################
# BIGRAMS                                 #
###########################################

bigrams1 <- mydf1 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)
bigrams2 <- mydf2 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)
bigrams3 <- mydf3 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)
bigrams4 <- mydf4 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)
bigrams5 <- mydf5 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)
bigrams6 <- mydf6 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)
bigrams7 <- mydf7 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)
bigrams8 <- mydf8 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)
bigrams9 <- mydf9 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)
bigrams10 <- mydf10 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)
bigrams10 <- mydf10 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)
bigrams10 <- mydf10 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)
bigrams11 <- mydf11 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)
bigrams12 <- mydf12 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)
bigrams13 <- mydf13 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)


combined_bigram <-bind_rows(
  mutate(bigrams1, song = "Bad Guy"),
  mutate(bigrams2, song = "xanny"),
  mutate(bigrams3, song = "You Should See Me in a Crown"),
  mutate(bigrams4, song = "all the good girls go to hell"),
  mutate(bigrams5, song = "Wish You Were Gay"),
  mutate(bigrams6, song = "when the party's over"),
  mutate(bigrams7, song = "8"),
  mutate(bigrams8, song = "My Strange Addiction"),
  mutate(bigrams9, song = "Bury a Freind"),
  mutate(bigrams10, song = "ilomilo"),
  mutate(bigrams11, song = "listen before i go"),
  mutate(bigrams12, song = "I Love You"),
  mutate(bigrams13, song = "Good Bye"),
)


negation_tokens <- c("no","never","without","not","dont")#what negation tokens do you want to use?




negated_words <- combined_bigram %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

negated_words

Billie_combined%>%
  cast_dtm(song,word,n)