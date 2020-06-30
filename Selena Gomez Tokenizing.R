library(textreadr)
library(dplyr)
library(stringr)
library(wordcloud)
library(tidytext)
library(ggplot2)
library(reshape2)


#Loadinf file 
NLP <- read_document(file="Selena Gomez.docx")

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
combined_s <-bind_rows(
  mutate(frequencies_tokens_nostop1, song = "Rare"),
  mutate(frequencies_tokens_nostop2, song = "Dance Again"),
  mutate(frequencies_tokens_nostop3, song = "Look At Her Now"),
  mutate(frequencies_tokens_nostop4, song = "Lose You to Love Me"),
  mutate(frequencies_tokens_nostop5, song = "Ring"),
  mutate(frequencies_tokens_nostop6, song = "Vulnerable"),
  mutate(frequencies_tokens_nostop7, song = "People You Know"),
  mutate(frequencies_tokens_nostop8, song = "Let Me Get Me"),
  mutate(frequencies_tokens_nostop9, song = "Crowded Room"),
  mutate(frequencies_tokens_nostop10, song = "Kinda Crazy"),
  mutate(frequencies_tokens_nostop11, song = "Fun"),
  mutate(frequencies_tokens_nostop12, song = "Cut You Off"),
  mutate(frequencies_tokens_nostop13, song = "A Sweeter Place"),
)

selena_combined <- combined_s%>%
  bind_tf_idf(word,song,n)

selena_combined %>%
  arrange(desc(tf_idf))

selena_combined %>%
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


selena_combined %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)


selena_combined %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

bing_counts <- selena_combined %>%
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

bing_counts

afinn_sent <- selena_combined %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value))

afinn_sent


selena_combined%>%
  cast_dtm(song,word,n)
