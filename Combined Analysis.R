combined_us <-bind_rows(
  mutate(combined_b, artist = "Billie Eilish"),
  mutate(combined_hal, artist = "Halsey"),
  mutate(combined_s, artist = "Selena Gomez"),
  mutate(combined_har, artist = "Harry Styles"),
  mutate(combined_c, artist = "Camila Cabello"))

USA_combined <- combined_us%>%
  bind_tf_idf(word,artist,n)%>%
  arrange(desc(tf_idf))
  
  

frequency1 <- combined_us%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(artist, word) %>%
  group_by(artist) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(artist, proportion) %>%
  gather(artist, proportion, `Halsey`,`Harry Styles`,`Selena Gomez`,`Camila Cabello`)

ggplot(frequency1, aes(x=proportion, y=`Billie Eilish`, 
                      color = abs(`Billie Eilish`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~artist, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Billie Eilish", x=NULL)



frequency2 <- combined_us%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(artist, word) %>%
  group_by(artist) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(artist, proportion) %>%
  gather(artist, proportion, `Billie Eilish`,`Harry Styles`,`Selena Gomez`,`Camila Cabello`)

ggplot(frequency2, aes(x=proportion, y=`Halsey`, 
                       color = abs(`Halsey`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~artist, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Halsey", x=NULL)



frequency3 <- combined_us%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(artist, word) %>%
  group_by(artist) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(artist, proportion) %>%
  gather(artist, proportion, `Billie Eilish`,`Halsey`,`Selena Gomez`,`Camila Cabello`)

ggplot(frequency3, aes(x=proportion, y=`Harry Styles`, 
                       color = abs(`Harry Styles`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~artist, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Harry Styles", x=NULL)


frequency4 <- combined_us%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(artist, word) %>%
  group_by(artist) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(artist, proportion) %>%
  gather(artist, proportion, `Billie Eilish`,`Halsey`,`Harry Styles`,`Camila Cabello`)

ggplot(frequency4, aes(x=proportion, y=`Selena Gomez`, 
                       color = abs(`Selena Gomez`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~artist, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Selena Gomez", x=NULL)


USA_combined %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(artist) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=song))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~artist, ncol=2, scales="free")+
  coord_flip()


USA_combined %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)


USA_combined %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

bing_counts <- USA_combined %>%
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

afinn_sent <- USA_combined %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value))

afinn_sent

USA_combined%>%
  cast_dtm(artist,word,n)
