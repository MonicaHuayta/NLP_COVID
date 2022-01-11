############################################################################
###### Analysis of Youtube comments of COVID vaccine USA vs UK #############
############### Hult Business School - Monica Huayta #######################
############################################################################

# Packages ----
library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(ggplot2)
library(tm)
library(NLP)
library(tidyr)
library(scales)
library(igraph)
library(ggraph)
library(widyr)
library(topicmodels)

# Setting my working directory ----
setwd("/Users/monicahuaytadurand/Desktop/R-Studio/NLP/Personal Project")

# Reading the databases ----
Comment_USA <- read.csv("Comment_USA.csv", header=FALSE, sep=",", quote="'")
Comment_UK  <- read.csv("Comment_UK.csv", header=FALSE, sep=",", quote="'")

# Cleaning Comments from USA ----
New_USA <- rbind(Comment_USA[,1],Comment_USA[,2])
for (i in 3:ncol(Comment_USA)) {
  New_USA <- rbind(New_USA,Comment_USA[,i])
}

New_USA <- as.data.frame(New_USA)
New_USA_1 <- New_USA$V1
New_USA_1 <- as.data.frame(New_USA_1)
New_USA_1 <- New_USA_1 %>% rename(text = New_USA_1)
New_USA_2 <- New_USA$V2
New_USA_2 <- as.data.frame(New_USA_2)
New_USA_2 <- New_USA_2 %>% rename(text = New_USA_2)
New_USA_3 <- New_USA$V3
New_USA_3 <- as.data.frame(New_USA_3)
New_USA_3 <- New_USA_3 %>% rename(text = New_USA_3)
New_USA_4 <- New_USA$V4
New_USA_4 <- as.data.frame(New_USA_4)
New_USA_4 <- New_USA_4 %>% rename(text = New_USA_4)

base_USA <- rbind(New_USA_1,New_USA_2,New_USA_3,New_USA_4)

index_USA <-as.data.frame(base_USA$text=="")
base_USA <- base_USA[!index_USA,]
base_USA <- as.data.frame(base_USA)
colnames(base_USA) <- c("text")
base_USA <- cbind.data.frame(id=c(1:nrow(base_USA)),text = base_USA$text)

# Initial analysis of USA----
tidy_USA   <- base_USA                        %>%  
              unnest_tokens(word,text)        %>% 
              anti_join(stop_words)           

tokens_USA <- base_USA                        %>%  
              unnest_tokens(word,text)        %>% 
              anti_join(stop_words)           %>% 
              count(word, sort=TRUE)          %>% 
              mutate(word = reorder(word,n))

top_n_USA <- tokens_USA  %>%
              top_n(15,n)
  
ggplot(top_n_USA, aes(word,n)) + geom_col(show.legend = F) + 
  coord_flip() + ggtitle("Top 15 USA tokens") +
  theme(plot.title = element_text(hjust = 0.5))

# Analyzing sentiments from USA-----
sentiment_USA <- tidy_USA %>% 
                 inner_join(get_sentiments("loughran")) %>% 
                 count(word,sentiment, sort=T) %>% 
                 ungroup()

sentiment_USA %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# Bigrams and quadrograms USA----

# Bigrams
USA_bigrams <- base_USA %>%
               unnest_tokens(word,text, token="ngrams", n=2) %>% 
               separate(word, c("word1","word2"), sep=" ") %>% 
               filter(!word1 %in% stop_words$word) %>% 
               filter(!word2 %in% stop_words$word) %>% 
               count(word1, word2, sort=T)

USA_bigraph <- USA_bigrams %>% 
                filter(n>2) %>% 
                graph_from_data_frame()

ggraph(USA_bigraph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name), vjust=1, hjust=1)

# Quadrograms  
USA_quadrogram <- base_USA %>%
  unnest_tokens(word,text, token="ngrams", n=4) %>% 
  separate(word, c("word1","word2","word3","word4"), sep=" ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  count(word1, word2, word3, word4, sort=T)

USA_quadroraph <- USA_quadrogram %>% 
  filter(n>1) %>% 
  graph_from_data_frame()

ggraph(USA_quadroraph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name), vjust=1, hjust=1)

# Correlation Pairwise USA ----
word_cors_USA <- tidy_USA %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, id, sort=TRUE)

word_cors_USA %>%
  filter(item1 %in% c("covid", "fake", "vaccine", "biden")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()

word_cors_USA %>%
  filter(correlation >.7) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=2)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()

# LDA USA ----
dtm_USA <- tidy_USA %>%
           count(id, word) %>%
           cast_dtm(id, word, n)

LDA_USA <- LDA(dtm_USA, k=4, control=list(seed=123))
USA_topics <- tidy(LDA_USA, matrix="beta")
top_terms_USA <- USA_topics %>%
                 group_by(topic) %>%
                 top_n(10, beta) %>%
                 ungroup() %>%
                 arrange(topic, -beta)

top_terms_USA %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()  
  
# Cleaning Comments from UK ----
New_UK <- rbind(Comment_UK[,1],Comment_UK[,2])
for (i in 3:ncol(Comment_UK)) {
  New_UK <- rbind(New_UK,Comment_UK[,i])
}

New_UK <- as.data.frame(New_UK)
New_UK_1 <- New_UK$V1
New_UK_1 <- as.data.frame(New_UK_1)
New_UK_1 <- New_UK_1 %>% rename(text = New_UK_1)
New_UK_2 <- New_UK$V2
New_UK_2 <- as.data.frame(New_UK_2)
New_UK_2 <- New_UK_2 %>% rename(text = New_UK_2)
New_UK_3 <- New_UK$V3
New_UK_3 <- as.data.frame(New_UK_3)
New_UK_3 <- New_UK_3 %>% rename(text = New_UK_3)

base_UK <- rbind(New_UK_1,New_UK_2,New_UK_3)
index_UK <-as.data.frame(base_UK$text=="")
base_UK <- base_UK[!index_UK,]
base_UK <- as.data.frame(base_UK)
colnames(base_UK) <- c("text")
base_UK <- cbind.data.frame(id=c(1:nrow(base_UK)),text = base_UK$text)

# Initial analysis for UK----
tidy_UK  <- base_UK                          %>%
            unnest_tokens(word,text)         %>% 
            anti_join(stop_words)            

tokens_UK <- base_UK                          %>%
             unnest_tokens(word,text)         %>% 
             anti_join(stop_words)            %>%
             count(word, sort=TRUE)           %>% 
             mutate(word = reorder(word,n))

top_n_UK <- tokens_UK  %>% 
             top_n(15,n)

ggplot(top_n_UK, aes(word,n)) + geom_col(show.legend = F) + 
  coord_flip() + ggtitle("Top 15 UK tokens") +
  theme(plot.title = element_text(hjust = 0.5))
 
# Analyzing sentiments from UK-----
sentiment_UK <- tidy_UK %>% 
  inner_join(get_sentiments("loughran")) %>% 
  count(word,sentiment, sort=T) %>% 
  ungroup()

sentiment_UK %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# Bigrams and quadrograms Uk----

# Bigrams
UK_bigrams <- base_UK %>%
  unnest_tokens(word,text, token="ngrams", n=2) %>% 
  separate(word, c("word1","word2"), sep=" ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  count(word1, word2, sort=T)

UK_bigraph <- UK_bigrams %>% 
  filter(n>2) %>% 
  graph_from_data_frame()

ggraph(UK_bigraph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name), vjust=1, hjust=1)

# Quadrograms  
UK_quadrogram <- base_USA %>%
  unnest_tokens(word,text, token="ngrams", n=4) %>% 
  separate(word, c("word1","word2","word3","word4"), sep=" ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  count(word1, word2, word3, word4, sort=T)

UK_quadroraph <- UK_quadrogram %>% 
  filter(n>2) %>% 
  graph_from_data_frame()

ggraph(UK_quadroraph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name), vjust=1, hjust=1)

# Correlation Pairwise UK ----
word_cors_UK <- tidy_UK %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, id, sort=TRUE)

word_cors_UK %>%
  filter(item1 %in% c("covid", "fake", "vaccine", "boris")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()

word_cors_UK %>%
  filter(correlation >.7) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=2)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()

# LDA UK ----
dtm_UK <- tidy_UK %>%
  count(id, word) %>%
  cast_dtm(id, word, n)

LDA_UK <- LDA(dtm_UK, k=4, control=list(seed=123))
UK_topics <- tidy(LDA_UK, matrix="beta")
top_terms_UK <- UK_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_UK %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()  

# Combining data between USA and UK----
mixed_data <- bind_rows(mutate(tokens_USA, country="USA"),
                        mutate(tokens_UK,  country="UK"))

top_n_mixed <- mixed_data           %>% 
               group_by(country) %>% 
               top_n(15,n) %>% 
               ungroup() %>% 
               mutate(word2 = fct_reorder(word,n))

ggplot(top_n_mixed, aes(x=word2, y=n, fill=country)) +
  geom_col(show.legend=F) +
  facet_wrap(~country, scales="free") +
  coord_flip() +
  ggtitle("Top 15 tokens in UK and USA")

# Analyzing correlograms and correlation----
frequency <- bind_rows(mutate(tidy_USA, country="USA"),
                       mutate(tidy_UK, country= "UK")) %>%
             mutate(word=str_extract(word, "[a-z']+")) %>%
             count(country, word) %>%
             group_by(country) %>%
             mutate(proportion = n/sum(n))%>%
             select(-n) %>%
             spread(country, proportion) %>%
             gather(country, proportion, `UK`)

ggplot(frequency, aes(x=proportion, y=`USA`, 
  color = abs(`USA`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~country, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "USA", x=NULL)

cor.test(data=frequency[frequency$country == "UK",],
         ~proportion + `USA`)
