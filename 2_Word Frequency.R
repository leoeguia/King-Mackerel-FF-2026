###call in packages***

library(googlesheets4)
library(devtools)
library(stringr)
library(reshape2)
library(dplyr)
library(readr)
library(leaflet)
library(htmlwidgets)
library(webshot)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(readxl)


##pull in data
rm(list=ls())
#setwd("C:/Users/Leo/Documents/R working directory/Fisherman Feedback") #adjust accordingly
df<- read_excel("King Mackerel_Fisherman Feedback_2026_CLEAN for analysis.xlsx") #Change this for different species

###create new database that is only the comments NOTE: that column and row changes each time
#rg<-df[1:200,1]
rg <- as.character(df[[7]])
#rg$id <- 1:nrow(rg)
#rg <- gsub("\\$" ,"", rg)
#rg <- gsub("[?]", "'", rg)
rg <- gsub("\\b(shark)\\b", "sharks", rg, ignore.case = TRUE)
rg <- gsub("\\b(dolphin|porpoise|porpoises)\\b", "dolphins", rg, ignore.case = TRUE)
rg <- gsub("smaller", "small", rg, ignore.case = TRUE)
rg <- gsub("larger", "large", rg, ignore.case = TRUE)
rg <- gsub("bigger", "big", rg, ignore.case = TRUE)
rg <- gsub("shorter", "short", rg, ignore.case = TRUE)
rg <- gsub("no shortage", "no_shortage", rg, ignore.case = TRUE)
rg <- gsub("abundance", "abundant", rg, ignore.case = TRUE)
rg <- gsub("consistently", "consistent", rg, ignore.case = TRUE)
rg <- gsub("worst", "worse", rg, ignore.case = TRUE)
rg <- gsub("downward", "down", rg, ignore.case = TRUE)
rg <- gsub("\\b(hard)\\b", "harder", rg, ignore.case = TRUE)
rg <- gsub("\\b(fewer)\\b", "few", rg, ignore.case = TRUE)
rg <- gsub("\\b(improve|improving|improvement|improvements)\\b", "improved", rg, ignore.case = TRUE)
rg <- gsub("\\b(increased|increasing|increasing|increasement|increasements)\\b", "increase", rg, ignore.case = TRUE)
rg <- gsub("\\b(disappear|disappearing|disappeared.there)\\b", "disappeared", rg, ignore.case = TRUE)
rg <- gsub("plenty", "plentiful", rg, ignore.case = TRUE)
rg <- gsub("\\b(declined|declining|declines)\\b", "decline", rg, ignore.case = TRUE)
rg <- gsub("\\b(red tide|red tides)\\b","redtide",rg,ignore.case = TRUE)
rg <- gsub("\\b(nets|netting|netted|netters|seines)\\b","net",rg,ignore.case = TRUE)
rg <- gsub("\\b(gil)\\b","gill", rg, ignore.case = TRUE)
rg <- gsub("\\b(gill net|net)\\b","gillnet", rg, ignore.case = TRUE)
rg <- gsub("Mexican", "Mexico", rg, ignore.case = TRUE)
out <- c()

###modify the Bing library for more fisheries context

dropword <-c("bloom","redtide","discard", "wreck","bait","shipwreck","limit","shark","lure","shallow","significant","catch","caught","catching","worked","hang","pretty","aggressive","hogs",'biting','like', 'winning','fall', 'lucky','well','top','important','enough')
sentiments <- get_sentiments("bing") %>% filter(!word %in% dropword)

sentiment.neg<-tibble(word=c("bloom","redtide","discard","harder","smaller","small","decrease","decreased","fewer","overfished","lower","less","diminished","shark","sharks","dolphin","dolphins","release", 'net', 'gillnet', 'down'),sentiment=c("negative"))
sentiment.pos<-tibble(word=c("bite","large","larger","biting","increase","increased","plenty","uptick","aggressive","no_shortage",'more'),sentiment=c("positive"))
additional_sentiment <- sentiment.neg%>%
  rbind(sentiment.pos)
new_sentiment <- sentiments%>%
  rbind(additional_sentiment)

#Different combos of "envious" categorized as both positive and negative
# (dupcheck <- count(new_sentiment,word) %>%
#   filter(n>1))
# dupcomp <- new_sentiment %>%
#   filter(word %in% dupcheck$word)


###run a loop that looks that scores each comment by sentiment
for(i in 1:length(rg)){
  #for(i in 1:2){
  tokens.i <- tibble(text = rg[i]) %>% unnest_tokens(word, text)
  x <- inner_join(tokens.i, new_sentiment)
  POS <- nrow(subset(x, sentiment=="positive"))
  NEG <- nrow(subset(x, sentiment=="negative"))
  SENT <- POS - NEG
  df <- data.frame(positive=POS, negative=NEG, sentiment=SENT)
  df$standardized <- df$sentiment/nrow(x) ###standardize for number of words
  df$words <- nrow(x)
  out <- rbind(out, df)
}


###organize sentiment category into groups (-1 to -.33 neg, -.33 to .33 neutral, .33 to 1 pos)
out2 <- na.omit(out)
out2$standardized <- as.numeric(as.character(out2$standardized))
out2$lab <- cut(out2$standardized, breaks = c(-1,-.33,.33,1), include.lowest=TRUE)
out2$type <- with(out2, ifelse(standardized < -.33, '-1',
                               ifelse(standardized > .33, '1', '0')))
colors <- c("#fc8d59", "#ffffbf", "#99d594")
out2 %>% 
  group_by(lab) %>%
  summarise(no_rows = length(lab))

out$type <- with(out, ifelse(standardized < -.33, '-1',
                             ifelse(standardized > .33, '1', '0')))

###write out the database for John to use for mapping
#write.csv(out,"Gag_john.csv")       



### run sentiment analysis to examine each word
rg2<-tibble(line=1:length(rg), text=rg)
rg3<-rg2%>%unnest_tokens(word,text)

overall_word_counts <- rg3 %>%
  count(word, sort = TRUE) %>%
  ungroup()

bing_word_counts <- rg3 %>%
  inner_join(new_sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#bing_word_counts <- gsub("no_shortage", "no shortage", bing_word_counts, ignore.case = TRUE)
###This creates the word sentiment bar chart
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  scale_fill_manual(values=c("#fc8d59","#99d594"))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +theme(axis.title=element_text(size=14),axis.text=element_text(size=12))+theme(strip.text=element_text(size=14))+
  scale_y_continuous()+
  coord_flip()

top_words <- bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) 

barchart <- ggplot(top_words, aes(word, n, fill = sentiment)) +
  scale_fill_manual(values=c("#fc8d59","#99d594"))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +theme(axis.title=element_text(size=14),axis.text=element_text(size=12))+theme(strip.text=element_text(size=14))+
  scale_y_continuous()+
  coord_flip()
print(barchart)

ggsave("Word Plots/Most Frequent Bar.png", 
       plot = barchart,
       width = 5.67, #Adjust width as needed
       height = 3.37, #Adjust height as needed
       units = "in", 
       dpi = 700)
###This creates the sentiment word cloud
png("Word Plots/Most Frequent Cloud_50.png",
    # width = 3.56, #Adjust width as needed
    # height = 3.37, #Adjust height as needed
    width = 4.45, #Adjust width as needed
    height = 4.2125, #Adjust height as needed
    units = "in", 
    res = 700)
par(mar = c(0, 0, 0, 0))
rg3 %>%
  inner_join(new_sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#fc8d59","#99d594"), 
                   max.words = 50, title.size=1.5,match.color=TRUE)
dev.off()

#Just to see the word plot
par(mar = c(0, 0, 0, 0))
rg3 %>%
  inner_join(new_sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#fc8d59","#99d594"), 
                   max.words = 50, title.size=1.5,match.color=TRUE)
# Abundance People Only ---------------------------------------------------

df<- read_xlsx("King Mackerel_Fisherman Feedback_2026_CLEAN for analysis.xlsx") #Change this for different species
#setwd("C:/Users/LisaH/OneDrive - GOM/Desktop/WFH/sentiment/gray triggerfish 25")
#df<-read.csv("gt_comments.csv",header=T)

df <- df[df$`Final Related to Stock Condition`=='y',]

###create new database that is only the comments NOTE: that column and row changes each time
#rg<-df[1:200,1]
rg <- as.character(df[[7]])
#rg$id <- 1:nrow(rg)
#rg <- gsub("\\$" ,"", rg)
#rg <- gsub("[?]", "'", rg)
rg <- gsub("\\b(shark)\\b", "sharks", rg, ignore.case = TRUE)
#rg<- gsub("sharkss", "sharks", rg, ignore.case = TRUE)
rg <- gsub("\\b(dolphin|porpoise|porpoises)\\b", "dolphins", rg, ignore.case = TRUE)
#rg<- gsub("dolphinss", "dolphins", rg, ignore.case = TRUE)
#rg <- gsub("porpoises", "dolphins", rg, ignore.case = TRUE)
#Combine small/smaller, large/larger, big/bigger, short/shorter
rg <- gsub("smaller", "small", rg, ignore.case = TRUE)
rg <- gsub("larger", "large", rg, ignore.case = TRUE)
rg <- gsub("bigger", "big", rg, ignore.case = TRUE)
rg <- gsub("shorter", "short", rg, ignore.case = TRUE)
rg <- gsub("no shortage", "no_shortage", rg, ignore.case = TRUE)
rg <- gsub("abundance", "abundant", rg, ignore.case = TRUE)
rg <- gsub("consistently", "consistent", rg, ignore.case = TRUE)
rg <- gsub("worst", "worse", rg, ignore.case = TRUE)
rg <- gsub("downward", "down", rg, ignore.case = TRUE)
rg <- gsub("\\b(hard)\\b", "harder", rg, ignore.case = TRUE)
rg <- gsub("\\b(fewer)\\b", "few", rg, ignore.case = TRUE)
rg <- gsub("\\b(improve|improving|improvement|improvements)\\b", "improved", rg, ignore.case = TRUE)
rg <- gsub("\\b(increased|increasing|increasing|increasement|increasements)\\b", "increase", rg, ignore.case = TRUE)
rg <- gsub("\\b(disappear|disappearing|disappeared.there)\\b", "disappeared", rg, ignore.case = TRUE)
rg <- gsub("\\b(decrease|decreasing|decreased)\\b", "decreased", rg, ignore.case = TRUE)
rg <- gsub("plenty", "plentiful", rg, ignore.case = TRUE)
rg <- gsub("\\b(declined|declining|declines)\\b", "decline", rg, ignore.case = TRUE)
#rg <- gsub("red tide", "redtide", rg, ignore.case = TRUE)
rg <- gsub("\\b(red tide|red tides)\\b","redtide",rg,ignore.case = TRUE)
#rg <- gsub("gill net", "gillnet", rg, ignore.case = TRUE)
# rg <- gsub("netting", "net", rg, ignore.case = TRUE)
# rg <- gsub("net", "gillnet", rg, ignore.case = TRUE)
# rg <- gsub("gillgillnet", "gillnet", rg, ignore.case = TRUE)
# rg <- gsub("gillnetnet", "gillnet", rg, ignore.case = TRUE)
rg <- gsub("\\b(nets|netting|netted|netters|seines)\\b","net",rg,ignore.case = TRUE)
rg <- gsub("\\b(gil)\\b","gill", rg, ignore.case = TRUE)
rg <- gsub("\\b(gill net|net)\\b","gillnet", rg, ignore.case = TRUE)
#rg <- gsub("\\b(gill net|netting|net|gillnets|gillnetted|gillnetters|gillnetting|gil|seines)\\b","gillnet",rg,ignore.case = TRUE)
rg <- gsub("Mexican", "Mexico", rg, ignore.case = TRUE)
out <- c()

###modify the Bing library for more fisheries context

dropword <-c("bloom","redtide","discard", "wreck","bait","shipwreck","limit","shark","lure","shallow","significant","catch","caught","catching","worked","hang","pretty","aggressive","hogs",'biting','like', 'winning','fall', 'lucky','well','top','important','enough')
sentiments <- get_sentiments("bing") %>% filter(!word %in% dropword)

sentiment.neg<-tibble(word=c("bloom","redtide","discard","harder","smaller","small","decrease","decreased","fewer","overfished","lower","less","diminished","shark","sharks","dolphin","dolphins","release", 'net', 'gillnet', 'down'),sentiment=c("negative"))
sentiment.pos<-tibble(word=c("bite","large","larger","biting","increase","increased","plenty","uptick","aggressive","no_shortage",'more','many'),sentiment=c("positive"))
additional_sentiment <- sentiment.neg%>%
  rbind(sentiment.pos)
new_sentiment <- sentiments%>%
  rbind(additional_sentiment)

#Different combos of "envious" categorized as both positive and negative
# (dupcheck <- count(new_sentiment,word) %>% 
#   filter(n>1))
# dupcomp <- new_sentiment %>% 
#   filter(word %in% dupcheck$word)


###run a loop that looks that scores each comment by sentiment
for(i in 1:length(rg)){
  #for(i in 1:2){
  tokens.i <- tibble(text = rg[i]) %>% unnest_tokens(word, text)
  x <- inner_join(tokens.i, new_sentiment)
  POS <- nrow(subset(x, sentiment=="positive"))
  NEG <- nrow(subset(x, sentiment=="negative"))
  SENT <- POS - NEG
  df <- data.frame(positive=POS, negative=NEG, sentiment=SENT)
  df$standardized <- df$sentiment/nrow(x) ###standardize for number of words
  df$words <- nrow(x)
  out <- rbind(out, df)
}


###organize sentiment category into groups (-1 to -.33 neg, -.33 to .33 neutral, .33 to 1 pos)
out2 <- na.omit(out)
out2$standardized <- as.numeric(as.character(out2$standardized))
out2$lab <- cut(out2$standardized, breaks = c(-1,-.33,.33,1), include.lowest=TRUE)
out2$type <- with(out2, ifelse(standardized < -.33, '-1',
                               ifelse(standardized > .33, '1', '0')))
colors <- c("#fc8d59", "#ffffbf", "#99d594")
out2 %>% 
  group_by(lab) %>%
  summarise(no_rows = length(lab))

out$type <- with(out, ifelse(standardized < -.33, '-1',
                             ifelse(standardized > .33, '1', '0')))

###write out the database for John to use for mapping
#write.csv(out,"Gag_john.csv")       



### run sentiment analysis to examine each word
rg2<-tibble(line=1:length(rg), text=rg)
rg3<-rg2%>%unnest_tokens(word,text)

overall_word_counts <- rg3 %>%
  count(word, sort = TRUE) %>%
  ungroup()

bing_word_counts <- rg3 %>%
  inner_join(new_sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#bing_word_counts <- gsub("no_shortage", "no shortage", bing_word_counts, ignore.case = TRUE)
###This creates the word sentiment bar chart
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  scale_fill_manual(values=c("#fc8d59","#99d594"))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +theme(axis.title=element_text(size=14),axis.text=element_text(size=12))+theme(strip.text=element_text(size=14))+
  scale_y_continuous()+
  coord_flip()

top_words <- bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) 
barchart <- ggplot(top_words, aes(word, n, fill = sentiment)) +
  scale_fill_manual(values=c("#fc8d59","#99d594"))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +theme(axis.title=element_text(size=14),axis.text=element_text(size=12))+theme(strip.text=element_text(size=14))+
  scale_y_continuous()+
  coord_flip()
print(barchart)

ggsave("Word Plots/Most Frequent Bar Abundance.png", 
       plot = barchart,
       width = 8, #Adjust width as needed
       height = 8, #Adjust height as needed
       units = "in", 
       dpi = 700)


###This creates the sentiment word cloud
png("Word Plots/Most Frequent Cloud Abundance.png",
    width = 4, #Adjust width as needed
    height = 4, #Adjust height as needed
    units = "in", 
    res = 700)
par(mar = c(0, 0, 0, 0))
rg3 %>%
  inner_join(new_sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#fc8d59","#99d594"), 
                   max.words = 50, title.size=1.5,match.color=TRUE)
dev.off()

par(mar = c(0, 0, 0, 0))
rg3 %>%
  inner_join(new_sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#fc8d59","#99d594"), 
                   max.words = 50, title.size=1.5,match.color=TRUE)

