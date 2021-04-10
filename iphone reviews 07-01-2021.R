
#installing packages
install.packages("tidytext")
install.packages("tidyverse")
install.packages("rvest")
install.packages("dplyr")
install.packages("readr")
install.packages("stringr")
install.packages("forcats")

#Loading libraries
library(tidytext)
library(tidyverse)
library(rvest)
library(readr)
library(forcats)

#start with an empty data frame
iphone_reviews <- data.frame() 
#colocar nomes para as colunas (text e rate) e a primeira linha vazia

# base url has everything by the last number, which indicates page number
base_url <- "https://www.amazon.com/Apple-iPhone-XR-Fully-Unlocked/product-reviews/B07P6Y8L3F/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews&pageNumber="

# create a for loop for page number
for (i in 1:200){
  # add base url to page number
  url <- paste0(base_url, i)
  
  # read in html file
  amazon_reviews <- read_html(url)
  
  # get nodes
  review_text <- amazon_reviews %>%
    html_nodes(".review-text-content") %>%
    html_text()
  
  review_rate <- amazon_reviews %>%
    html_nodes(".a-icon-alt") %>%
    html_text() %>%
    tail(10)
 
  # bind rows of this page data frame with the rest of the data
  iphone_reviews = bind_rows(iphone_reviews, #rbind()
                              data.frame(text = review_text,
                                         rate = review_rate)) %>%
    
    # dar espaço entre ponto e palavra e apagar espaços extras 
    mutate(text = text,
    #testar cada um dos str_replace_all para ver qual está dando erro
    str_replace_all(pattern = "\\.", replacement = "\\. ") %>% #gsub()
    str_replace_all (pattern = " +", replacement = " "))
}


#looking at the data
head(iphone_reviews)
tail(iphone_reviews)

# replace \n with a space
iphone_reviews %>%
  mutate(text = gsub("\\n", " ", text)) %>%
  head()

# trim leading white space
iphone_reviews %>%
  mutate(text = gsub("\\n", "", text)) %>%
  mutate(text = trimws(text)) %>%
  head()

# looks good, overwrite data
iphone_reviews <- iphone_reviews %>%
  mutate(text = gsub("\\n", " ", text)) %>%
  mutate(text = trimws(text))

# replace "out of 5 stars" with nothing
iphone_reviews %>%
  mutate(rate = sub("out of 5 stars", "", rate)) %>%
  select(rate) %>%
  head()

# looks good, overwrite data
iphone_reviews <- iphone_reviews %>%
  mutate(rate = sub("out of 5 stars", "", rate))

# inspect data
glimpse(iphone_reviews)

# mutate rate to be a number
iphone_reviews <- iphone_reviews %>%
  mutate(rate = parse_number(rate))

# inspect data
glimpse(iphone_reviews)

mean(iphone_reviews$rate)

# add new review_id column
iphone_reviews <- iphone_reviews %>%
  mutate(review_id = row_number())

# tokenize words
iphone_reviews_tokenized <- iphone_reviews %>%
  unnest_tokens(word, text)

# inspect data
iphone_reviews_tokenized %>%
  head()
str(iphone_reviews_tokenized)

#--------

######## ANOTAÇÃO POS
#baseado em <https://stackoverflow.com/questions/50325625/extracting-the-pos-tags-in-r-using>
#referência: https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-annotation.html#Remarks

#carregando o pacote udpipe
install.packages("udpipe")
library(udpipe)

#download the model
en <- udpipe_download_model(language = "english")

#importando os dados
tokenized = iphone_reviews_tokenized[,3] %>%
  as.character()

#buscando os modelos para inglês
## Either give a file in the current working directory
udmodel = udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")

#anotando os dados
x = udpipe_annotate(udmodel, tokenized)

x = as.data.frame(x) %>%
  select(-c("doc_id", "paragraph_id", "sentence_id",
            "sentence", "token_id"))
y = iphone_reviews_tokenized

textPOS = bind_cols(iphone_reviews_tokenized %>% select, x[1:5857,]) #palavras como isn't é =1 em y mas =2 em x

## ver a quantidade de tokens em cada classe de palavra
table(x$upos)

#exportar df com POS em excel
install.packages("readxl")
library(readxl)

write.table(textPOS, "textPOS.xlsx")

# Df com adverbios
adverbs = textPOS[textPOS$upos %>% str_detect("ADV"),] %>%
  group_by(token) %>% mutate(count=n()) %>% ungroup() %>% arrange(desc(count)) %>%
  group_by(token, rate) %>% mutate(count_rate=n()) %>% ungroup() %>%
  mutate(token =  fct_infreq(token))

# quantos advérbios diferentes
adv_df <- (adverbs %>% group_by(token) %>% summarize(n()))

# Df com os adverbios e as avaliações do produto
reviews_adverbs <- iphone_reviews %>% filter(review_id %in% adverbs$review_id)

# grafico de advérbios por avaliação do produto
starAdvPlot = ggplot(adverbs %>% filter (count>20),
       aes(x=token, fill= as.factor(rate)))+
  geom_bar(position = "fill")+ ## ou grafco de barras geom_bar()+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
starAdvPlot = starAdvPlot + guides(fill=guide_legend(title="star reviews"))

#marcando 11 ngrams para análise sentencial
iphone_reviews_11grams <- iphone_reviews %>%
  mutate(text = paste("<PRE> <PRE> <PRE> <PRE> <PRE> <PRE>", text, 
                      "<POS> <POS> <POS> <POS> <POS> <POS>")) %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 11) %>% 
  separate(ngram, remove = F, sep = " ",
           into = c("w1","w2","w3","w4","w5","w6",
                    "w7","w8","w9","w10","w11")) %>%
  select(rate, review_id, ngram, word=w6)

#ordenando por frequência
adverbs_adjacents <-iphone_reviews_11grams[
  iphone_reviews_11grams$word %>% str_detect("ly$"),
] %>%
  mutate(word =  fct_infreq(word))

#Colocando os advérbios e suas frases lado a lado
adverbs_and_adjacents <- cbind(
  adverbs %>% arrange(review_id, word),
  adverbs_adjacents %>% arrange(review_id, word) %>%
    select(-word,-review_id,-rate)) %>% 
  arrange(word) #### Ordenar/"agrupar" por palavra(adverbio)


# tokenize bigrams
iphone_reviews_trigrams <- iphone_reviews %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 3)

# inspect data
iphone_reviews_trigrams %>%
  head()

# check stop_words data frame
stop_words %>%
  count(word) %>%
  arrange(-n)

# the smallest lexicon is snowball
stop_words %>%
  count(lexicon)

#filter the stop words to keep only words from the snowball lexicon.
my_stop_words <- stop_words %>%
  filter(lexicon == "snowball")

# remove stop words from iphone reviews tokenized
iphone_reviews_clean <- iphone_reviews_tokenized %>%
  anti_join(my_stop_words)

# most frequent tokens per rate
iphone_reviews_clean %>%
  count(word, rate) %>%
  arrange(-n)

#plotting the most frequent words per rate (unbalanced)
iphone_reviews_clean %>%
  count(word, rate) %>%
  group_by(rate) %>%
  top_n(10) %>%
  ggplot(aes(x = n, 
             y = reorder_within(word, n, rate))) +
  geom_col() +
  facet_wrap(~rate, scales = "free_y") +
  scale_y_reordered() +
  labs(y = "")

# count number of tokens (i.e., rows) per rate
iphone_reviews_clean %>%
  count(rate)

# get total word count per rate, rename n so it's total instead
rate_word_count <- iphone_reviews_clean %>%
  count(rate) %>%
  rename(total = n)

# get count for each word per rate
word_count_per_rate <- iphone_reviews_clean %>%
  count(word, rate)

# normalize individual word count
# first merge two counts
word_count_normalized <- left_join(word_count_per_rate,
                                   rate_word_count)

# create a new normalized count column, with n divided by total
word_count_normalized <- word_count_normalized %>%
  mutate(norm_n = (n/total)*1000)

#Plotting it again, by normalized frequency instead
word_count_normalized %>%
  group_by(rate) %>%
  top_n(10) %>%
  ggplot(aes(x = norm_n, 
             y = reorder_within(word, norm_n, rate))) +
  geom_col() +
  facet_wrap(~rate, scales = "free_y") +
  scale_y_reordered() +
  labs(y = "")

###We can calculate term frequency inverse document frequency (tf-idf) 
###instead of normalized frequency. The goal in using tf-idf is to decrease 
###the weight for commonly used words (i.e., words used across all documents) 
###and increase the weight for words that are less frequent in 
###other documents in that collection.

# calculate tf-idf based on n, providing the word column and the category col
word_tf_idf <- word_count_normalized  %>%
  bind_tf_idf(word, rate, n)

# inspect data
word_tf_idf %>%
  head()

#We can also add range, 
#to decide what words to keep and understand tf-idf a little better.

# calculate range per word
word_range <- iphone_reviews_clean %>%
  distinct(word, review_id) %>%
  count(word) %>%
  rename(range = n)

# add range to data frame with left_join
word_tf_idf <- left_join(word_tf_idf, word_range)

# inspect data
word_tf_idf %>%
  head()

# what's the mean range?
mean(word_tf_idf$range)

#Plotting it again, by tf-idf filtering by range.
word_tf_idf %>%
  filter(range > 20) %>%
  group_by(rate) %>%
  top_n(n = 10, wt = tf_idf) %>%
  ggplot(aes(x = tf_idf, 
             y = reorder_within(word, tf_idf, rate))) +
  geom_col() +
  facet_wrap(~rate, scales = "free_y") +
  scale_y_reordered() +
  labs(y = "")

###Sentiment analysis
# tidyverse already has a sentiment classification - first 10 rows
sentiments %>%
  head(10)

# how many total words
sentiments %>%
  nrow()

# how many for each sentiment
sentiments %>%
  count(sentiment)

###We can use the function get_sentiments() to get the sentiments for 
###different lexicons (these lexicons are available under different 
###licenses, be sure that the license for the lexicon you want 
###to use is appropriate for your project).

#loading lexicon packages
install.packages("textdata")
library(textdata)

# AFINN from Finn Årup Nielsen (http://www2.imm.dtu.dk/pubdb/pubs/6010-full.html)
get_sentiments("afinn")

# bing from Bing Liu and collaborators (https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html)
get_sentiments("bing")


##Adding Sentiment Info to our Data

# start with our clean tokenized data
# use inner_join to add sentiment from bing lexicon
iphone_reviews_sentiment <- iphone_reviews_clean %>%
  inner_join(sentiments)

# inspect data
iphone_reviews_sentiment %>%
  count(rate, sentiment) %>%
  arrange(n)

# count tokens in each sentiment
iphone_reviews_sentiment %>%
  count(sentiment)

###Since we have a rating for each review, we can calculate the 
###mean and standard deviation of these ratings per sentiment. 
###The hypothesis here is that positive words will have a 
###higher mean rating than negative words.

# mean rate of each sentiment
iphone_reviews_sentiment %>%
  group_by(sentiment) %>%
  summarise(mean_rate = mean(rate),
            sd_rate = sd(rate))

# regression of sentiment by rate
iphone_reviews_sentiment %>%
  mutate(dep_var = ifelse(sentiment == "negative", 0, 1)) %>%
  lm(formula = dep_var ~ rate) %>%
  summary()

###We can also recalculate our tf-idf based on our two new categories.

# first we count instances of each word per sentiment
word_sentiment_count <- iphone_reviews_sentiment %>%
  count(word, sentiment) 

# calculate tf-idf based on n, providing the word column and the category 
# column, which is sentiment this time around
sentiment_tf_idf <- word_sentiment_count  %>%
  bind_tf_idf(word, sentiment, n)

# we can add range here too, which we already calculated 
sentiment_tf_idf <- left_join(sentiment_tf_idf, word_range)

# inspect data
sentiment_tf_idf %>%
  head()

#Plotting it by tf-idf filtering by range across different sentiments.
sentiment_tf_idf %>%
  group_by(sentiment) %>%
  top_n(n = 10, wt = tf_idf) %>%
  mutate(tf_idf = ifelse(sentiment == "negative",
                         -tf_idf,
                         tf_idf)) %>%
  ggplot(aes(x = tf_idf, 
             y = reorder(word, tf_idf),
             fill = sentiment)) +
  geom_col() +
  geom_label(aes(label = range), show.legend = FALSE) +
  labs(y = "") +
  ggtitle("Top 10 most important words per sentiment",
          subtitle = "labels show range") +
  theme_bw()



#caso tiver que usar o antconc
writeLines(iphone_reviews[,1], "iphone_reviews.txt")








