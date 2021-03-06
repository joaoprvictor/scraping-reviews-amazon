
# Df com adverbios
adverbs <- iphone_reviews_tokenized[iphone_reviews_tokenized$word %>% str_detect("ly$"),] %>%
  group_by(word) %>% mutate(count=n()) %>% ungroup() %>% arrange(desc(count)) %>%
  group_by(word,rate) %>% mutate(count_rate=n()) %>% ungroup() %>%
  mutate(word =  fct_infreq(word))
  
# Df com os adverbios e as avalia��es do produto
reviews_adverbs <- iphone_reviews %>% filter(review_id %in% adverbs$review_id)

# grafico de adv�rbios por avalia��o do produto
ggplot(adverbs %>% filter (count>10),
       aes(x=word, fill= as.factor(rate)))+
  geom_bar(position = "fill")+ # ou geom_bar()+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.position="none")

#marcando 11 ngrams para an�lise sentencial
iphone_reviews_11grams <- iphone_reviews %>%
  mutate(text = paste("<PRE> <PRE> <PRE> <PRE> <PRE> <PRE>", text, 
                      "<POS> <POS> <POS> <POS> <POS> <POS>")) %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 11) %>% 
  separate(ngram, remove = F, sep = " ",
           into = c("w1","w2","w3","w4","w5","w6",
                    "w7","w8","w9","w10","w11")) %>%
  select(rate, review_id, ngram, word=w6)

#ordenando por frequ�ncia
adverbs_adjacents <-iphone_reviews_11grams[
  iphone_reviews_11grams$word %>% str_detect("ly$"),
  ] %>%
  mutate(word =  fct_infreq(word))

#Colocando os adv�rbios e suas frases lado a lado
adverbs_and_adjacents <- cbind(
  adverbs %>% arrange(review_id, word),
  adverbs_adjacents %>% arrange(review_id, word) %>%
    select(-word,-review_id,-rate)) %>% 
  arrange(word) #### Ordenar/"agrupar" por palavra(adverbio)


# quantos adv�rbios diferentes
adv_df <- (adverbs %>% group_by(word) %>% summarize(n()))


# ggplot(adverbios %>% filter (count>30), aes(x=word, y= rate))+
#   geom_violin( alpha=0.1)+
#   geom_jitter(aes(color = as.factor(rate)),alpha=0.5)+
#   theme(axis.text.x  = element_text(angle=90),legend.position="none")
