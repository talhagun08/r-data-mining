install.packages("tuber")
install.packages("httr")
install.packages("httpuv")
install.packages("dplyr")
install.packages("tidytext")
install.packages("wordcloud2")
install.packages("stopwords")
install.packages("stringr")
install.packages("stringi")
install.packages("textdata")
install.packages("tidyverse")
install.packages("sentimentr")
install.packages("ggthemes")
install.packages("ggpubr")
install.packages("ggstance")
install.packages("pander")
install.packages("pastecs")
install.packages("DT")
install.packages("wordcloud")
install.packages("RColorBrewer")


library(RColorBrewer)
library(tuber)
library(httr)
library(httpuv)
library(dplyr)
library(tidytext)
library(wordcloud2)
library(stopwords)
library(stringr)
library(stringi)
library(textdata)
library(tidyverse)
library(ggplot2)
library(sentimentr)
library(ggthemes)
library(ggstance)
library(pander)
library(pastecs)
library(syuzhet)
library(DT)
library(wordcloud)


setwd("C:\\Users\\LENOVO-\\Desktop\\R F??NAL PROJES??")

clientid <- "my-key"
clientsecret <-"my-client-secret"

yt_oauth(clientid,clientsecret,token = " ")

yorum <- get_all_comments(video_id = "oZUot1er0F4")
write.csv(yorum, file="yorumlar.csv")


yorum <- read.csv("yorumlar.csv")
durakkelimeler <- readLines("stopwords.csv")
durakkelimeler <- data.frame(word = durakkelimeler)

yorum <- yorum %>%
  dplyr::select(X, textOriginal) %>%
  mutate(textCleaned = iconv(textOriginal, "latin1", "ASCII", sub = ""))


# Temizlik ??slemleri
yorum <- yorum %>%
  mutate(textCleaned = stri_trans_general(textCleaned, "latin-ascii"))

yorum <- yorum %>%
  mutate(textCleaned = str_replace_all(textCleaned, "[^[:alnum:] ]", ""))

yorum <- yorum %>%
  mutate(textCleaned = str_replace_all(textCleaned, "[\\x{1F1E6}-\\x{1F1FF}]+", ""))

yorum <- yorum %>%
  mutate(textCleaned = str_replace_all(textCleaned, "\\p{P}", ""))

yorum <- yorum %>%
  mutate(textCleaned = str_remove_all(textCleaned, "\\d+"))

yorum <- yorum %>%
  mutate(textCleaned = str_replace(textCleaned, "[??]", "i"))




# Tokenize text into words

kelimeler <- yorum %>%
  mutate(word = str_to_lower(textCleaned)) %>% 
  unnest_tokens(word, textCleaned)

kelimeler <- kelimeler %>% 
  as_tibble() %>% 
  rename(word = word)
  



# Remove stopwords
silinecek_kelimeler<-c(  "ufaeuffauffuffauffuffaufftrump","trumpufcufcufc","uffauffuffauffuffauffuffaufftrump","communitiesuatrump","jhostagestrump","trumppppp","trumptrumptrump","h??storytrump","trumpuffauffuffauffuffauffuf","hes","uffauff","??um","itus","nbc","rfk","doesnt","??ts", "dont", "may", "can","years","age","mean","diet", "qol","based","dog","two","per","model","will","is","aarc","results","label","among","are","level","cancer", "used","many", "nlm", "data", "study", "p", "n", "net", "biochar", "aloe", "ses", "systems","avs", "acid","rate","aha","year","time","since","stroke", "however", "p lt", "ssris", "score", "index","hia", "largest", "current", "ncds", "found", "labelobjective", "It","yet","models", "vera","total", "also", "ptsd", "lt", "sroi", "icdm", "scores", "new", "arm", "china", "sample", "semen", "survey", "labelconclusions","within","labelbacground","range","followed","tagsnps","mbovis","labeldesign","increased","published", "likely", "nlmcategorymethods", "acids", "fatty", "employersponsored","labelmethods", "using", "conclusions","setting", "whether","findings","crosssectional", "one", "three","review","associated", "analysis", "studies", "nlmcategory", "background", "nlmcategorymethods", "in", "of", "and", "as", "19", "the","have","were","an", "was", "during", "to", "for","a", "cth","or","hcs", "this", "u","ci", "t", "la", "043d", "064a","043b", "062a", "trastuzumab","ewars","research", "either", "ipps73","ippss","87,017", "ofips", "15,000", "eshre","tdabc","thaihealth", "0.157", "and", "analyzed", "methods", "terms", "subjects", "being", "there", "compared", "while",  "when", "their","these","between", "his","her", "which", "that","who", "after","related","higher","group", "other","each","those","lower", "number","rates","first", "about", "groups","through","including", "across", "should", "must", "have","months","adjusted", "analyses", "estimated", "could", "would", "without", "further", "versus", "cross", "method", "follow", "general", "under", "below", "estimate","following", "received", "available", "before", "aimed", "collected", "often", "often","include","relevant","having","therefore","because","thus", "gutenberg","almost", "license", "ebook", "anyone", "anywhere", "title", "author", "enough", "nothing", "cannot", "really", "never", "again", "wwwgutenbergorg", "whatsoever", "editor", "chapter","perhaps", "shall", "every","where", "been", "more", "than", "even", "with", "what",  "who", "when", "only", "your", "into", "in", "on", "always", "online", "copy", "away", "back", "myself","whose", "still")

kelimeler<-kelimeler %>% filter(!word %in% silinecek_kelimeler)


stopwords <- tidytext::stop_words

stopwords <- bind_rows(stopwords, durakkelimeler)

kelimeler <- kelimeler %>%
  anti_join(stopwords)



# Remove links
kelimeler <- kelimeler %>%
  filter(!grepl("http", word))  # Remove words containing "http"

kelimeler <- kelimeler %>%
  filter(nchar(word) > 2)  # Remove very short words






#kelimelerin frekans??n??n al??nd??g?? k??s??m burada kelimelerin frekans de??erlerini al??yorum ki wordcloud ve barplotlar d??zg??n ????ks??n
frekansli_kelime <- kelimeler %>%
  count(word, sort = TRUE)


wordcloud2(frekansli_kelime, size = 1, minSize = 3, gridSize =  0,
           fontFamily = 'Segoe UI', fontWeight = 'bold',
           color = 'random-dark', backgroundColor = "white",
           minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
           rotateRatio = 0.4, shape = 'cloud', ellipticity = 0.65,
           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)


#bing ile duygu tablosu
duygular <- frekansli_kelime %>%
  inner_join(get_sentiments("bing"))

duygular %>%
  group_by(sentiment) %>%
  summarise(n = mean(n)) %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  scale_fill_manual(values = c("positive" = "#00bfc4", "negative" = "#f8766d")) +
  xlab("Duygu") +
  ylab("Frekans")



top30_duygu <- duygular %>%
  group_by(sentiment) %>%
  top_n(30, wt = n)


ggplot(top30_duygu, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Kelime", x = "Frekans") +
  coord_flip() +
  theme_hc() +
  labs() +
  theme(plot.caption = element_text(hjust = 0, face = "italic"))



#polarite
polarite <- sentiment(kelimeler$word)
tablo <- cbind(kelimeler$word, polarite[,c(3,4)])

ggplot(tablo, aes(word_count, sentiment))+
  geom_point(color="blue")+
  geom_hline(yintercept = mean(tablo$sentiment), color="red", size=1)+
  labs(y = "Skor", x = "Frekans")+
  theme_igray()+
  theme(plot.caption = element_text(hjust = 0, face="italic"))

stat.desc(polarite$sentiment, basic = T) %>% pander()




                                                                                                                                                                                                        buttons = c('excel', "csv")))