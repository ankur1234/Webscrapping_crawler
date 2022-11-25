

library(rvest)
library(purrr)
library(wordcloud)

##Crawl over reviews page by page and extract the comments ####

url_base<-"https://www.glassdoor.co.in/Reviews/StarHub-Reviews-E8662_P%d.htm"

map_df(1:5, function(i) {
  
  pg <- read_html(sprintf(url_base, i))
  
  data.frame(text=html_text(html_nodes(pg,"p")))
             
}) -> reviews  ##reviews dataframe

reviews<-reviews[5:362,]

write.csv(reviews,'reviews.csv')

##Jobs data####
###############

###Job title

url_base<-"https://www.glassdoor.co.in/Jobs/StarHub-Jobs-E8662_P%d.htm"

map_df(1:2, function(i) {
  
  pg <- read_html(sprintf(url_base, i))
  
  data.frame(role=html_text(html_nodes(pg,".jobLink")))

}) -> jobs  ##jobs dataframe

##Job location

url_base<-"https://www.glassdoor.co.in/Jobs/StarHub-Jobs-E8662_P%d.htm"

map_df(1:2, function(i) {
  
  pg <- read_html(sprintf(url_base, i))
  
  data.frame(location=html_text(html_nodes(pg,".loc")))
  
}) -> jobs_location  ##jobs_location dataframe



###Interview questions###

url_base<-"https://www.glassdoor.co.in/Interview/StarHub-Interview-Questions-E8662_P%d.htm"

map_df(1:5, function(i) {
  
  pg <- read_html(sprintf(url_base, i))
  
  data.frame(interview=html_text(html_nodes(pg,".wrapToggleStr")))
  #location=html_text(html_nodes(pg,".loc")),stringsAsFactors = FALSE
  
}) -> interview  ##interview dataframe


url_base<-"https://www.glassdoor.co.in/Benefits/StarHub-Singapore-Benefits-EI_IE8662.0,7_IL.8,17_IN217_IP%d.htm"

map_df(1:5, function(i) {
  
  cat(".")
  
  pg <- read_html(sprintf(url_base, i))
  
  data.frame(benefit=html_text(html_nodes(pg,".wrapToggleStr")),
  date=html_text(html_nodes(pg,".date")),
  stringsAsFactors = FALSE)
  
}) -> benefits  ##jobs dataframe


##Safely() for error pages###

check<-safely(pg)


###Analysis###
#Jobs

loc_num<-data.frame(table(jobs_location))
loc_num<-loc_num[2:4,]
loc_num<-loc_num[order(-loc_num$Freq),]


##Get unique job roles
jobs_title<-data.frame(unique(jobs$role))
jobs_title<-jobs_title[2:52,]
d<-data.frame(jobs_title)

##Set some random numbers for word cloud
d$freq<- sample(51, size = nrow(d), replace = TRUE)

#Create Deloitte colour palette
deloitte_colours <- c("#53565A","#C4D600","#62B5E5","#43B028","#00A3E0","#63666A","#86BC25")


#Make wordcloud
#--------------
wordcloud_name <- paste("wordcloud_jobstitle",".png",sep="")
png(wordcloud_name, width=4000,height=2000)
wordcloud(as.character(d$jobs_title), 
          d$freq, scale=c(6,1),min.freq=1,max.words=200, 
          random.order=FALSE, rot.per=0, colors=deloitte_colours)
dev.off()


##BENEFITS###

# --------
# PACKAGES
# --------
# List of packages for session
packages <- c("tau", "tm","RColorBrewer", "portfolio", 
              "lsa","plyr","ggplot2", "XML",
              "wordcloud", "stringr","SnowballC")

# Load packages into session 
sapply(packages, require, character.only = TRUE)

corpus <- VCorpus(VectorSource(benefits$benefit))

#Pre-processing
my_corpus <- tm_map(corpus, content_transformer(tolower))
my_corpus <- tm_map(my_corpus, removeNumbers)
stopwords_custom<-c(stopwords("SMART"),"aws","al","benefit","benefits")
my_corpus <- tm_map(my_corpus, removeWords,stopwords_custom) 
#spc<-function(x) gsub("'", "", x)
#my_corpus<-tm_map(my_corpus,spc)
my_corpus <- tm_map(my_corpus, stripWhitespace)
my_corpus <- tm_map(my_corpus, removePunctuation)
#my_corpus <- tm_map(my_corpus, stemDocument,language="english")
#my_corpus <- tm_map(my_corpus, PlainTextDocument) ##Plaintext throws error


##Extract three words
NLPtrigramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}

tdm_NLP <- TermDocumentMatrix(my_corpus, control=list(tokenize = NLPtrigramTokenizer))
m <- as.matrix(tdm_NLP)
v <- sort(rowSums(m),decreasing=TRUE)
d_3 <- data.frame(word = names(v),freq=v)
d<-data.frame(d_3)
rownames(d)<-NULL
d<-data.frame(d[1:50,])

##Change the frequency manually for a better visualisation
d[1,2]<-20
d[2,2]<-17
d[3,2]<-15
d[4,2]<-12
d[5,2]<-10
d[6,2]<-7
d[7,2]<-5
d[8,2]<-5
d[9,2]<-5
d[10,2]<-5
d[11,2]<-5
d[12,2]<-5
d[13,2]<-5



#Make wordcloud
#--------------
wordcloud_name <- paste("wordcloud_benefits",".png",sep="")
png(wordcloud_name, width=4000,height=2000)
wordcloud(as.character(d$word), 
          d$freq, scale=c(6,1),min.freq=1,max.words=200, 
          random.order=FALSE, rot.per=0, colors=deloitte_colours)
dev.off()


##Interview questions###

corpus <- VCorpus(VectorSource(interview$interview))

#Pre-processing
my_corpus <- tm_map(corpus, content_transformer(tolower))
my_corpus <- tm_map(my_corpus, removeNumbers)
stopwords_custom<-c(stopwords("SMART"),"interview","online","singapore","weeks","week","day","applied","starhub","interview","interviewed","answer","question","questions","asked","august","january","october","april","years","time","december","abt","june")
my_corpus <- tm_map(my_corpus, removeWords,stopwords_custom) 
#spc<-function(x) gsub("'", "", x)
#my_corpus<-tm_map(my_corpus,spc)
my_corpus <- tm_map(my_corpus, stripWhitespace)
my_corpus <- tm_map(my_corpus, removePunctuation)
#my_corpus <- tm_map(my_corpus, stemDocument,language="english")
#my_corpus <- tm_map(my_corpus, PlainTextDocument) ##Plaintext throws error


##Extract three words
NLPtrigramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

tdm_NLP <- TermDocumentMatrix(my_corpus, control=list(tokenize = NLPtrigramTokenizer))
m <- as.matrix(tdm_NLP)
v <- sort(rowSums(m),decreasing=TRUE)
d_2 <- data.frame(word = names(v),freq=v)
d<-data.frame(d_2)
rownames(d)<-NULL
d<-data.frame(d[1:50,])

#Make wordcloud
#--------------
wordcloud_name <- paste("wordcloud_interview",".png",sep="")
png(wordcloud_name, width=4000,height=2000)
wordcloud(as.character(d$word), 
          d$freq, scale=c(6,1),min.freq=1,max.words=200, 
          random.order=FALSE, rot.per=0, colors=deloitte_colours)
dev.off()


###Reviews####
library(stringr)
reviews$pos<-data.frame(str_count(reviews$value,"Pros"))
reviews$neg<-data.frame(str_count(reviews$value,"Cons"))
colnames(reviews)<-c("reviews","pos","neg")
reviews$neg<-ifelse(reviews$neg==1,2,0)
reviews$code<-ifelse(reviews$neg==2,2,ifelse(reviews$pos==1,1,0))
reviews<-reviews[,c(1,4)]
write.csv(reviews,'reviews.csv')

##Saved the file and used Fill down from power query in excel - R substitute needed####
##Load the filled in new sheet again and then run the below code

pos<-data.frame(reviews$reviews[reviews$code==1])
neg<-data.frame(reviews$reviews[reviews$code==2])

corpus <- VCorpus(VectorSource(pos$reviews.reviews.reviews.code....1.))

#Pre-processing
my_corpus <- tm_map(corpus, content_transformer(tolower))
my_corpus <- tm_map(my_corpus, removeNumbers)
stopwords_custom<-c(stopwords("SMART"),"Pros","abv","balance")
my_corpus <- tm_map(my_corpus, removeWords,stopwords_custom) 
#spc<-function(x) gsub("'", "", x)
#my_corpus<-tm_map(my_corpus,spc)
my_corpus <- tm_map(my_corpus, stripWhitespace)
my_corpus <- tm_map(my_corpus, removePunctuation)
#my_corpus <- tm_map(my_corpus, stemDocument,language="english")
#my_corpus <- tm_map(my_corpus, PlainTextDocument) ##Plaintext throws error


##Extract two words
NLPtrigramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

tdm_NLP <- TermDocumentMatrix(my_corpus, control=list(tokenize = NLPtrigramTokenizer))
m <- as.matrix(tdm_NLP)
v <- sort(rowSums(m),decreasing=TRUE)
d_2 <- data.frame(word = names(v),freq=v)
d<-data.frame(d_2)
rownames(d)<-NULL
d<-data.frame(d[1:30,])

#Make wordcloud
#--------------
wordcloud_name <- paste("wordcloud_pos",".png",sep="")
png(wordcloud_name, width=4000,height=2000)
wordcloud(as.character(d$word), 
          d$freq, scale=c(6,1),min.freq=1,max.words=200, 
          random.order=FALSE, rot.per=0, colors=deloitte_colours)
dev.off()


##Negative comments
###################
corpus <- VCorpus(VectorSource(neg$reviews.reviews.reviews.code....2.))

#Pre-processing
my_corpus <- tm_map(corpus, content_transformer(tolower))
my_corpus <- tm_map(my_corpus, removeNumbers)
stopwords_custom<-c(stopwords("SMART"),"cons","starhub","fulltime","glassdoor","copyright","cookie","enabled","review","ratings","logo","trademarks","working","years","reviews","change","clouds","clue","clear","roadmap")
my_corpus <- tm_map(my_corpus, removeWords,stopwords_custom) 
#spc<-function(x) gsub("'", "", x)
#my_corpus<-tm_map(my_corpus,spc)
my_corpus <- tm_map(my_corpus, stripWhitespace)
my_corpus <- tm_map(my_corpus, removePunctuation)
#my_corpus <- tm_map(my_corpus, stemDocument,language="english")
#my_corpus <- tm_map(my_corpus, PlainTextDocument) ##Plaintext throws error


##Extract one words
NLPtrigramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
}

tdm_NLP <- TermDocumentMatrix(my_corpus, control=list(tokenize = NLPtrigramTokenizer))
m <- as.matrix(tdm_NLP)
v <- sort(rowSums(m),decreasing=TRUE)
d_3 <- data.frame(word = names(v),freq=v)
d<-data.frame(d_3)
rownames(d)<-NULL
d<-data.frame(d[43:80,])
d$freq<- sample(38, size = nrow(d), replace = TRUE)

#Make wordcloud
#--------------
wordcloud_name <- paste("wordcloud_neg",".png",sep="")
png(wordcloud_name, width=4000,height=2000)
wordcloud(as.character(d$word), 
          d$freq, scale=c(6,1),min.freq=1,max.words=200, 
          random.order=FALSE, rot.per=0, colors=deloitte_colours)
dev.off()





###DO NOT RUN#####


library(rvest)
library(purrr)

url_base<-"https://www.glassdoor.co.in/Reviews/StarHub-Reviews-E8662_P%d.htm"

map_df(2:643, function(i) {
  
  pg <- read_html(sprintf(url_base, i))

  data.frame(pros=html_text(html_nodes(pg,".label")),
             positive=html_text(html_nodes(pg,"span")),
             cons=html_text(html_nodes(pg,".label")),
             negatives=html_text(html_nodes(pg,"span")))

}) -> reviews




###NOT ABLE TO GET DESIRED RESULTS #####
################################

library("processx")
library("Rcrawler")
url<-"https://www.glassdoor.co.in/Overview/Working-at-StarHub-EI_IE8662.11,18.htm"
Rcrawler(Website = url, no_cores = 1, no_conn = 1)


Data<-ContentScraper(Url = "https://www.glassdoor.co.in/Reviews/StarHub-Reviews-E8662.htm", CssPatterns = c("span",".undecorated")) 

Data<-ContentScraper(Url = "https://www.glassdoor.co.in/Reviews/StarHub-Reviews-E8662.htm",  XpathPatterns = c("//span | //a")) 


Rcrawler(Website = "http://forum.zebulon.fr/forums-de-zebulonfr-f30.html",no_cores = 2, no_conn = 2,  ExtractCSSPat = c(".ipsType_pagetitle",".entry-content"), ManyPerPattern = TRUE,  ignoreUrlParams =c("view","orderby"))


df<-data.frame(do.call("rbind", DATA))



corpus %>% tokens(ngrams = 1:3) %>% # generate tokens
  dfm %>% # generate dfm
  convert(to = "tm") %>% # convert to tm's document-term-matrix
  t # transpose it to term-document-matrix

