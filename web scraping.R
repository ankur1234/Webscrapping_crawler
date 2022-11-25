
library(rvest)
library(purrr)
library(wordcloud)
library(dplyr)
library(plyr)

url_base<-"http://aviation-safety.net/database/dblist.php?Year=1972&lang=&page=%d"

map(1:4, function(i) {
  
  pg <- read_html(sprintf(url_base, i))
  
  pg %>% html_table()

  
}) -> results


df <- ldply (results, data.frame)






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
  
  pg <- read_html(sprintf(url_base, i))
  
  data.frame(benefit=html_text(html_nodes(pg,".wrapToggleStr")),
             date=html_text(html_nodes(pg,".date")),
             stringsAsFactors = FALSE)
  
}) -> benefits  ##jobs dataframe