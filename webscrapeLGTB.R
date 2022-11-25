
library(rvest)
library(purrr)
library(wordcloud)
library(dplyr)
library(plyr)

url_base<-"https://outstanding.involverolemodels.org/poll/2021-top-100-lgbt-future-leaders/?location=united-kingdom"


HTML <- read_html(url_base)

name_html <- html_nodes(HTML,'.entry-header h4')
name <- html_text(name_html)
name1<-data.frame(name)

des_html <- html_nodes(HTML,'.entry-content h5')
des <- html_text(des_html)
des1<-data.frame(des)

org_html <- html_nodes(HTML,'.entry-content h6')
org <- html_text(org_html)
org1<-data.frame(org)


future_leaders<-cbind(name1,des1,org1)

write.csv(future_leaders,'future_leaders.csv')


url_base<-"https://outstanding.involverolemodels.org/poll/2021-top-100-lgbt-executives/?location=united-kingdom"

HTML <- read_html(url_base)

name_html <- html_nodes(HTML,'.entry-header h4')
name <- html_text(name_html)
name1<-data.frame(name)

des_html <- html_nodes(HTML,'.entry-content h5')
des <- html_text(des_html)
des1<-data.frame(des)

org_html <- html_nodes(HTML,'.entry-content h6')
org <- html_text(org_html)
org1<-data.frame(org)

exes<-cbind(name1,des1,org1)

write.csv(exes,'exes.csv')


url_base<-"https://outstanding.involverolemodels.org/poll/2019-top-30-lgbt-public-sector-executives/?location=united-kingdom"

HTML <- read_html(url_base)

name_html <- html_nodes(HTML,'.entry-header h4')
name <- html_text(name_html)
name1<-data.frame(name)

des_html <- html_nodes(HTML,'.entry-content h5')
des <- html_text(des_html)
des1<-data.frame(des)

org_html <- html_nodes(HTML,'.entry-content h6')
org <- html_text(org_html)
org1<-data.frame(org)

pse<-cbind(name1,des1,org1)

write.csv(pse,'pse.csv')


url_base<-"https://outstanding.involverolemodels.org/poll/2021-top-50-ally-executives/?location=united-kingdom"

HTML <- read_html(url_base)

name_html <- html_nodes(HTML,'.entry-header h4')
name <- html_text(name_html)
name1<-data.frame(name)

des_html <- html_nodes(HTML,'.entry-content h5')
des <- html_text(des_html)
des1<-data.frame(des)

org_html <- html_nodes(HTML,'.entry-content h6')
org <- html_text(org_html)
org1<-data.frame(org)

ally<-cbind(name1,des1,org1)

write.csv(ally,'ally.csv')



