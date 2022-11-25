

page<- 1:3
urls <- paste0("http://aviation-safety.net/database/dblist.php?Year=2014&lang=&page=", page)

get_table <- function(url) {
  url %>%
    read_html() %>%
    #html_nodes('.list') %>% 
    html_table()
}

results <- lapply(urls, get_table) ### can use map()

library(plyr)
df <- ldply (resultfor (pages in 1:page){
  url <- paste0(url_year,"&lang=&page=", pages)
  data<-url %>% read_html() %>% html_table()
}s, data.frame)


year<-2000:2019
url_year<-paste0("http://aviation-safety.net/database/dblist.php?Year=",year)

pages <- function(url) {
  url %>%
    read_html() %>% html_text()
}

pages_result<-lapply(url_year,pages)

pages<-ldply(pages_result,data.frame)


base_url<-"http://aviation-safety.net/database/"

models <- data_frame(model = c(2000:2019),link = paste0(base_url, model),page = map(link, read_html))

model_specs <- models %>% map(page, html_table, fill=TRUE) 


# set-up of initial values
startyear <- 1960
endyear <- 1965
url_init <- "http://aviation-safety.net/database/dblist.php?Year="

# initiate empty dataframe, in which we will store the data
dat <- data.frame(date = numeric(0), type = numeric(0), registration = numeric(0),
                  operator = numeric(0), fatalities = numeric(0),
                  location = numeric(0), category = numeric(0))

for (year in startyear:endyear){
  # get url for this year
  url_year <- paste0(url_init, year)
  
  # get pages
  pages <- url_year %>% html() %>%
    html_nodes(xpath = '//*[@id="contentcolumnfull"]/div/div[2]') %>%
    html_text() %>% strsplit(" ") %>% unlist() %>%
    as.numeric() %>% max()
  
  # loop through the pages
  for (page in 1:pages){
    url <- paste0(url_year,"&lang=&page=", page)
    
    # get the html data and convert it to a data.frame
    incidents <- url %>% html() %>%
      html_nodes(xpath = '//*[@id="contentcolumnfull"]/div/table') %>%
      html_table() %>% data.frame()
    
    # combine the data
    dat <- rbind(dat, incidents)
  }
}
  

