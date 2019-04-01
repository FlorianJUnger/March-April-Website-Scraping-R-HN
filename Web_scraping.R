####
#### Web scraping HackerNews
#### Florian UNGER - March 2019
#### 

## Install packages 
libraries_function <- function(){
  if(require("pacman")=="FALSE"){
    install.packages("pacman")
    library("pacman")
    pacman::p_load(here, rvest, selectr, XML)
  } else {
    library("pacman")
    pacman::p_load(here, rvest, selectr, XML)
  }
}

webpage = read_html("https://news.ycombinator.com/show?p=1") #fill in the webpage by replacing 1 with 2

news_title <- c()
news_points <- c()
comments <- c()
links <- c()

## Title
news_title <- webpage %>% 
  html_nodes(".storylink") %>% # you need nodes not just node
  html_text()
news_title <- gsub("Show HN: ","",news_title)

## Points
news_points <- webpage %>% 
  html_nodes(".score") %>%
  html_text()
news_points <- gsub(" points", "", news_points)

## Comments
comments <- webpage %>%
  html_nodes(".age~ a") %>%
  html_text()
comments <- str_trim(gsub("comment|comments|discuss| ","", comments), "r")
comments[comments == ""] <- 0

## Get the Link
links <- webpage %>%
  html_nodes(".storylink") %>%
  html_attr("href") # need html attr rather than the text


## Extra: some websites lead to a sublinks within Hacker News
# extract the links from those subwebsites and replace initial link

item <- grep("item?id", links, fixed = TRUE, value = TRUE)

if (length(item) == 1) { # if there is only one internal link on HN

    newwebsite <- paste("https://news.ycombinator.com/",item, sep = "") # create new sublink

    newlinks <- read_html(newwebsite)                                   # read sublink
    newlinks <- newlinks %>%                                            # scrape information
    html_nodes("tr:nth-child(4) td+ td") %>%
    html_nodes("p a") %>%
    html_text()

links[grep(pattern = "item?id", links, fixed = T)] <- str_c(newlinks, collapse = " : ")

} else {                # if there is more than one internal link on HN
  for (e in seq(1,length(item),by = 1)) {
    
    newwebsite <- paste("https://news.ycombinator.com/",item[e], sep = "") 
    
    newlinks <- read_html(newwebsite)                                      
    newlinks <- newlinks %>%                                               
    html_nodes("tr:nth-child(4) td+ td") %>%
    html_nodes("p a") %>%
    html_text()
    
    links[grep(pattern = item[e], links, fixed = T)] <- str_c(newlinks, collapse = " : ") # replace false links
  }
}

# Combine the results
Results <- data.frame(news_title, news_points, comments, links)

# write CSV
write.csv(Results, file = paste(paste("Results_HN_",Sys.Date(),"_", hour(Sys.time()), minute(Sys.time()),sep = "")
                                , "csv", sep = "."),row.names = FALSE)

