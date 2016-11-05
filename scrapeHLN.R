sheetURL <- "https://docs.google.com/spreadsheets/d/1CBlUs_eai9HeY59JrfCtvEtqOchISCL-l34KwA7QUN0/edit?usp=sharing"
library(rvest)
library(httr)
library(XML)
library(googlesheets)
library(stringr)
options(digits = 4)
HLNgetCommentsOfPage <- function(url) {
    # Read comment module of given page
    urlPart <- strsplit(url,"/")[[1]][10]
    commentURL <- paste0("http://www.hln.be/hln/reaction/listContent.do?page=0&componentId=",urlPart)
    page <- read_html(commentURL)
    
    # Are there comments?
    areThereComments <- ifelse(GET(commentURL)$all_headers[[1]]$headers$`content-length` > 25,TRUE,FALSE)
    
    if (areThereComments) {
        
        # Go to comment module of given page
        commentURL <- paste0("http://www.hln.be/hln/reaction/listContent.do?page=0&componentId=",urlPart)
        page <- read_html(commentURL)
        
        # How many comment pages are there?
        pageLength <- as.numeric(gsub("[ \r\n]","",substring(trimws(page %>% html_node(".pagenav.right") %>% html_text()),7,8)))-1
        pageLength <- gsub(" ","",pageLength)
        
        # First iteration
        quotes <- page %>% html_nodes("li blockquote") %>% html_text()
        users <- page %>% html_nodes("li cite") %>% html_text()
        
        df <- as.data.frame(cbind(users,quotes))
        
        # More iterations if there is more than one page of comments
        if(!is.na(pageLength)) {
            for (i in 1:pageLength) {
                commentURL <- paste0("http://www.hln.be/hln/reaction/listContent.do?page=",i,"&componentId=",urlPart)
                page <- read_html(commentURL)
                
                quotes <- page %>% html_nodes("li blockquote") %>% html_text()
                users <- page %>% html_nodes("li cite") %>% html_text()
                
                dataset <- as.data.frame(cbind(users,quotes))
                df <- rbind(df,dataset)
            }
        }
        names(df) <- c("USER","COMMENT")
        
        # Remove spaces & nextlines
        df$USER <- trimws(gsub(pattern="[\t\n\r\v\f]",replacement="",df$USER))
        df$COMMENT <- trimws(gsub(pattern="[\t\n\r\v\f]",replacement="",df$COMMENT))
        df
    } else {
        df <- data.frame(URL=character(),USER=character(),COMMENT=character())
    }

}
HLNgetCommentsOfURLs <- function(urls = HLNgetKeywordURLs("kris peeters",3)) {
    df <- data.frame(DATETIME=character(),URL=character(),TITLE=character(),USER=character(),COMMENT=character())
    for (i in 1:nrow(urls)) {
        commentList <- HLNgetCommentsOfPage(as.character(urls[i,2]))
        commentsLength <- nrow(commentList)
        message(paste("Article",i,"of",nrow(urls),"- Collecting",commentsLength,"comments."))
        dateList <- rep(urls[i,1],commentsLength)
        urlList <- rep(urls[i,2],commentsLength)
        titleList <- rep(urls[i,3],commentsLength)
        if (i == 1) {
            df <- cbind(dateList,urlList,titleList,commentList)
        } else {
            df <- rbind(df,data.frame(cbind(dateList,urlList,titleList,commentList)))
        }
    }
    names(df) <- c("DATETIME","URL","TITLE","USER","COMMENT")
    df
}
HLNgetKeywordURLs <- function(keyword = "kris peeters", maxPages=3) {
    # How many pages are there about this keyword?
    keyword <- URLencode(keyword)
    url <- paste0("http://www.hln.be/hln/article/searchResult.do?language=nl&navigationItemId=1&searchValue=",keyword,"&page=0&resultAmountPerPage=&startSearchDate=&endSearchDate=&filterNavigationItemId=&filterSource=&timeFilter")
    firstpage <- read_html(url)
    pageLength <- trimws(firstpage %>% html_node("body .pagenav li") %>% html_text())
    pageLength <- as.numeric(substring(pageLength,9,nchar(pageLength)-2))-1
    
    # Loop over all the pages and grab all the links, titles & datetimes
    urls <- c()
    titles <- c()
    datetimes <- c()
    if (maxPages < pageLength) { pageLength <- maxPages}
    for (i in 0:pageLength) {
        url <- paste0("http://www.hln.be/hln/article/searchResult.do?language=nl&navigationItemId=1&searchValue=",keyword,"&page=",i,"&resultAmountPerPage=&startSearchDate=&endSearchDate=&filterNavigationItemId=&filterSource=&timeFilter")
        page <- read_html(url)
        urlList <- page %>% html_nodes("h3 a") %>% html_attr("href")
        dateTimeList <- page %>% html_nodes("p .time") %>% html_text()
        titleList <- page %>% html_nodes("h3 a") %>% html_text()
        currentLength <- length(urls)
        message(paste("Article search results page",i,"of",pageLength,"- Collecting URL, title & timestamp."))
        for (j in 1:length(urlList)) {
            urls[currentLength+j] <- urlList[j]
            titles[currentLength+j] <- titleList[j]
            datetimes[currentLength+j] <- dateTimeList[j]
        }
    }
    
    # cbind all the data and give appropriate names
    urls <- paste0("http://www.hln.be",urls)
    allData <- data.frame(cbind(datetimes,urls,titles))
    names(allData) <- c("DATE","URL","TITLE")
    message(paste(nrow(allData),"articles found. Harvesting comments."))
    allData
}
HLNcleanCSV <- function(file = "test.csv") {
    df <- read.csv(file,header=T,sep=",",strip.white=T)
    df$COMMENT <- trimws(gsub(pattern="[\t\n\r\v\f]",replacement="",df$COMMENT))
    df$TITLE <- trimws(gsub(pattern="[\t\n\r\v\f]",replacement="",df$TITLE))
    write.csv(df,file)
}