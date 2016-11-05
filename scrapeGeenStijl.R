sheetURL <- "https://docs.google.com/spreadsheets/d/1CBlUs_eai9HeY59JrfCtvEtqOchISCL-l34KwA7QUN0/edit?usp=sharing"
library(rvest)
library(httr)
library(XML)
library(googlesheets)
library(stringr)
options(digits = 4)

GSgetCommentsOfPage <- function(url) {
    # Are there comments?
    # areThereComments <- ifelse(GET(commentURL)$all_headers[[1]]$headers$`content-length` > 25,TRUE,FALSE)
    areThereComments <- TRUE
    
    if (areThereComments) {
        page <- read_html(url)

        # How many comment pages are there?
        quotes <- page %>% html_nodes(".commentlist article") %>% html_text()
        pageLength <- length(quotes)
        selection <- substr(quotes,1,1) != "@" & !grepl("\\-weggejorist\\-",quotes) & !grepl("\\|",substring(quotes,1,30))
        quotes <- quotes[selection]
        for (i in 1:length(quotes)) {
            quotes[i] <- substr(quotes[i],1,tail(gregexpr("[\n\t]",quotes[i])[[1]]))
        }

        # First iteration
        footer <- page %>% html_nodes(".commentlist article footer") %>% html_text()
        footer <- footer[selection]
        users <- data.frame(matrix(unlist(strsplit(footer,"[|]")),ncol=3,byrow=T),stringsAsFactors=F)[,1]
        
        if (length(users) == length(quotes))
            {
            df <- cbind.data.frame(users,quotes,stringsAsFactors=F)
            # More iterations if there is more than one page of comments
            names(df) <- c("USER","COMMENT")
            
            # Remove spaces & nextlines
            df$USER <- trimws(gsub(pattern="[\t\n\r\v\f]",replacement="",df$USER))
            df$COMMENT <- trimws(gsub(pattern="[\t\n\r\v\f]",replacement="",df$COMMENT))
            df
        } else {
            df <- data.frame(URL=character(),USER=character(),COMMENT=character())
        }
    } else {
        df <- data.frame(URL=character(),USER=character(),COMMENT=character())
    }
    
}
GSgetCommentsOfURLs <- function(urls = GSgetKeywordURLs("islam",3)) {
    df <- data.frame(DATETIME=character(),URL=character(),TITLE=character(),USER=character(),COMMENT=character())
    for (i in 1:nrow(urls)) {
        commentList <- GSgetCommentsOfPage(as.character(urls[i,2]))
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
GSgetKeywordURLs <- function(keyword = "islam", maxPages=3) {
    maxPages <- maxPages - 1
    # How many pages are there about this keyword?
    keyword <- URLencode(keyword)
    url <- paste0("https://www.geenstijl.nl/fastsearch?query=",keyword,"&category=&order=&lastn=0&offset=0&blogs=1")
    firstpage <- read_html(url)
    countSentence <- firstpage %>% html_node("#sitesearch2 p") %>% html_text()
    pageLength <- floor(as.numeric(trimws(substring(countSentence,9,gregexpr("[ ]",countSentence)[[1]][3]-1)))/25)
    
    # Loop over all the pages and grab all the links, titles & datetimes
    urls <- c()
    titles <- c()
    datetimes <- c()
    if (maxPages < pageLength) { pageLength <- maxPages}
    for (i in 0:pageLength) {
        url <- paste0("https://www.geenstijl.nl/fastsearch?query=",keyword,"&category=&order=&lastn=25&offset=",i*25,"&blogs=1")
        page <- read_html(url)
        urlList <- page %>% html_nodes(".pagecontent.search ul li a") %>% html_attr("href")
        dateTimeList <- page %>% html_nodes(".pagecontent.search ul li .footer") %>% html_text()
        titleList <- page %>% html_nodes(".pagecontent.search ul li a") %>% html_text()
        currentLength <- length(urls)
        message(paste("Article search results page",i,"of",pageLength,"- Collecting URL, title & timestamp."))
        for (j in 1:length(urlList)) {
            urls[currentLength+j] <- urlList[j]
            titles[currentLength+j] <- titleList[j]
            datetimes[currentLength+j] <- dateTimeList[j]
        }
    }
    
    # cbind all the data and give appropriate names
    allData <- data.frame(cbind(datetimes,urls,titles))
    names(allData) <- c("DATE","URL","TITLE")
    message(paste(nrow(allData),"articles found. Harvesting comments."))
    allData
}