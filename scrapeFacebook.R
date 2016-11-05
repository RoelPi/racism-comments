app_id <- '461103667412110'
app_secret <- '4bc695ef0146ee5a7c6e5901ea60d1e6'
flagwords <- "terror|moslim|islam|vluchteling|allah|mohammed|profeet|syri|francken|mosul|strijder|saoudi|turk|onverdoofd|ramadan|moske"
library(Rfacebook)
FBauth <- function() {
    fbToken <<- fbOAuth(app_id, app_secret, extended_permissions = FALSE,
            legacy_permissions = FALSE)
}
FBgetMessagesOfPages <- function(id='hln.be',amount) {
    getPage(id,token=fbToken,n=amount)
}
FBgetReactions <- function(id='hln.be',postcount=25) {
    messages <- FBgetMessagesOfPages(id,postcount)
    messages <- messages[grep(flagwords,messages$message),]
    message(paste0(nrow(messages)," relevant messages found. Scraping comments."))
    comments <- data.frame(DATETIME=character(),URL=character(),TITLE=character(),USER=character(),COMMENT=character())
    
    for (i in 1:nrow(messages)) {
        commentsOfPost <- getPost(messages$id[i],fbToken,n=10000,comments=T,likes=F)$comments
        commentTime <- commentsOfPost$created_time
        commentURL <- rep(messages$link[i],length(commentsOfPost$message))
        commentTitle <- rep(messages$message[i],length(commentsOfPost$message))
        commentUser <- commentsOfPost$from_name
        commentComment <- commentsOfPost$message
        message(paste0("Scraping comments of post ",i,": '",commentTitle[i],"'"))
        selectedColumns <- data.frame(cbind(commentTime,commentURL,commentTitle,commentUser,commentComment))
        
        if (i == 1) {
            comments <- selectedColumns
        } else {
            comments <- rbind(comments,selectedColumns)
        }
    }
    colnames(comments) <- c("DATETIME","URL","TITLE","USER","COMMENT")
    comments
}