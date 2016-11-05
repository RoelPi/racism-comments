library(plyr)
makeUpDataSet <- function(name="Vlaams Belang",df = fb_VB) {
    df[,6] <- name
    df[,7:9] <- 0
    df[,10] <- row.names(df)
    colnames(df) <- c("DATETIME","URL","TITLE","USER","COMMENT","KANAAL","RACIST","NOTRACIST","NOOPINION","ID")
    df
}
saveDataSet <- function(df=makeUpDataSet(),filename="fb_comments_vlaamsbelang_20161030.csv") {
    con<-file(filename,encoding="UTF-8")
    write.csv(df,file=con,row.names=F)
    message(paste0("File saved to ",filename,"."))
}
bindFiles <- function(folder) {
    # thank you  https://stackoverflow.com/questions/23995384/read-and-rbind-multiple-csv-files
    mybig <- 
        do.call(
            rbind, lapply(
                list.files(folder), function(theFile){ 
                    read.csv(paste0(folder,"/",theFile), header=TRUE,fileEncoding="UTF-8")
                }
            )
        )
}
prepareFullDataSet <- function(df = bindfiles("fb_datasets")) {
    set.seed(1988)
    df$TITLE <- gsub("\"","'",df$TITLE)
    df$COMMENT <- gsub("\"","'",df$COMMENT)
    df$TITLE <- gsub("\"","'",df$TITLE)
    df$COMMENT <- gsub("\"","'",df$COMMENT)
    df <- ddply(df,"KANAAL",function(selection) {
        message(selection$KANAAL[1],nrow(selection))
        if (nrow(selection) <= 1000) {
            selection
        } else {
            selection[sample(x=nrow(selection),size=1000,replace=F),]
        }
    })
    df$ID <- seq(1,nrow(df))
    write.csv(df,"exportset.csv",row.names=F,fileEncoding="UTF-8")
    df
}