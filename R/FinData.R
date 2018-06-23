FinData <- function(x){
  library(TTR)
  library(quantmod)
  library(xts)
  library(zoo)
  library(Quandl)
  if(x == 1){
    v <- Quandl(c("LME/PR_AL","LME/PR_NI","LME/PR_ZI","LME/PR_PB"),type = "xts",start_date= Sys.Date()-7)
    c <- v[,c("LME.PR_AL - Cash Buyer","LME.PR_NI - Cash Buyer","LME.PR_ZI - Cash Buyer","LME.PR_PB - Cash Buyer")]
    colnames(c) <- c("Aluminum","Nickle","Zinc","Pb")
    nam <- c("Aluminum","Nickle","Zinc","Pb")
    CTT <- rep(index(c)[nrow(c)],ncol(c))
    CTT <- format(as.POSIXct(CTT,"%Y-%m-%d %H"),"%Y-%m-%d %H:%M:%S")
    Last <- c[nrow(c),]
    Change <- diff(c)[nrow(c),]
    Percent <- (Change / (Last - Change)) * 100
    e <- data.frame(as.vector(CTT),as.vector(Last),as.vector(Change),as.vector(Percent))
    a <- c("GC=F","SI=F","PL=F","BZ=F","HG=F","CL=F","RB=F")
    b <- c("Gold","Silver","Platinum","Brent Oil","Copper","Crude Oil","Galone Benzin")
    d <- getQuote(a)
    d[,1] <- as.character(d[,1])
    row.names(d) <- NULL
    g <- d[,1:4]
    colnames(e) <- c("TradeDate","Last","Change","Percent")
    colnames(g) <- c("TradeDate","Last","Change","Percent")
    final <- rbind(e,g)
    Names <- c(nam,b)
    final[,5] <- Names
    colnames(final)[5] <- "Names"
    final
  }
}
