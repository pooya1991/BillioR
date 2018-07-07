ShareData <- function(share,timeframe = "daily",sdate = "2014-01-01",edate = Sys.Date()){
  library(zoo)
  library(xts)
  library(quantmod)
  library(googlesheets)
  library(dplyr)
  gs_auth(token = token)
  share <- paste(" ",share," ",sep = "")
  d <- gs_read(gs_title(share))
  b <- pull(d,Time)
  Time <- as.POSIXct(strptime(b, "%Y-%m-%d %H:%M:%S"))
  ete <- data.frame(pull(d,6),pull(d,3),pull(d,4),pull(d,2),pull(d,8),pull(d,5),pull(d,7))
  colnames(ete) <- c("Open","High","Low","Close","Volume","nTrades","Value")
  dat <- data.frame(Time,ete)
  ete <- dat[complete.cases(dat),]
  dat <- xts(ete[,-1],order.by = ete[,1])
  baz <- paste(sdate,"/",edate,sep = "")
  dat <- dat[baz]
  switch(timeframe,
         m1 = result <- dat,
         m3 = result <- to.minutes3(dat),
         m5 = result <- to.minutes5(dat),
         m10 = result <- to.minutes10(dat),
         m15 = result <- to.minutes15(dat),
         m30 = result <- to.minutes30(dat),
         hourly = result <- to.hourly(dat),
         daily = result <- to.daily(dat),
         weekly = result <- to.weekly(dat),
         monthly = result <- to.monthly(dat),
         quarterly = result <- to.quarterly(dat)
         )
  Time <- index(result)
  result <- as.data.frame(result)
  result[,6] <- Time
  colnames(result) <- c("Open","High","Low","Close","Volume","Time")
  row.names(result) <- NULL
  result
}
