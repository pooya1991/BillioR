SLTPReEn <- function(Stg,p,ReType,ReAmm){
  library(jsonlite)
  x <- as.character(Stg)
  Stg <- fromJSON(x)
  Exit <- Stg[["BUY"]][["Exit"]]
  sl <- 0
  tp <- 0
  re <- 0
  if(Exit$StopLost[1,1] == "Percent"){
    sl <- floor(p * ((100 - Exit$StopLost[1,2])/100))
  }else if(Exit$StopLost[1,1] == "Price"){
    sl <- floor(p - Exit$StopLost[1,2])
  }
  if(Exit$TakeProfit[1,1] == "Percent"){
    tp <- floor(p * ((100 + Exit$TakeProfit[1,2])/100))
  }else if(Exit$TakeProfit[1,1] == "Price"){
    tp <- floor(p + Exit$TakeProfit[1,2])
  }
  switch (ReType,
    inc_pri = re <- floor(p + ReAmm),
    inc_per = re <- floor(p * ((ReAmm + 100)/100)),
    dec_pri = re <- floor(p - ReAmm),
    dec_per = re <- floor(p * ((100 - ReAmm)/100))
  )
  Result <- data.frame(StopLost = sl,TakeProfit = tp, ReEnter = re)
  return(Result)
}