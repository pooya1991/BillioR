SLTPReEn <- function(Stg,p,ReType,ReAmm){
  library(jsonlite)
  x <- as.character(Stg)
  Stg <- fromJSON(x)
  Exit <- Stg[["BUY"]][["Exit"]]
  sl <- NA
  tp <- NA
  re <- NA
  if(Exit$StopLost[1,1] == "Percent"){
    sl <- p * ((100 - Exit$StopLost[1,2])/100)
  }else if(Exit$StopLost[1,1] == "Price"){
    sl <- p - Exit$StopLost[1,2]
  }
  if(Exit$TakeProfit[1,1] == "Percent"){
    tp <- p * ((100 + Exit$TakeProfit[1,2])/100)
  }else if(Exit$TakeProfit[1,1] == "Price"){
    tp <- p + Exit$TakeProfit[1,2]
  }
  switch (ReType,
    inc_pri = re <- p + ReAmm,
    inc_per = re <- p * ((ReAmm + 100)/100),
    dec_pri = re <- p - ReAmm,
    dec_per = re <- p * ((100 - ReAmm)/100)
  )
  Result <- data.frame(StopLost = sl,TakeProfit = tp, ReEnter = re)
  return(Result)
}