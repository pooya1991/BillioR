BasMng <- function(X){
  x <- as.character(X)
  library(jsonlite)
  vv <- fromJSON(x)
  reb <- 0
  LRebDate <- as.Date(vv$rebalance_date)
  FRebDate <- as.Date(vv$first_rebalance_date)
  rn <- as.numeric(vv$risk_number)
  t <- as.numeric(Sys.Date() - LRebDate)
  erisk <- RiskNumbers[RiskNumbers[,3]==rn,][1,1]
  eret <- RiskNumbers[RiskNumbers[,3]==rn,][1,2]
  Asset <- vv$investment
  init_val <- Asset[1,2]
  Ret <- Asset[,2] / init_val
  risk1 <- -sd(Ret-1)
  risk2 <- (min(Ret) - 1) * 100
  risk <- min(risk1,risk2)
  dd <- dim(Asset)[1]
  pd <- dd/120
  pret <- ((1+eret/100)^pd) * 100
  current <- Ret[dd]
  if(erisk < risk){
    risk_notif <- 1
  }else{
    risk_notif <- 0
  }
  if(current > 1.15*pret){
    ret_notif <- 1
  }else if(current < 0.85*pret){
    ret_notif <- -1
  }else{
    ret_notif <- 0
  }
  if(risk_notif == 1){
    if(ret_notif==1){
      reb <- 0
    }else{
      reb <- 1
    }
  }else{
    if(ret_notif == 1){
      reb <- -1
    }else if(ret_notif == -1){
      if(t > 10){
        reb <- 1
      }else{
        reb <- 1
      }
    }else{
      reb <- -1
    }
  }
  if(reb == -1){
    Portfolio <- vv$portfolio[,c(1,4)]
    Rebalance <- reb
    data <- list(Rebalance,Portfolio)
  }else if(reb == 0){
    Portfolio <- vv[[4]][,c(1,4)]
    Rebalance <- reb
    data <- list(Rebalance,Portfolio)
  }else{
    n <- length(PortfolioRisks)
    nam <- colnames(PortfolioRisks[[1]])
    PR <- data.frame(matrix(unlist(PortfolioRisks),nrow = n,byrow=T))
    colnames(PR) <- nam
    y <- PR[PR[,4] == rn,]
    y <- y[with(y, order(-strength)), ]
    id <- y[1,1]
    Portfolio <- Portfolios[[id]][,-dim(Portfolios[[id]])]
    Rebalance <- reb
    data <- list(Rebalance,Portfolio)
  }
  Error <- 0
  Error_detail <- "No Error"
  list(Error,Error_detail,data)
}