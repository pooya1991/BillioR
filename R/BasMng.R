BasMng <- function(X){
  x <- as.character(X)
  vv <- fromJSON(x)
  if(vv[[1]]==0){
    LRebDate <- as.Date(vv[[3]][[1]])
    FRebDate <- as.Date(vv[[3]][[2]])
    rn <- vv[[3]][[3]]
    t <- as.numeric(Sys.Date() - LRebDate)
    erisk <- RiskNumbers[RiskNumbers[,3]==vv[[3]][[3]],][1,1]
    eret <- RiskNumbers[RiskNumbers[,3]==vv[[3]][[3]],][1,2]
    Asset <- vv[[3]][[5]]
    init_val <- Asset[1,2]
    Ret <- Asset[,2] / init_val
    risk1 <- -sd(Ret)
    risk2 <- (min(Ret) - 1) * 100
    dd <- dim(Asset)[1]
    pd <- dd/12
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
    if(risk_notif[i] == 1){
      if(ret_notif[i]==1){
        reb <- 0
      }else{
        reb <- 1
      }
    }else{
      if(ret_notif[i] == 1){
        reb <- -1
      }else if(ret_notif[i] == -1){
        if(t > 10){
          reb <- 1
        }else{
          reb <- 1
        }
      }else{
        reb <- -1
      }
    }
  }
  ErrorCode <- 0
  ErrorDetail <- "No Error"
  if(reb == -1){
    Portfolio <- vv[[3]][[4]][,c(1,4)]
    Rebalance <- reb
    data <- list(Rebalance,Portfolio)
  }else if(reb == 0){
    Portfolio <- vv[[3]][[4]][,c(1,4)]
    Rebalance <- reb
    data <- list(Rebalance,Portfolio)
  }else{
    Portfolio <- Bascket(rn = rn)
    Rebalance <- reb
    data <- list(Rebalance,Portfolio)
  }
  ErrorCode <- 0
  ErrorDetail <- "No Error"
  list(ErrorCode,ErrorDetail,data)
}









