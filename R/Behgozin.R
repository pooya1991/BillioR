Behgozin <- function(x,m,n,o){
  library(jsonlite)
  library(zoo)
  library(xts)
  library(TTR)
  library(quantmod)
  # get the data from PHP
  x <- as.character(x)
  v <- fromJSON(x)
  EndDtae <- Sys.Date()
  StartDate <- as.Date(EndDate - 180)
  share <- v$stock
}