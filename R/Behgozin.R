Behgozin <- function(x,s,r){
  library(jsonlite)
  library(zoo)
  library(xts)
  library(TTR)
  library(quantmod)
  # get the data from PHP
  x <- as.character(x)
  v <- fromJSON(x)
  EndDate <- Sys.Date()
  StartDate <- as.Date(EndDate - 182)
  share <- v$stock
}