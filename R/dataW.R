dataW <- function(x){
  library(googlesheets)
  if(x==1){
    token <- token
    gs_auth(token = token)
    b <- gs_ls()
    b
  }
}