dataW <- function(x){
  if(x==1){
    library(googlesheets)
    token <- token
    gs_auth(token = token)
    b <- gs_ls()
    b
  }
}