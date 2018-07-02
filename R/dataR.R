dataR <- function(x){
  if(x==1){
    username <- "billionaire_sc"
    password <- "DiAHd%ueKJCep_q"
    URL <- "https://pranatrader.ir/services/LoginSuperClient"
    lin <- paste(URL,"?username=",username,"&password=",password,"&ip=0:0:0:0:0:0:0:1",sep = "")
    bb <- readLines(lin)
  }
}