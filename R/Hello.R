Hello <- function(myname = ""){
  if(myname == ""){
    stop("Tell me your name!")
  }
  list(
    message = paste("hello", myname)
  )
}