Hello <- function(myname = ""){
  if(myname == ""){
    stop("Tell me your name!")
  }
  list(
    message = paste("hello Update Bascket 20180203", myname)
  )
}