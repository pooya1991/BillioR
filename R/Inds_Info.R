Inds_Info <- function(x = "All"){
  if(x=="All"){
    result <- Indic
  }else{
    result <- Indic[[x]]
  }
  result
}