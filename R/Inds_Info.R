Inds_Info <- function(x = "All"){
  if(x=="All"){
    result <- INDICS
  }else{
    result <- INDICS[[x]]
  }
  result
}