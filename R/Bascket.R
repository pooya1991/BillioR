dest <- "./data/Portfolios.rda"
load(dest)

Bascket <- function(id){
  Portfolios[[id]][,-dim(Portfolios[[id]])] * 100
}
