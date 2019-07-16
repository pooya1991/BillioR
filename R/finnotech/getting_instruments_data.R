library(httr)
library(jsonlite)
load("data/finnotech/token.rda")

service <- "companies"

address <- "https://api.finnotech.ir"
clientID <- "Billionaire"
finnotech_version <- "v1"
token <- content_token[["access_token"]][["value"]]

service_url <- paste(address, "bourse", finnotech_version, "clients", clientID, service, sep = "/")

instrument_get <- GET(service_url, add_headers(Authorization = paste("Bearer", token)))
if(status_code(instrument_get) == 200) {
  instruments <- jsonlite::fromJSON(content(instrument_get, "text"),simplifyVector = FALSE)
  instruments <- instruments[["result"]]
  save(instruments, file = "data/finnotech/instruments.rda")
}