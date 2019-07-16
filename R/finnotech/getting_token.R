library(httr)
library(jsonlite)

service <- "oauth2/token"

address <- "https://api.finnotech.ir"
clientID <- "Billionaire"
finnotech_version <- "v1"
CLIENTSECRET <- "211705bcb2ac2df2c2eb"

national_number = "2740370733"

service_url <- paste(address, "dev", finnotech_version, service, sep = "/")

body <- toJSON(data.frame(grant_type = "client_credentials", nid = national_number))
body <- substr(body,2,nchar(body)-1)
token_post <- POST(service_url, body = body, add_headers(Authorization = paste("Basic", base64_enc(paste(clientID, CLIENTSECRET, sep = ":"))), `Content-Type` = "application/json"))

if(status_code(token_post) == 200) {
  content_token <- jsonlite::fromJSON(content(token_post, "text"),simplifyVector = FALSE)
  save(content_token, file = "data/finnotech/token.rda")
}

headers(token_post)











