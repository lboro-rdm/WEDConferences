library(httr)
library(jsonlite)

api_token <- Sys.getenv("APIkey")
base_url <- "https://api.figshare.com/v2"

get_groups <- function() {
  res <- GET(
    url = paste0(base_url, "/account/institution/groups"),
    add_headers(Authorization = paste("token", api_token))
  )
  content(res, "parsed", simplifyVector = TRUE)
}
groups <- get_groups()

wedc_groups <- groups[grep("^WEDC", groups$name), ]
print(wedc_groups)

write.csv(wedc_groups[, c("id", "name")], "wedc_groups.csv", row.names = FALSE)
