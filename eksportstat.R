options(encoding="UTF-8")
library(httr)
# use rjstat JSON-stat library
library(rjstat)
library(jsonlite)
library(RJSONIO)
# bruk dplyr for å rydde tabeller og data
library(dplyr)
# bruk writexl for å eksportere til excel
library(writexl)
# Adress to Eksport  JSON-Stat dataset for laksestat - Index 
url <- "https://data.ssb.no/api/v0/no/table/08817/"
data <- '{
  "query": [
{
  "code": "Region",
  "selection": {
  "filter": "item",
  "values": [
  "01",
  "02",
  "03",
  "04",
  "05",
  "06",
  "07",
  "08",
  "09",
  "10",
  "11",
  "12",
  "14",
  "15",
  "50",
  "16",
  "17",
  "18",
  "19",
  "20"
  ]
  }
},
  {
  "code": "SITC",
  "selection": {
  "filter": "item",
  "values": [
  "SITC0-1",
  "SITC03u",
  "SITC2_4",
  "SITC3b",
  "SITC5-9"
  ]
  }
  },
  {
  "code": "ContentsCode",
  "selection": {
  "filter": "item",
  "values": [
  "Verdi"
  ]
  }
  }
  ],
  "response": {
  "format": "json-stat"
  }
  }'

d.tmp <- POST(url , body = data, encode = "json", verbose())
# get content from d.tmp as text, using fromJSONstat

eks <- fromJSONstat(content(d.tmp, "text"))

eksport <- data.frame(eks)

colnames(eksport) <- c("region", "varegruppe", "var", "MND", "verdi")
