library(readr)
library(syuzhet)
library(RMariaDB)
library(tidyverse)
library(tidytext)
rm(list = ls())

# Sentiment analysis functions - Single response
f_afinn <- function(x, output){
  analysand <- x["item_title"]
  s_v <- get_sentiment(analysand, method = 'afinn')
}

f_bing <- function(x, output){
  analysand <- x["item_title"]
  s_v <- get_sentiment(analysand, method = 'bing')
}

f_syuzhet <- function(x, output){
  analysand <- x["item_title"]
  s_v <- get_sentiment(analysand, method = 'syuzhet')
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

remoteuserpassword <- "m3t1sz"
conRemote <- dbConnect(RMariaDB::MariaDB(), dbname = 'metis', 'metis', password = remoteuserpassword, host = "178.62.8.181", port = 3306)
print("Connected - Remote db")

localuserpassword <- "metis_pw"

conLocal <- dbConnect(RMariaDB::MariaDB(), dbname = 'metis_db', 'metis_local', password = localuserpassword, host = "127.0.0.1", port = 3306)
print("Connected - Local db")


# Obtain list of tables in db for conversion (ie created from sources, but not SA processed yet)

dbQuery <- dbSendQuery(conLocal, "SELECT * FROM information_schema.tables WHERE table_schema = 'metis_db'")
rssSources <- dbFetch(dbQuery)
rssSources <- filter(rssSources, substr(rssSources$TABLE_NAME, 1,5) == "RSS_d")
rssSources <- as_tibble(rssSources[,3])

# Start of clean / augment

#nrow(rssSources)
for(i in 1: nrow(rssSources)){
  print(rssSources[i,1])
  tbl_date <- substrRight(rssSources[i,1],10)
  tbl_date <- gsub("-", "_", tbl_date)
  print(tbl_date)
  init_txt <- 'SELECT * FROM metis_db.`'   # ALERT - Change to accessed database
  date_extract <- rssSources[i,1]
  t_name = paste0(init_txt, rssSources[i,1], '`')
  t_name
  dbQuery <- dbSendQuery(conLocal, t_name)
  intermediate <- dbFetch(dbQuery)

  #Single response
 # print('SINGLE RESPONSE ', i)
  print("Afinn")
  intermediate$afinn_score <- apply(intermediate, 1, f_afinn)
  print("Bing")
  intermediate$bing_score <- apply(intermediate, 1, f_bing)
  print("Syuzhet")
  intermediate$syuzhet_score <- apply(intermediate, 1, f_syuzhet)

  # Multi response

  # nrc
  print("nrc")
  n_intermediate = nrow(intermediate)
  nrc_frame <- data.frame(anger = numeric(0),
                          anticipation = numeric(0),
                          disgust  = numeric(0),
                          fear = numeric(0),
                          joy = numeric(0),
                          sadness = numeric(0),
                          surprise = numeric(0),
                          trust = numeric(0),
                          negative = numeric(0),
                          positive = numeric(0))

  for (i in 1:nrow(intermediate)) {
    nrc_data <- get_nrc_sentiment(intermediate[i, "item_title"])
    nrc_frame <- rbind(nrc_frame, nrc_data)
  }

  print('nrc_frame')
  nrc_frame
  nrc_frame <- rename(nrc_frame,
                      nrc_score_anger = anger,
                      nrc_score_anticipation = anticipation,
                      nrc_score_disgust  = disgust,
                      nrc_score_fear = fear,
                      nrc_score_joy = joy,
                      nrc_score_sadness = sadness,
                      nrc_score_surprise = surprise,
                      nrc_score_trust = trust,
                      nrc_score_negative = negative,
                      nrc_score_positive = positive)


  intermediate1 <- cbind(intermediate, nrc_frame)

  ## End of nrc

  ## Loughran
  print("loughran")
  method = "loughran"

  loughran_dataY <- data.frame(intermediate[2,])
  loughran_dataY <- loughran_dataY[-1,]
  for (i in 1:nrow(intermediate)){
    loughran_dataX <- intermediate[i,] %>%
      unnest_tokens(word, item_title) %>%
      anti_join(stop_words) %>%
      inner_join(get_sentiments(method), by = "word")
    loughran_dataY <- rbind(loughran_dataY, loughran_dataX)
  }
print("Problem location")
print(loughran_dataY$index)
print(loughran_dataY$sentiment)
  loughran_consol <- dplyr::select(loughran_dataY, index, sentiment)
  loughran_frame <- data.frame(index = 1:nrow(intermediate),
                               negative = 0,
                               positive = 0,
                               uncertain  = 0,
                               litigious = 0,
                               constraining = 0)


  for(i in 1:nrow(loughran_consol)){
    switch(loughran_consol$sentiment[i],
           "negative" = loughran_frame$negative[i] <- 1,
           "positive" = loughran_frame$positive[i] <- 1,
           "uncertain" = loughran_frame$uncertain[i] <- 1,
           "litigious" = loughran_frame$litigious[i] <- 1,
           "constraining" = loughran_frame$constraining[i] <- 1
    )
  }

  loughran_frame <- rename(loughran_frame,
                           loughran_frame_negative = negative,
                           loughran_frame_positive = positive,
                           loughran_frame_uncertain  = uncertain,
                           loughran_frame_litigious = litigious,
                           loughran_frame_constraining = constraining)
print("Here")
  intermediate2 <- cbind(intermediate1, loughran_frame[,2:ncol(loughran_frame)])
  ####### Write SA processed table
  tabName <- paste0("sa_RSS_library", tbl_date)
  write_start <- Sys.time()
  dbWriteTable(conLocal, value = intermediate2,name = tabName, append = FALSE, overwrite = TRUE )
  print("Table written local")
  dbWriteTable(conRemote, value = intermediate2,name = tabName, append = FALSE, overwrite = TRUE )
  print("Table written remote")
  print( tabName)

}
dbDisconnect(conRemote)


dbDisconnect(conLocal)
