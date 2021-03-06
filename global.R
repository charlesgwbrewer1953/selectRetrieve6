#
#
# selectRetrieve5
# Git
# /Developmnt/metis_dev2/selectRetrieve5
# - resegment_observe branch

# SERVER
#
#
# IMPORTANT NOTE - If plyr is loaded after dplyr (in tidyverse()), then group_by statement will fail
# with group factor not carried across
#
#

library(shiny)
library(tidyverse)
library(RMariaDB)



print("About to connect - Gobal/Remote")
# Establish connection to Digital Ocean (remote) database
remoteuserpassword <- "m3t1sz"
conR <- dbConnect(RMariaDB::MariaDB(), dbname = 'metis', 'metis', password = remoteuserpassword, host = "178.62.8.181", port = 3306)
print("Connected remote 1")
dbListTables(conR)


# Retrieve RSS feed static data
dbQuery <- dbSendQuery(conR, "SELECT * FROM rssSources")
rssSources <- dbFetch(dbQuery)
print("RSS Feeds static data retrieved")
rssSources.names <- unique(select(rssSources,Feed))
rssSources.names <- sort(rssSources.names[,1])
rss.Countries <- unique(select(rssSources,Country))
rss.Countries <- sort(rss.Countries[,1])
rss.Orientation <- unique(select(rssSources,Orientation))
rss.Orientation <- sort(rss.Orientation[,1])
rss.Lookups <- unique(select(rssSources,URL, Orientation))

# dbDisconnect(conM)
# print("Remote disconnected - Global/Remote")

# print("About to connect - Local")

############
#
# local user = metis_local
# local pw = metis_pw
# locval db = metis_db
#
############

#
#
# remoteuserpassword <- "metis_pw"
# conL <- dbConnect(RMariaDB::MariaDB(), dbname = 'metis_db', 'metis_local', password = remoteuserpassword, host = "127.0.0.1", port = 3306)
# print("Connected local 1")
# dbListTables(conL)
# dbDisconnect(conL)
