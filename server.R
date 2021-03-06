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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Connections

  print("About to connect - Server/Remote 1")
  # Establish connection to Digital Ocean (remote) database
  remoteuserpassword <- "m3t1sz"
  conR <- dbConnect(RMariaDB::MariaDB(), dbname = 'metis', 'metis', password = remoteuserpassword, host = "178.62.8.181", port = 3306)
  print("Connected Server/Remote 1")
  dbListTables(conR)

  # Normal Functions

    rssSelection <- function(rssSelected,  Source, Orientation, Country, Topic){
      ifelse(is.null(Source), rssSelected <- rssSelected,
             rssSelected <- filter(rssSelected, Source == ext_name))
        ifelse(is.null(Orientation), rssSelected <- rssSelected,
               rssSelected <- filter(rssSelected, orientation %in% Orientation))
        ifelse(is.null(Country), rssSelected <- rssSelected,
               rssSelected <- filter(rssSelected, Country  == country))
        ifelse(is.null(Topic), rssSelected <- rssSelected,
               rssSelected)
    }

    posneg <- function(SA_scores){
        neg <- -sum(SA_scores[SA_scores <0])
        pos <- sum(SA_scores[SA_scores>0 ])
    }

    # Create sequence of dates for insertion into table names for SQL retrieval

    output$sqlDates <- renderText({
        outSeq <- seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")
        outSeq <- paste0("sa_RSS_library", outSeq, sep = "")
    })

# Reactive functions
# sumvals - sums values by selected
    sumVals <-  reactive({
      query_in <- rssSelection(query_out_Date(), input$isource, input$iorientation, input$icountry, input$iTextinput)
      sumVals <- query_in %>%
      group_by(item_date_published) %>%
      summarize(
        syuzhet = sum(syuzhet_score),
        afinn = sum(afinn_score),
        bing = sum(bing_score),
        nrc_anger = sum(nrc_score_anger),
        nrc_anticipation = sum(nrc_score_anticipation),
        nrc_disgust = sum(nrc_score_disgust),
        nrc_fear = sum(nrc_score_fear),
        nrc_joy = sum(nrc_score_joy),
        nrc_positive =sum(nrc_score_positive),
        nrc_negative = sum(nrc_score_negative),
        nrc_sadness = sum(nrc_score_sadness),
        nrc_surprise = sum(nrc_score_surprise),
        nrc_trust = sum(nrc_score_trust),
        loughran_constraining = sum(loughran_frame_constraining),
        loughran_litigious = sum(loughran_frame_litigious),
        loughran_negative = sum(loughran_frame_negative),
        loughran_positive = sum(loughran_frame_positive),
        loughran_uncertain = sum(loughran_frame_uncertain),
        ensemble_posneg = sum(ensemble_posneg)
      )
    sumVals <- filter(sumVals, item_date_published >= input$dateRange[1]) # Remove items before selection date
    sumVals <-sumVals %>% gather('syuzhet', 'afinn', 'bing', 'nrc_anger', 'nrc_anticipation', 'nrc_disgust', 'nrc_fear', 'nrc_joy',
                                 'nrc_positive', 'nrc_negative', 'nrc_sadness', 'nrc_surprise', 'nrc_trust', 'loughran_constraining',
                                 'loughran_litigious', 'loughran_negative', 'loughran_positive', 'loughran_uncertain','ensemble_posneg', key = "factorName", value = 'factorValue')
    sumVals$factorName <- as.factor(sumVals$factorName)
    sumVals
    })


#totVals - sums values for total period for each SA factor
    totVals <- reactive({
      query_in <- rssSelection(query_out_Date(), input$isource, input$iorientation, input$icountry, input$iTextinput)
      totVals <- query_in %>% gather(syuzhet_score, afinn_score, bing_score,
                                                              nrc_score_anger, nrc_score_anticipation, nrc_score_disgust, nrc_score_fear,
                                                              nrc_score_joy, nrc_score_positive, nrc_score_negative,
                                                              nrc_score_sadness, nrc_score_surprise, nrc_score_trust,
                                                              loughran_frame_constraining, loughran_frame_litigious,
                                                              loughran_frame_negative, loughran_frame_positive, loughran_frame_uncertain, nrc_comp, loughran_comp, ensemble_posneg, key = "factorName", value = 'factorValue')

    totVals$factorName <- as.factor(totVals$factorName)
    totValsSums <- tapply(totVals$factorValue, totVals$factorName, FUN = sum, na.rm = TRUE)
    totValsSums1 <- melt(totValsSums)
    print("totVals Here")
    colnames(totValsSums1) <- c("Factor", "Value")
    totValsSums1
    })



    ##############
    #
    #   Build full dataframe for dates
    #
    ##############
    query_out_Date <- reactive({
        queryDate <- as.Date(input$dateRange[1])
        queryDate <- format(as.Date(queryDate), "%Y_%m_%d")
        print(paste("queryDate (1) ", queryDate))

        outSeq <- seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")
        outSeq <- format(as.Date(outSeq, "%Y_%m_%d"))
        query_out_frame <- data.frame(ext_name = character(), item_title = character(), item_date_published = character(), orientation = character(), country = character() ,
                                      syuzhet_score = numeric(), afinn_score = numeric(), bing_score  = numeric(),
                                      nrc_score_anger = numeric(), nrc_score_anticipation = numeric(), nrc_score_disgust = numeric(), nrc_score_fear = numeric(),
                                      nrc_score_joy = numeric(), nrc_score_positive = numeric(), nrc_score_negative = numeric(),
                                      nrc_score_sadness = numeric(), nrc_score_surprise = numeric(), nrc_score_trust = numeric(),
                                      loughran_frame_constraining = numeric(), loughran_frame_litigious = numeric(), loughran_frame_negative = numeric(),
                                      loughran_frame_positive = numeric(), loughran_frame_uncertain = numeric(),
                                      hash_value = character())
        error_date <- data.frame(fail_date = character())
        ##############
        #
        #   Read database - default date if initial date unavailable
        #
        ##############
        print("Date DB read initiated")
        #### Table dates
        inserted_date_seq <- seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")

        for(i in seq_along(inserted_date_seq)){         # Start of read DB loop
            inserted_date <-  as.character( gsub("-", "_", inserted_date_seq[i]  ))

            # Construct SQL query for initial date

            queryScript <- paste0("SELECT ext_name, item_title,item_date_published, orientation, country, syuzhet_score, afinn_score, bing_score,
            nrc_score_anger, nrc_score_anticipation, nrc_score_disgust, nrc_score_fear,
            nrc_score_joy, nrc_score_positive, nrc_score_negative,
            nrc_score_sadness, nrc_score_surprise, nrc_score_trust,
            loughran_frame_constraining, loughran_frame_litigious,
            loughran_frame_negative, loughran_frame_positive, loughran_frame_uncertain,
            md5(concat(item_title, item_date_published)) AS hash_value
                             FROM sa_RSS_library", inserted_date, "
                            ;" )
            tryCatch(
                expr = {
                    try_date <- paste0("sa_RSS_library", inserted_date)
                    query1  <- dbGetQuery(conR, queryScript)
                    query1$item_date_published <- as.Date(query1$item_date_published, format = "%Y-%m-%d")
                    query_out_frame <- rbind(query_out_frame, query1)
                },
                error = function(e){
                    message(paste0("Error message on date: ", inserted_date, " "))
                    message(queryScript)
                    error_date <- rbind(error_date, inserted_date)
                },
                finally = {
                    message("tryCatch finished")
                }
            )  # end of tryCatch
        }

        error_date # LIst dates with missing tables
        query_out_frame <- query_out_frame[!duplicated(query_out_frame$hash_value),]   # Query response with no duplicates
        query_out_full <- query_out_frame # Contains all values (country / orientation )
 #       query_out <- rssSelection(query_out, Source = input$isource, Orientation = input$iorientation,Country = input$icountry, Topic = input$iTextinput)
        print("Here break")

        # Normalize values for ensemble positive / negative

        query_out_frame$nrc_comp <- query_out_frame$nrc_score_positive - query_out_frame$nrc_score_negative
        query_out_frame$loughran_comp <- query_out_frame$loughran_frame_positive - query_out_frame$loughran_frame_negative
        afinn.norm <- max(abs(query_out_frame$afinn_score))
        syuzhet.norm <- max(abs(query_out_frame$syuzhet_score))
        bing.norm <- max(abs(query_out_frame$bing_score))
        nrc.norm <- max(abs(query_out_frame$nrc_comp))
        loughran.norm <- max(abs(query_out_frame$loughran_comp))
        query_out_frame$ensemble_posneg <- query_out_frame$afinn_score/afinn.norm + query_out_frame$bing_score/bing.norm + query_out_frame$syuzhet_score/syuzhet.norm +
          query_out_frame$nrc_comp/nrc.norm + query_out_frame$loughran_comp/loughran.norm

        query_out_frame # returned
        # end of read DB loop
    })   # Retrieves records between dates. This is the only database retrieval. Other selections are done from this

    query_out_Date_proc <- query_out_Date
    query_out_List <- reactive({rssSelection(query_out_Date, input$isource, input$iorientation, input$icountry, input$iTextinput)
      }) # Filter on input$

    ##############
    #
    #   Output section
    #
    ##############
    output$SA_by_date_line <- renderPlot({
      sumVals1 <- filter(sumVals(), factorName %in% input$iSentimentFactor )
      p <- ggplot(sumVals1, aes(x = item_date_published, y = rollmean(factorValue, input$dateGrouping, na.pad = TRUE), colour = factorName)) +
        geom_line() +
        xlab("Story date") + ylab("Factor score") +
        ggtitle("Time series analysis / Moving average") +
        labs(colour = "Methods")

      p
    })

    output$SA_summary_by_period <-renderPlot({
      print("SA_s_b_P")
#      totVals1 <- filter(totVals(), factorName %in% input$iSentimentFactor )
      q <- ggplot(totVals(), aes(x = reorder(Factor, Value), y = Value))+
        theme(axis.text.x = element_text(angle = 90))+
        ggtitle("Period analysis / Sentiment") +
        geom_bar(stat = "identity")

      q
    })
    output$tbl <- DT::renderDT(rssSelection(query_out_Date(), input$isource, input$orientation, input$icountry, input$iTextinput))

})
