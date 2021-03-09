#
#
# selectRetrieve6
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

Sys.setlocale('LC_ALL','C')
library(shiny)
library(pool)
library(tidyverse)
library(DBI)
library(zoo)
library(shinysky)
library(reshape2)
library(shinythemes)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    dropdown(shinythemes::themeSelector(), label = "Themes"),
    # Application title
    titlePanel("Sentiment Analysis Database Retrieval"),
    #########################
    #                       #
    #                       #
    #      Sidebar          #
    #                       #
    #                       #
    #                       #
    #########################
    sidebarLayout(
        sidebarPanel(width = 3,
                     dateRangeInput(inputId= 'dateRange',
                                    label = "Date range",
                                    start = Sys.Date() - 14,
                                    format = "yyyy_mm_dd"),
###                     # Selection 1
                     selectizeInput("isource",
                                    "Source 1",
                                    choices = rssSources.names,
                                    multiple = TRUE),
                     selectizeInput("icountry",
                                    "Country 1",
                                    choices = rss.Countries,
                                    multiple = TRUE),
                     selectizeInput("iorientation",
                                    "Orientation 1",
                                    choices = rss.Orientation,
                                    multiple = TRUE),

                     numericInput(inputId = "dateGrouping",
                                  "Rolling average 1",
                                  value = 1,
                                  min = 1,
                                  max = 90),

                     selectizeInput("iSentimentFactor",
                                    "Sentiment factor 1",
                                    c("Syuzhet" = 'syuzhet', "Afinn" = "afinn","Bing" = "bing", "Anger - nrc" = "nrc_anger","Anticipation - nrc" = "nrc_anticipation","Disgust - nrc" = "nrc_disgust",
                                      "Fear - nrc" = "nrc_fear", "Joy - nrc" = "nrc_joy","Positive- nrc"= "nrc_positive","Negative - nrc" = "nrc_negative","Sadness - nrc" = "nrc_sadness",
                                      "Surprise - nrc" = "nrc_surprise", "Trust - nrc" = "nrc_trust","Constraining - Lo" = "loughran_constraining","Litigous - Lo" = "loughran_litigious",
                                      "Uncertain- Lo" = "loughran_uncertain", "Negative - Lo" = "loughran_negative","Positive - Lo" = "loughran_positive", "Ensemble +/-" = "ensemble_posneg") ,
                                    multiple = TRUE,
                                    selected = "ensemble_posneg"),
                     textInput("iTextinput",
                               "Text selection 1",
                               value = " "),
###    # Selection 2
selectizeInput("isource2",
               "Source 2",
               choices = rssSources.names,
               multiple = TRUE),
selectizeInput("icountry2",
               "Country 2",
               choices = rss.Countries,
               multiple = TRUE),
selectizeInput("iorientation2",
               "Orientation 2",
               choices = rss.Orientation,
               multiple = TRUE),

numericInput(inputId = "dateGrouping2",
             "Rolling average 2",
             value = 1,
             min = 1,
             max = 90),

selectizeInput("iSentimentFactor2",
               "Sentiment factor 2",
               c("Syuzhet" = 'syuzhet', "Afinn" = "afinn","Bing" = "bing", "Anger - nrc" = "nrc_anger","Anticipation - nrc" = "nrc_anticipation","Disgust - nrc" = "nrc_disgust",
                 "Fear - nrc" = "nrc_fear", "Joy - nrc" = "nrc_joy","Positive- nrc"= "nrc_positive","Negative - nrc" = "nrc_negative","Sadness - nrc" = "nrc_sadness",
                 "Surprise - nrc" = "nrc_surprise", "Trust - nrc" = "nrc_trust","Constraining - Lo" = "loughran_constraining","Litigous - Lo" = "loughran_litigious",
                 "Uncertain- Lo" = "loughran_uncertain", "Negative - Lo" = "loughran_negative","Positive - Lo" = "loughran_positive", "Ensemble +/-" = "ensemble_posneg") ,
               multiple = TRUE,
               selected = "ensemble_posneg"),
textInput("iTextinput2",
          "Text selection 2",
          value = " ")

        ),

        #########################
        #                       #
        #                       #
        #      Main             #
        #                       #
        #                       #
        #                       #
        #########################

        # Show a plot of the generated distribution
        mainPanel(fluidRow(

            column(4,
            dropdown(
              tootip = TRUE,
              label = "Smoothing",
              tags$h3("Smoothing"),
              radioButtons("ismooth", "Method",
                           c("loess" = "loess", "lm" = "lm","gam" = "gam", "None"= ""))
            ),
            dropdown(
                tooltip = TRUE,
                label = "Normalize",
                fluidRow(
                radioButtons("iPosNegNorm", "Normalise pos/neg", choices = c("On", "Off"), inline = TRUE),
                radioButtons("iLRCNorm", "Normalise LRC", choices = c("On", "Off"), inline = TRUE),
                radioButtons("iCountryNorm", "Normalise Countries", choices = c("On", "Off"), inline = TRUE))
            ))),
            plotOutput("SA_by_date_line"),
            plotOutput("SA_summary_by_period"),
            plotOutput("SA_by_date_line2"),
            plotOutput("SA_summary_by_period2"),
            DT::dataTableOutput("tbl")

        )
    )
))
