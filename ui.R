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

Sys.setlocale('LC_ALL','C')
library(shiny)
library(pool)
library(tidyverse)
library(DBI)
library(zoo)
# library(shinysky)
library(reshape2)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    shinythemes::themeSelector(),
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
                                    start = Sys.Date() - 3,
                                    format = "yyyy_mm_dd"),
                     selectizeInput("isource",
                                    "Source",
                                    choices = rssSources.names,
                                    multiple = TRUE),
                     selectizeInput("icountry",
                                    "Country",
                                    choices = rss.Countries,
                                    multiple = TRUE),
                     selectizeInput("iorientation",
                                    "Orientation",
                                    choices = rss.Orientation,
                                    multiple = TRUE),

                     numericInput(inputId = "dateGrouping",
                                  "Rolling average",
                                  value = 1,
                                  min = 1,
                                  max = 90),

                     selectizeInput("iSentimentFactor",
                                    "Sentiment factor",
                                    c("Syuzhet" = 'syuzhet', "Afinn" = "afinn","Bing" = "bing", "Anger - nrc" = "nrc_anger","Anticipation - nrc" = "nrc_anticipation","Disgust - nrc" = "nrc_disgust",
                                      "Fear - nrc" = "nrc_fear", "Joy - nrc" = "nrc_joy","Positive- nrc"= "nrc_positive","Negative - nrc" = "nrc_negative","Sadness - nrc" = "nrc_sadness",
                                      "Surprise - nrc" = "nrc_surprise", "Trust - nrc" = "nrc_trust","Constraining - Lo" = "loughran_constraining","Litigous - Lo" = "loughran_litigious",
                                      "Uncertain- Lo" = "loughran_uncertain", "Negative - Lo" = "loughran_negative","Positive - Lo" = "loughran_positive", "Ensemble +/-" = "ensemble_posneg") ,
                                    multiple = TRUE,
                                    selected = "ensemble_posneg"),
                     textInput("iTextinput",
                               "Text selection",
                               value = " "

                     )

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
        mainPanel(
            fluidRow(
                column(4, radioButtons("iPosNegNorm", "Normalise pos/neg", choices = c("On", "Off"), inline = TRUE)),
                column(4,radioButtons("iLRCNorm", "Normalise LRC", choices = c("On", "Off"), inline = TRUE)),
                column(4,radioButtons("iCountryNorm", "Normalise Countries", choices = c("On", "Off"), inline = TRUE)),
                plotOutput("SA_by_date_line"),
                plotOutput("SA_summary_by_period"),
            ),

            DT::dataTableOutput("tbl")

        )
    )
))
