library(readxl)
library(tidyverse)
function_dev <- read_excel("function_dev.xlsx") # Selectd data
function_dev$act_date <- as.Date(str_sub(function_dev$item_date_published, 1, 10)) # Reduce date to YYYY-MM-DD

################## Functional code starts here


#Factor normailzation
max_by_col1 <- as_tibble(group_by(function_dev, act_date))    # Takes source df and renders as tibble, groups by date
max_by_col1[!is.finite(max_by_col1)] <- NA    # Removes infinite values - replaces as NA



max_rtn1 <- summarize(max_by_col1, afinn = max(abs(afinn_score), na.rm = TRUE), bing = max(abs(bing_score), na.rm = TRUE),
                     nrc_anger = max(abs(nrc_score_anger), na.rm = TRUE), nrc_anticipation = max(abs(nrc_score_anticipation), na.rm = TRUE),
                     nrc_disgust = max(abs(nrc_score_disgust), na.rm = TRUE), nrc_fear = max(abs(nrc_score_fear), na.rm = TRUE),nrc_joy = max(abs(nrc_score_joy), na.rm = TRUE),
                     nrc_sadness = max(abs(nrc_score_sadness), na.rm = TRUE),nrc_surprise = max(abs(nrc_score_surprise), na.rm = TRUE),nrc_trust = max(abs(nrc_score_trust), na.rm = TRUE),
                     nrc_negative = max(abs(nrdc_score_negative), na.rm = TRUE),nrc_positive = max(abs(nrc_score_positive), na.rm = TRUE),
                     loughran_negative = max(abs(loughran_frame_negative), na.rm = TRUE), loughran_positive = max(abs(loughran_frame_positive), na.rm = TRUE),
                     loughran_uncertain = max(abs(loughran_frame_uncertain), na.rm = TRUE),loughran_litigous = max(abs(loughran_frame_litigous), na.rm = TRUE),
                     loughran_constraining = max(abs(loughran_frame_constraining), na.rm = TRUE))


# +/- normaalization
