library(readxl)
library(tidyverse)
function_dev <- read_excel("~/OneDrive/Development/metis_dev2/selectRetrieve6/function_dev.xlsx") # Selectd data
function_dev$act_date <- as.Date(str_sub(function_dev$item_date_published, 1, 10)) # Reduce date to YYYY-MM-DD

################## Functional code starts here

# max_by_col1 <- as_tibble(group_by(function_dev, act_date))    # Takes source df and renders as tibble, groups by date
# max_by_col1[!is.finite(max_by_col1)] <- NA    # Removes infinite values - replaces as NA
#
#
#
# max_rtn1 <- summarize(max_by_col1, syuzhet = max(abs(syuzhet_score), na.rm = TRUE), bing = max(abs(bing_score), na.rm = TRUE), syuzhet = max(abs(syuzhet_score), na.rm = TRUE),
#                      nrc_anger = max(abs(nrc_score_anger), na.rm = TRUE), nrc_anticipation = max(abs(nrc_score_anticipation), na.rm = TRUE),
#                      nrc_disgust = max(abs(nrc_score_disgust), na.rm = TRUE), nrc_fear = max(abs(nrc_score_fear), na.rm = TRUE),nrc_joy = max(abs(nrc_score_joy), na.rm = TRUE),
#                      nrc_sadness = max(abs(nrc_score_sadness), na.rm = TRUE),nrc_surprise = max(abs(nrc_score_surprise), na.rm = TRUE),nrc_trust = max(abs(nrc_score_trust), na.rm = TRUE),
#                      nrc_negative = max(abs(nrdc_score_negative), na.rm = TRUE),nrc_positive = max(abs(nrc_score_positive), na.rm = TRUE),
#                      loughran_negative = max(abs(loughran_frame_negative), na.rm = TRUE), loughran_positive = max(abs(loughran_frame_positive), na.rm = TRUE),
#                      loughran_uncertain = max(abs(loughran_frame_uncertain), na.rm = TRUE),loughran_litigous = max(abs(loughran_frame_litigous), na.rm = TRUE),
#                      loughran_constraining = max(abs(loughran_frame_constraining), na.rm = TRUE))

#######################

num_cols <- dplyr::select(function_dev, c(afinn_score, bing_score, syuzhet_score, nrc_score_positive, nrc_score_negative,
                                   loughran_frame_positive, loughran_frame_negative ))

afinn_pos <- sum(num_cols$afinn_score[num_cols$afinn_score > 0], na.rm = TRUE)
afinn_neg <- -1 *sum(num_cols$afinn_score[num_cols$afinn_score < 0], na.rm = TRUE)
syuzhet_neg <- -1 *sum(num_cols$syuzhet_score[num_cols$syuzhet_score < 0], na.rm = TRUE)
bing_pos <- sum(num_cols$bing_score[num_cols$bing_score > 0], na.rm = TRUE)
bing_neg <- -1 *sum(num_cols$bing_score[num_cols$bing_score < 0], na.rm = TRUE)
syuzhet_pos <- sum(num_cols$syuzhet_score[num_cols$syuzhet_score > 0], na.rm = TRUE)
syuzhet_neg <- -1 *sum(num_cols$syuzhet_score[num_cols$syuzhet_score < 0], na.rm = TRUE)
nrc_pos <- sum(num_cols$nrc_score_positive, na.rm = TRUE)
nrc_neg <- sum(num_cols$nrc_score_negative, na.rm = TRUE)
loughran_pos <- sum(num_cols$loughran_frame_positive, na.rm = TRUE)
loughran_neg <- sum(num_cols$loughran_frame_negative, na.rm = TRUE)

afinn_posneg_norm <- afinn_neg/afinn_pos
bing_posneg_norm <- bing_neg/bing_pos
syuzhet_posneg_norm <- syuzhet_neg/syuzhet_pos
nrc_posneg_norm <- nrc_neg/nrc_pos
loughran_posneg_norm <- loughran_neg/loughran_pos
posneg_norm <- data.frame(afinn_posneg_norm, bing_posneg_norm, syuzhet_posneg_norm, nrc_posneg_norm, loughran_posneg_norm)
###############################

sa_method_norm1 <- function(score_name){
    score_pos <- sum(score_name[score_name >0], na.rm = TRUE)
    score_neg <- sum(score_name[score_name <0], na.rm = TRUE)
    score_adjust <- score_neg/score_pos
}

sa_method_norm2 <- function(score_name1, score_name2){
  score_pos <- sum(score_name1[score_name1 >0], na.rm = TRUE)
  score_neg <- -1* sum(score_name2[score_name2 <0], na.rm = TRUE)
  score_adjust <- score_neg/score_pos
}


afinn_posneg_norm1 <- sa_method_norm1(num_cols$afinn_score)
bing_posneg_norm1 <- sa_method_norm1(num_cols$bing_score)
syuzhet_posneg_norm1 <- sa_method_norm1(num_cols$syuzhet_score)
nrc_posneg_norm1 <- sa_method_norm2(num_cols$nrc_score_positive, num_cols$nrc_score_negative)
loughran_posneg_norm1 <- sa_method_norm2(num_cols$loughran_frame_positive, num_cols$loughran_frame_negative)
