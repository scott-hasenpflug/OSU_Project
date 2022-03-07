#############################################################
## Exploratory analysis of Classified and Unclassified data sets
## Scott Hasenpflug
## hasenpflug7@gmail.com
## 06MAR2022
#############################################################

##############################
# 0 - Load libraries
##############################
library(tidyverse)
library(pdftools)
library(rebus)
library(lubridate)
library(tidytext)
library(skimr)
library(scales)
library(knitr)

############################## 
# 1 - Source files 
##############################
load("classified_df.RData") 
load("unclassified_df.RData")
