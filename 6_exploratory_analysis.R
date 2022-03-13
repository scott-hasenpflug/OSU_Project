#*******************************************************************************
# OSU Exploratory Analysis
# Scott Hasenpflug
# hasenpflug7@gmail.com
# 13MAR2022
#*******************************************************************************

#*******************************************************************************
# 0 - Load libraries
#*******************************************************************************

library(tidyverse)
library(pdftools)
library(rebus)
library(lubridate)
library(tidytext)
library(skimr)
library(scales)
library(knitr)
library(stringr)
library(tibble)

#*******************************************************************************
# 1 - Source Data Frames 
#*******************************************************************************

load("osu_df.RData") 

# Un-comment if you want appointees in the data set
#load("osu_wth_appt_df.RData")

#*******************************************************************************
# 2 - Write Functions
#*******************************************************************************

my_fun <- function(arg1, arg2) {
  # Do something
}
#*******************************************************************************
# 3 - Explore
#*******************************************************************************

# Look at the structure of the data frame
str(osu)
glimpse(osu)
skim(osu)

