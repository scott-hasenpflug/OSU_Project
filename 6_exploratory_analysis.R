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
library(gender)

#*******************************************************************************
# 1 - Source Data Frames 
#*******************************************************************************

load("osu_df.RData") 

# Un-comment if you want appointees in the data set
#load("osu_wth_appt_df.RData")

#*******************************************************************************
# 2 - Write Functions
#*******************************************************************************

Headcount <- function(var, var2 = NULL, var3 = NULL, var4 = NULL, sort = Headcount) {
  osu %>%
    group_by({{var}}, {{var2}}, {{var3}}, {{var4}}) %>%
    summarize(Headcount = length(unique(name.full)),
              FTE = sum(percent.time)/100,
              MedianSalary = median(pay.annual.adj, na.rm = TRUE),
              AverageTenure = mean(tenure.yrs)) %>%
    arrange(desc({{sort}}))
}

#*******************************************************************************
# 3 - Explore
#*******************************************************************************

# Customer Requests ------------------------------------------------------------

# Headcount
#   I wrote a function: Headcount

Headcount(type.employee)
Headcount(type.employee, full.time)
Headcount(type.employee, full.time, loaner)
Headcount(type.employee, full.time, loaner, job.type)
Headcount(type.employee, full.time, loaner, job.type, sort = MedianSalary)
Headcount(home.org, sort = FTE)

# Try to find out why Unknown genders (unusual first names) get paid 
#   more. Maybe they're more likely to be professors?
osu %>%
  group_by(gender, type.employee) %>%
  summarize(Median_Salary = median(pay.annual.adj, na.rm = TRUE),
            Average_Salary = mean(pay.annual.adj, na.rm = TRUE),
            Count = n()) %>%
  arrange(desc(Median_Salary))


osu %>%
  group_by(type.employee) %>%
  summarize(Male = percent(sum(gender == "Male")/n(), .1),
            Female = percent(sum(gender == "Female")/n(), .1),
            Unknown = percent(sum(gender == "Unknown")/n(), .1),
            Count = n())

osu %>%
  group_by(tenure.log) %>%
  summarize(Male = percent(sum(gender == "Male")/n(), .1),
            Female = percent(sum(gender == "Female")/n(), .1),
            Unknown = percent(sum(gender == "Unknown")/n(), .1),
            Count = n())

osu %>%
  group_by(type.employee) %>%
  summarize(Male = percent(sum(gender == "Male")/n(), .1),
            Female = percent(sum(gender == "Female")/n(), .1),
            Unknown = percent(sum(gender == "Unknown")/n(), .1),
            Count = n())

osu %>%
  group_by(type.employee, tenure.log) %>%
  summarize(Male = percent(sum(gender == "Male")/n(), .1),
            Female = percent(sum(gender == "Female")/n(), .1),
            Unknown = percent(sum(gender == "Unknown")/n(), .1),
            Count = n())
