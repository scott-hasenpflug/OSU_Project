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

Headcount <- function(var, var2 = NULL, var3 = NULL, var4 = NULL, 
                      sort = Headcount) {
  osu %>%
    group_by({{var}}, {{var2}}, {{var3}}, {{var4}}) %>%
    summarize(Headcount = length(unique(name.full)),
              FTE = sum(percent.time)/100,
              MedianSalary = median(pay.annual.adj, na.rm = TRUE),
              AverageTenure = mean(tenure.yrs)) %>%
    arrange(desc({{sort}}))
}

#*******************************************************************************
# 3 - Customer Requests
#*******************************************************************************

# Headcount/FTE ------------------------------------------------------------

# Headcount
#   I wrote a function: Headcount

# Examples:
Headcount(type.employee)
Headcount(type.employee, full.time)
Headcount(type.employee, full.time, loaner)
Headcount(type.employee, full.time, loaner, job.type)
Headcount(type.employee, full.time, loaner, job.type, sort = MedianSalary)
Headcount(home.org, sort = FTE)

# Gender Splits ----------------------------------------------------------------
#   Examine differences in gender. Try to find out why Unknown genders (unusual 
#   first names) get paid more 

# Group by gender and staff/faculty, look at pay
osu %>%
  group_by(gender, type.employee) %>%
  summarize(Median_Salary = median(pay.annual.adj, na.rm = TRUE),
            Average_Salary = mean(pay.annual.adj, na.rm = TRUE),
            Count = n()) %>%
  arrange(desc(Median_Salary))
# Faculty of unknown gender make the most. Interesting, but relatively few of 
# them. The rest rank as you would assume

# Get the gender split by employee type.
osu %>%
  group_by(type.employee) %>%
  summarize(Male = percent(sum(gender == "Male")/n(), .1),
            Female = percent(sum(gender == "Female")/n(), .1),
            Unknown = percent(sum(gender == "Unknown")/n(), .1),
            Count = n())
# Faculty is less female dominated than staff and has more "unknowns"

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

# Tenure v Salary --------------------------------------------------------------
# More granular salary and tenure bins -----------------------------------------
# Salary of professor v other instructor by employment length ------------------
# Is the % of tenured faculty consistent across units? -------------------------
# Pay rates for different types of faculty -------------------------------------
# Do classified pay rates correlate with tenure --------------------------------
# Overlap in compensation rates across classified and unclassified -------------
# Compare IT compensation specifically -----------------------------------------
# Director position title inflation --------------------------------------------
# Distribution of salaries (by bin) of Director titles vs. Manager -------------
# Percentage of titles: Manager, Director, Executive Director, AVP/VP ----------
# How many VPs and Deans (senior leaders) have been there over 5 years? --------
# Where has ther most hiring occured in the last 2 years (by unit and job)? ----
# What is the ratio of professors to instructors in all colleges? --------------
# What colleges have the highest salaries for professors? ----------------------
# Network diagram - What orgs hire from what orgs ------------------------------
# Network diagram - Relationship between Extension Service and College ---------
# Range of compensation and tenure for exec/admin assistants -------------------
# Overlap in compensation rates staff/faculty ----------------------------------
# Are IT positions across staff/faculty compensated equally? -------------------
# Do pay rates for staff in same title correlate with tenure? ------------------
# Most common staff titles? ----------------------------------------------------
# Headcount visual per school by job bracket and employee class ----------------
# Is grad student info anywhere in this dataset? -------------------------------
# Can I break out tenure/tenure track? -----------------------------------------
# Add title of person's supervisor to some degree, maybe just school prez ------
