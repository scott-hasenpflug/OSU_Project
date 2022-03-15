#*******************************************************************************
# OSU Exploratory Analysis
# Scott Hasenpflug
# hasenpflug7@gmail.com
# 14MAR2022
#*******************************************************************************

#*******************************************************************************
# 0 - Load libraries
#*******************************************************************************

# Library() calls --------------------------------------------------------------

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
library(formattable)

# End Section 0 ----

#*******************************************************************************
# 1 - Source Data Frames 
#*******************************************************************************

# Load -------------------------------------------------------------------------
load("osu_df.RData") 

# Un-comment if you want appointees in the data set
#load("osu_wth_appt_df.RData")

# End Section 1 ----

#*******************************************************************************
# 2 - Custom Functions
#*******************************************************************************

# Complete ---------------------------------------------------------------------
report_head <- function(group1 = home.category, group2 = NULL, group3 = NULL, 
                        group4 = NULL, sort = headcount) {
  osu %>%
    group_by({{group1}}, {{group2}}, {{group3}}, {{group4}}) %>%
    summarize(headcount = length(unique(name.full)),
              fte = sum(percent.time)/100,
              median_pay = median(pay.annual.adj, na.rm = TRUE),
              average_tenure = mean(tenure.yrs)) %>%
    arrange(desc({{sort}}))
}

report_spend <- function(group = home.category, sort = cost) {
  budget <- sum(osu$pay.annual.adj)
  osu %>%
    group_by({{group}}) %>%
    summarise(cost = accounting(sum(pay.annual.adj),0), 
              share = percent(sum(pay.annual.adj)/sum(budget)),
              fte = accounting(sum(pay.annual.adj)/(sum(percent.time)/100),0),
              head = accounting(sum(pay.annual.adj)/length(unique(name.full)),0)) %>%
    relocate({{sort}}, .after = {{group}}) %>%
    arrange(desc({{sort}}))
}

# Incomplete -------------------------------------------------------------------

report_tenure <- function(var) {
  # Action
}

# End Section 2 ----

#*******************************************************************************
# 3 - Customer Requests
#*******************************************************************************

# Headcount/FTE * ------------------------------------------------------------

# Headcount
#   I wrote a function: report_head

# Examples:
report_head()
report_head(type.employee)
report_head(type.employee, full.time)
report_head(type.employee, full.time, loaner)
report_head(type.employee, full.time, loaner, job.type)
report_head(type.employee, full.time, loaner, job.type, sort = median_pay)
report_head(sort = average_tenure)

# Gender Splits * ----------------------------------------------------------------
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

#*******************************************************************************
# 4 - My Exploration
#*******************************************************************************

# Check for duplicate full names -----------------------------------------------
osu$name.full[duplicated(osu$name.full)]

#doesn't work because of multiple jobs. Come back later'

# Spending report * --------------------------------------------------------------
#   I wrote a function: report_spend
#   Takes two arguments: Group variable, and sort by: (share, cost, fte, head)

# Examples:
report_spend()
report_spend(type.employee)
report_spend(gender, share)


# Salary model -----------------------------------------------------------------

# Original model
mdl_pay_osu <- lm(pay.annual.adj ~ gender + type.employee + tenure.yrs +
                  loaner + percent.time + rank.admin + rank.acad + job.category
                  + senior + job.type, osu)

# Filter out head football coach
osu_no_hfc <- osu %>%
  filter(name.full != "Smith, Jonathan C")

# Filter out top paid in athletics (mostly coaches but captures AD and astaff)
osu_no_top_coaches <-  osu %>%
  filter(job.code != "YIA" | pay.annual.adj < 150000)

# Filter out all in athletic department
osu_no_YIA <-  osu %>%
  filter(job.code != "YIA")

# Original model
mdl_pay_osu <- lm(pay.annual.adj ~ gender + type.employee + tenure.yrs +
                  loaner + percent.time + rank.admin + rank.acad + job.category 
                  + senior + job.type, osu)

mdl_pay_osu %>%
  glance() %>%
  pull("adj.r.squared")
# 0.510914

# Model no head football coach
mdl_pay_no_hfc <- lm(pay.annual.adj ~ gender + type.employee + tenure.yrs +
                       loaner + percent.time + rank.admin + rank.acad + job.category + 
                       senior + job.type, osu_no_hfc)

mdl_pay_no_hfc %>%
  glance() %>%
  pull("adj.r.squared")
# 0.6515625

# Model no coaches
mdl_pay_no_coaches <- lm(pay.annual.adj ~ gender + type.employee + tenure.yrs +
                           loaner + percent.time + rank.admin + rank.acad + job.category + 
                           senior + job.type, osu_no_top_coaches)

mdl_pay_no_coaches %>%
  glance() %>%
  pull("adj.r.squared")
# 0.745358

# Model no athletic department
mdl_pay_no_YIA <- lm(pay.annual.adj ~ gender + type.employee + tenure.yrs +
                       loaner + percent.time + rank.admin + rank.acad + job.category + 
                       senior + job.type, osu_no_YIA)

mdl_pay_no_YIA %>%
  glance() %>%
  pull("adj.r.squared")
# 0.7493115

# Massive improvements when removing HFC and then top coaches from the model.
#   I don't need to do this though, I just need to capture their coach status
#   in admin.rank, and possibly their association with football specifically,
#   then re-run the model. Eliminating all of YIA did not improve the model
#   after I had removed the top coaches.

# Next: Go back to script 5 and capture in rank.admin: 
#     (Sr) Assoc ADs
#     (Asst/Assoc) Head coach or other top coaches
#     Maybe a logical for "football"
