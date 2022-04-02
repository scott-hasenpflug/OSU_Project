#*******************************************************************************
# Wrangle Staff
# Scott Hasenpflug
# hasenpflug7@gmail.com
# 12MAR2022
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
# 1 - Source Data Frame 
#*******************************************************************************

load("staff_df.RData") 

#*******************************************************************************
# 2 - Wrangle
#*******************************************************************************

# Fix columns ------------------------------------------------------------------

# Coerce object types as appropriate
staff <- staff %>%
        mutate(appt_percent = as.numeric(as.character(appt_percent)))

# Add columns of separated first and last names (drops middle and suffixes), keep full names as full_name 
staff <- staff %>%
        mutate(full_name = name) %>%
        separate(name, c("last_name", "first_name"), sep = ", ") %>%
        mutate(first_name = word(first_name))

# Clean up months_length
staff <- staff %>%
        mutate(months_length = if_else(months_length != "-Intermittent (J)", months_length, "Intermittent")) %>%
        mutate(months_length = if_else(months_length != "-Limited Duration (J", months_length, "Limited")) %>%               
        mutate(months_length = if_else(months_length != "-Seasonal (J)", months_length, "Seasonal")) %>%
        mutate(months_length = if_else(months_length != "12-month", months_length, "12")) %>%
        mutate(months_length = if_else(months_length != "10-month", months_length, "10")) %>%
        mutate(months_length = if_else(months_length != "9-month", months_length, "9")) 

# Convert salaries from character to numeric
staff$monthly_salary_rate <- as.numeric(as.character(staff$monthly_salary_rate))
staff$hourly_salary_rate <- as.numeric(as.character(staff$hourly_salary_rate))

# Convert dates from strings
staff <- staff %>%
        mutate(first_hired = dmy(first_hired), 
               adj_service_date = dmy(adj_service_date))

# Create some variables --------------------------------------------------------

# First designate these as Faculty

staff <- staff %>%
        mutate(type = "Staff")

# Create organization code variables
staff <- staff %>%
        mutate(home_org_code = str_extract(home_orgn, pattern = START %R% WRD %R% WRD %R% WRD),
               job_org_code = str_extract(job_orgn, pattern = START %R% WRD %R% WRD %R% WRD))

# Create IDs
staff <- staff %>%
        mutate(ID = paste0("S-", seq(1: nrow(staff))))

# Create hourly and annual salary equivalent (assume 120 hr month, 12 month yr)
staff <- staff %>%
        mutate(monthly_salary_equivalent = hourly_salary_rate * 120) %>%
        mutate(monthly_salary_equivalent = case_when(
                is.na(monthly_salary_equivalent) ~ monthly_salary_rate,
                TRUE ~ monthly_salary_equivalent)) %>%
        mutate(monthly_salary_equivalent = monthly_salary_equivalent * (appt_percent/100)) %>%
        mutate(monthly_salary_equivalent = round(monthly_salary_equivalent, 2)) %>%
        mutate(salary_adj = monthly_salary_equivalent *12)

# Distribute salary_adj into bins

staff <- staff %>%
        mutate(salary_bin = case_when(
                is.na(salary_adj) ~ "Unpaid",
                between(salary_adj, 0, 45000) ~ "Very Low", 
                between(salary_adj, 45001, 58000) ~ "Low",
                between(salary_adj, 58001, 100000) ~ "Medium",
                between(salary_adj, 100001, 150000) ~ "High",
                between(salary_adj, 150001, 300000) ~ "Very High",
                TRUE ~ "Top Tier")) %>%
        mutate(salary_bin = factor(salary_bin, 
                                   levels = c("Unpaid", "Very Low", 
                                              "Low", "Medium", "High", 
                                              "Very High", "Top Tier")))

# Distribution of tenure into 5 yr bins

# Create an integer tenure metric
staff <- staff %>%
        mutate(tenure = floor((as.numeric((today() - first_hired)/365))))

# Create labels for the 5 yr tenure bins
tenure_bin_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40+")

# Create a column of the bin value based on tenure
staff <- staff %>%
        mutate(tenure_bin = case_when(
                between(tenure, 0, 4.999) ~ "0-4", 
                between(tenure, 5, 9.999) ~ "5-9",
                between(tenure, 10, 14.999) ~ "10-14",
                between(tenure, 15, 19.999) ~ "15-19",
                between(tenure, 20, 24.999) ~ "20-24",
                between(tenure, 25, 29.999) ~ "25-29",
                between(tenure, 30, 34.999) ~ "30-34",
                between(tenure, 35, 39.999) ~ "35-39",
                TRUE ~ "40+")) %>%
        mutate(tenure_bin = factor(tenure_bin, levels = tenure_bin_labels))

# Add job and home categories --------------------------------------------------

# Home Category

# Snag all 11 Colleges:
S_agriculture <- startsWith(staff$home_org_code, "A")
S_business <-  startsWith(staff$home_org_code, "B")
S_libarts <- startsWith(staff$home_org_code, "C")
S_engineering <- startsWith(staff$home_org_code, "E")
S_forestry <- startsWith(staff$home_org_code, "F")
S_health <- startsWith(staff$home_org_code, "H")
S_education <- startsWith(staff$home_org_code, "K")
S_earth <- startsWith(staff$home_org_code, "O")
S_pharmacy <- startsWith(staff$home_org_code, "P")
S_science <- startsWith(staff$home_org_code, "S")
S_vet <- startsWith(staff$home_org_code, "V")

# ## Other divisions:
S_affairs <- startsWith(staff$home_org_code, "M")
S_athletics <- startsWith(staff$home_org_code, "Y")
S_tech <- startsWith(staff$home_org_code, "J")
S_research  <- startsWith(staff$home_org_code, "R") | staff$home_org_code == "XMS"
S_marketing  <- startsWith(staff$home_org_code, "N")
S_extension <- startsWith(staff$home_org_code, "T")
S_enrollment <- startsWith(staff$home_org_code, "XEM")
S_library <- startsWith(staff$home_org_code, "DLB") 
S_finance <- startsWith(staff$home_org_code, "Q") | staff$home_org_code == "ZSS"
S_president <- startsWith(staff$home_org_code, "U")
S_graduate <- startsWith(staff$home_org_code, "G")
S_provost <- staff$home_org_code %in% c("XPV", "WHC", "INT", "XUS")

## Other locations:
S_cascades  <- startsWith(staff$home_org_code, "L")
S_ecampus <- startsWith(staff$home_org_code, "XEC")

staff <- staff %>%
        mutate(home_category = case_when(
                S_agriculture ~ "College of Agriculture",
                S_business ~ "College of Business",
                S_libarts ~ "College of Liberal Arts",
                S_engineering ~ "College of Engineering",
                S_forestry ~ "College of Forestry",
                S_health ~ "College of Public Health and Human Services",
                S_education ~ "College of Education",
                S_earth ~ "College of Earth, Ocean, and Atmospheric Sciences",
                S_pharmacy ~ "College of Pharmacy",
                S_science ~ "College of Science",
                S_vet ~ "College of Veterinary Medicine",
                S_affairs ~ "Division of Student Affairs",
                S_athletics ~ "Intercollegiate Athletics",
                S_tech ~ "University Information and Technology",
                S_research ~ "Research Office",
                S_marketing ~ "University Relations and Marketing",
                S_cascades ~ "OSU - Cascades",
                S_extension ~ "Extension and Engagement",
                S_ecampus ~ "Ecampus",
                S_enrollment ~ "Enrollment Management",
                S_library ~ "Library",
                S_finance ~ "Finance and Administration",
                S_president ~ "Office of the President",
                S_graduate ~ "Graduate School",
                S_provost ~ "Office of the Provost",
                TRUE ~ "Other")
        )

# Job Category

# Snag all 11 Colleges:
SJ_agriculture <- startsWith(staff$job_org_code, "A")
SJ_business <-  startsWith(staff$job_org_code, "B")
SJ_libarts <- startsWith(staff$job_org_code, "C")
SJ_engineering <- startsWith(staff$job_org_code, "E")
SJ_forestry <- startsWith(staff$job_org_code, "F")
SJ_health <- startsWith(staff$job_org_code, "H")
SJ_education <- startsWith(staff$job_org_code, "K")
SJ_earth <- startsWith(staff$job_org_code, "O")
SJ_pharmacy <- startsWith(staff$job_org_code, "P")
SJ_science <- startsWith(staff$job_org_code, "S")
SJ_vet <- startsWith(staff$job_org_code, "V")

# ## Other divisions:
SJ_affairs <- startsWith(staff$job_org_code, "M")
SJ_athletics <- startsWith(staff$job_org_code, "Y")
SJ_tech <- startsWith(staff$job_org_code, "J")
SJ_research  <- startsWith(staff$job_org_code, "R") | staff$job_org_code == "XMS"
SJ_marketing  <- startsWith(staff$job_org_code, "N")
SJ_extension <- startsWith(staff$job_org_code, "T")
SJ_enrollment <- startsWith(staff$job_org_code, "XEM")
SJ_library <- startsWith(staff$job_org_code, "DLB") 
SJ_finance <- startsWith(staff$job_org_code, "Q") | staff$job_org_code == "ZSS"
SJ_president <- startsWith(staff$job_org_code, "U")
SJ_graduate <- startsWith(staff$job_org_code, "G")
SJ_provost <- staff$job_org_code %in% c("XPV", "WHC", "INT", "XUS")

## Other locations:
S_cascades  <- startsWith(staff$job_org_code, "L")
S_ecampus <- startsWith(staff$job_org_code, "XEC")

staff <- staff %>%
        mutate(job_category = case_when(
                S_agriculture ~ "College of Agriculture",
                S_business ~ "College of Business",
                S_libarts ~ "College of Liberal Arts",
                S_engineering ~ "College of Engineering",
                S_forestry ~ "College of Forestry",
                S_health ~ "College of Public Health and Human Services",
                S_education ~ "College of Education",
                S_earth ~ "College of Earth, Ocean, and Atmospheric Sciences",
                S_pharmacy ~ "College of Pharmacy",
                S_science ~ "College of Science",
                S_vet ~ "College of Veterinary Medicine",
                S_affairs ~ "Division of Student Affairs",
                S_athletics ~ "Intercollegiate Athletics",
                S_tech ~ "University Information and Technology",
                S_research ~ "Research Office",
                S_marketing ~ "University Relations and Marketing",
                S_cascades ~ "OSU - Cascades",
                S_extension ~ "Extension and Engagement",
                S_ecampus ~ "Ecampus",
                S_enrollment ~ "Enrollment Management",
                S_library ~ "Library",
                S_finance ~ "Finance and Administration",
                S_president ~ "Office of the President",
                S_graduate ~ "Graduate School",
                S_provost ~ "Office of the Provost",
                TRUE ~ "Other"))

# Good for now ----------------------------------------------------------------
save(staff, file = "staff_df2.RData")
