#*******************************************************************************
# All OSU Join and Wrangle
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

load("faculty_df2.RData") 
load("staff_df2.RData")

#*******************************************************************************
# 2 - Join and Wrangle
#*******************************************************************************

# Join combined data frame -----------------------------------------------------

# Join staff and faculty
osu_wth_appt <- full_join(staff, faculty)
rm(staff, faculty)

# Wrangle combined data frame --------------------------------------------------

# Notice appt_end_date is a character variable. Fix
osu_wth_appt <- osu_wth_appt %>%
        mutate(appt_end_date = dmy(appt_end_date))

# I like the type variable. Let's put appointee as a 3rd type
osu_wth_appt <- osu_wth_appt %>%
        mutate(type = case_when(
                appointee == TRUE ~ "Appointee",
                TRUE ~ as.character(type)))

# I think a part-time/full-time logical variable would be useful
osu_wth_appt <- osu_wth_appt %>%
        mutate(full_time = case_when(
                appt_percent == 100 ~ TRUE,
                TRUE ~ FALSE))

# I think a "loaner" logical variable would be useful if an employee works 
# outside of their home org

osu_wth_appt <- osu_wth_appt %>%
        mutate(loaner = case_when(
                job_org_code != home_org_code ~ TRUE,
                TRUE ~ FALSE))

# Rearrange the variables. First: Critical info. Followed by: Logical, Date, 
#   Numeric, Factor, Character

# Define variable categories
cols_critical <- c("full_name", "type", "job_title", "salary_adj", 
                   "annual_salary_rate", "tenure", "job_category", "loaner", 
                   "full_time", "rank_admin", "rank_academic", "ID")
cols_logical <- c("has_tenure", "appointee", "senior_designation")
cols_date <- c("first_hired", "adj_service_date", "rank_effective_date", 
          "appt_begin_date", "appt_end_date")
cols_numeric <- c("monthly_salary_rate", "hourly_salary_rate", "appt_percent", 
                  "posn_suff", "monthly_salary_equivalent")
cols_factor <- c("tenure_bin", "salary_bin")
cols_character <- c("job_type", "first_name", "last_name", "home_org_code",
                    "job_org_code", "home_orgn", "home_category",
                    "job_orgn", "months_length", "rank_name")

# Reorder
osu_wth_appt <- osu_wth_appt %>%
        relocate(cols_critical, cols_logical, cols_date, cols_numeric, 
                 cols_factor, cols_character)

# Remove variable categories
rm(cols_critical, cols_logical, cols_date, cols_numeric, 
   cols_factor, cols_character)

# Make column names shorter and correct style
osu_wth_appt <- osu_wth_appt %>%
        rename(
                name.full = full_name,
                type.employee = type,
                job.title= job_title,
                pay.actual = salary_adj,
                tenure.yrs = tenure,
                job.category = job_category,
                loaner = loaner,
                full.time = full_time,
                rank.admin = rank_admin,
                rank.acad= rank_academic,
                id = ID,
                tenure.log = has_tenure,
                appointee = appointee,
                senior = senior_designation,
                first.hired = first_hired,
                date.adj.serv = adj_service_date,
                date.rank.eff = rank_effective_date,
                date.appt.begin = appt_begin_date,
                date.appt.end = appt_end_date,
                pay.annualized = annual_salary_rate,
                pay.monthly= monthly_salary_rate,
                pay.hourly = hourly_salary_rate,
                percent.time = appt_percent,
                posn.code = posn_suff,
                pay.monthly.equiv = monthly_salary_equivalent,
                bin.tenure = tenure_bin,
                bin.pay = salary_bin,
                job.type = job_type,
                name.first = first_name,
                name.last = last_name,
                home.code = home_org_code,
                job.code = job_org_code,
                home.org = home_orgn,
                home.category = home_category,
                job.org = job_orgn,
                contract.length= months_length,
                rank.name = rank_name)

# Fix one instance of "Emerita" job title causing incorrect appointee status
osu_wth_appt <- osu_wth_appt %>%
        mutate(appointee = case_when(
                id == "F-1785" ~ TRUE,
                TRUE ~ appointee
        ))

osu_wth_appt <- osu_wth_appt %>%
        mutate(appointee = case_when(
                type.employee == "Staff" ~ FALSE,
                TRUE ~ appointee
        ))

# Change Staff tenure.log from NA to FALSE
osu_wth_appt <- osu_wth_appt %>%
        replace_na(list(tenure.log = FALSE))

# Some first names are actually middle initials. Fix
osu_wth_appt <- osu_wth_appt %>%
        mutate(name.first = case_when(
                str_length(osu_wth_appt$name.first) == 1 ~ str_match(
                        osu_wth_appt$name.full, pattern = SPC %R% WRD %R% SPC 
                        %R% capture(one_or_more(WRD)))[, 2],
                id == "F-2872" ~ "KJ",
                TRUE ~ as.character(name.first)
        ))

#Make a data.frame with the best guess for each first name present
#       Use a package called "gender"
gender_guess <- distinct(
        gender(osu_wth_appt$name.first, years = c(1950, 2004), method = "ssa", 
               countries = "United States")[, c("name", "gender")])

# Use left_join to give everyone their gender
osu_wth_appt <- left_join(osu_wth_appt, gender_guess, 
                          by = c("name.first" = "name")) %>%
        mutate(gender = str_to_title(gender)) %>%
        replace_na(list(gender = "Unknown")) %>%
        relocate(gender, .after = name.full)
# 345 could not be determined

# Try another method with the remainder
gender_guess2 <- osu_wth_appt %>%
        filter(gender== "Unknown") %>%
        distinct(
                gender(name.first, method = "genderize")[, c("name", "gender")])
# That found 261 of those 345. Only 16 are still unknown.

# Use left_join to give those 261  employees their gender
osu_wth_appt <- left_join(osu_wth_appt, gender_guess2, 
                           by = c("name.first" = "name"))

# Now have two gender columns. Fix
osu_wth_appt <- osu_wth_appt %>%
        mutate(gender.x = case_when(
                gender.x == "Unknown" ~ as.character(gender.y),
                TRUE ~ as.character(gender.x)),
               gender.x = str_to_title(gender.x)) %>%
        replace_na(list(gender.x = "Unknown")) %>%
        select(-gender.y) %>%
        rename(gender = gender.x)

# Check
osu_wth_appt %>% count(gender)

# Only 72 of 7712 (jobs, not necessarily people) have no gender. 0.93%
        
# Make KJ "Female" just because I happened to look her up. gender_guess2
#       assigned some of the one letter first names a gender. I'm fine with it
#       but I had already coded this previously.
osu_wth_appt <- osu_wth_appt %>%
        mutate(name.first = replace(
                name.first, name.first == "J", "KJ")) %>%
        mutate(gender = case_when(
                id == "F-2872" ~ "Female",
                TRUE ~ as.character(gender)
        ))
# Clean NAs --------------------------------------------------------------------

osu_wth_appt["pay.annualized"][is.na(osu_wth_appt["pay.annualized"])] <- 0
osu_wth_appt["rank.admin"][is.na(osu_wth_appt["rank.admin"])] <- "None"
osu_wth_appt["rank.acad"][is.na(osu_wth_appt["rank.acad"])] <- "None"
osu_wth_appt["rank.name"][is.na(osu_wth_appt["rank.name"])] <- "No Rank"
osu_wth_appt["senior"][is.na(osu_wth_appt["senior"])] <- FALSE
osu_wth_appt["name.first"][is.na(osu_wth_appt["name.first"])] <- "Shamsunnahar"

# Split and save the data frame -----------------------------------------------

save(osu_wth_appt, file = "osu_wth_appt_df.RData")
