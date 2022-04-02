#*******************************************************************************
# Wrangle Faculty
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

load("faculty_df.RData") 

#*******************************************************************************
# 2 - Wrangle
#*******************************************************************************

# Fix columns ------------------------------------------------------------------

# Coerce object types as appropriate
faculty <- faculty %>%
        mutate(annual_salary_rate = as.numeric(as.character(annual_salary_rate)),
               appt_percent = as.numeric(as.character(appt_percent)),
               months_length = as.character(months_length))

# Create first and last name columns, keep full name separately
faculty <- faculty %>%
        mutate(full_name = name) %>%
        separate(name, c("last_name", "first_name"), sep = ", ") %>%
        mutate(first_name = word(first_name))

# Based on how we built the data frame, employee's additional jobs have NAs for 
# four variables. Luckily they just need the values directly above them. Fix.
faculty <- faculty %>% 
        fill(last_name, first_name, first_hired, home_orgn, full_name)

# Transform character vectors containing dates into Date objects
faculty <- faculty %>%
        mutate(first_hired = dmy(first_hired), 
               adj_service_date = dmy(adj_service_date),
               rank_effective_date = dmy(rank_effective_date),
               appt_begin_date = dmy(appt_begin_date))

# Transform certain variables to factors and set levels. I will kill these later
# but won't touch them retroactively in case it breaks something.
faculty <- faculty %>%
        mutate(job_type = factor(job_type, levels = c("P", "S", "O"))) %>%
        mutate(months_length = factor(months_length, levels = c("9", "12")))

# Create a column for three letter code preceding home organization to better 
# simplify organizations
faculty <- faculty %>%
        mutate(home_org_code = str_extract(
                home_orgn, pattern = START %R% WRD %R% WRD %R% WRD))

# I discovered some typos unless some people were hired in the 3rd century. Fix.
faculty <- faculty %>%
        mutate(appt_begin_date = replace(
                appt_begin_date, appt_begin_date == "0221-01-01", "2021-01-01")) %>%
        mutate(adj_service_date = replace(
                adj_service_date, adj_service_date == "0200-05-04", "2020-05-04")) %>%
        mutate(home_orgn = replace(
                home_orgn, home_orgn == "***Change to 302000 eff 7/1/17", 
                "ENS - Sch Nuclear Sci & Engr")) %>%
        mutate(home_org_code = replace(home_org_code, is.na(home_org_code), "ENS"))

# Create some variables --------------------------------------------------------

# First designate these as Faculty

faculty <- faculty %>%
        mutate(type = "Faculty")

# Put faculty salary into bins
faculty <- faculty %>%
        mutate(salary_bin = case_when(
                is.na(annual_salary_rate) ~ "Unpaid",
                between(annual_salary_rate, 0, 45000) ~ "Very Low", 
                between(annual_salary_rate, 45001, 58000) ~ "Low",
                between(annual_salary_rate, 58001, 100000) ~ "Medium",
                between(annual_salary_rate, 100001, 150000) ~ "High",
                between(annual_salary_rate, 150001, 300000) ~ "Very High",
                TRUE ~ "Top Tier")) %>%
        mutate(salary_bin = factor(salary_bin, 
                                   levels = c("Unpaid", "Very Low", 
                                              "Low", "Medium", "High", 
                                              "Very High", "Top Tier")))

# Create an integer tenure metric
faculty <- faculty %>%
        mutate(tenure = floor((as.numeric((today() - first_hired)/365))))

# Label tenure bin levels
tenure_bin_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                       "35-39", "40+")

# Create tenure bins
faculty <- faculty %>%
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

# Reorder columns. I add many more later but I'm going to leave this alone for now
faculty <- faculty %>%
        relocate(last_name, first_name, job_title, job_orgn, annual_salary_rate, 
                 appt_percent, months_length, salary_bin, rank_name, job_type, 
                 home_orgn, tenure, tenure_bin, first_hired, adj_service_date,
                 rank_effective_date, appt_begin_date, appt_end_date, posn_suff,
                 home_org_code, full_name)

# Label appointees, academic tenure, tag if "senior"
faculty <- faculty %>%
        mutate(appointee = str_detect(
                faculty$job_title, pattern = or("Emeritus", 
                                                "Courtesy Appointment")),
               has_tenure = str_detect(faculty$rank_name, pattern = "Professor"),
               senior_designation = str_detect(faculty$rank_name, pattern = "Senior"))

# Create administrative ranks. I get much more detailed later.               
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                str_detect(faculty$job_title, 
                           pattern = or("Manager", "Mgr")) ~ "Manager",
                str_detect(faculty$job_title, 
                           pattern = or("Director", "Dir-", "Dir.")) ~ "Director",
                str_detect(faculty$job_title, 
                           pattern = or("President", "Pres.", "Pres-")) ~ "President",
                TRUE ~ "None")) %>%
        mutate(rank_admin = factor(
                rank_admin, levels = c("President", "Director", "Manager", "None")))

# Create academic ranks. I won't have to touch these again. At least I haven't yet
faculty <- faculty %>%
        mutate(rank_academic = case_when(
                str_detect(faculty$rank_name, 
                           pattern = "Assistant Professor") ~ "Assistant Professor",
                str_detect(faculty$rank_name, 
                           pattern = "No Rank") ~ "None",
                str_detect(faculty$rank_name, 
                           pattern = "Associate Professor") ~ "Associate Professor",
                str_detect(faculty$rank_name, pattern = "Instructor") ~ "Instructor",
                str_detect(faculty$rank_name, pattern = "Fellow") ~ "Fellow",
                str_detect(faculty$rank_name, pattern = "Research") ~ "Researcher",
                str_detect(faculty$rank_name, pattern = "Lecturer") ~ "Lecturer",
                TRUE ~ "Professor")) %>%
        mutate(rank_academic = factor(
                rank_academic, levels = c("Professor", "Associate Professor", 
                                          "Assistant Professor", "Researcher",
                                          "Lecturer", "Instructor", "Fellow", "None")))

# Create a variable for what employees are actually paid if they're part time.
faculty <- faculty %>%
        mutate(salary_adj = (annual_salary_rate * appt_percent)/100)

# Clean up "Executives" --------------------------------------------------------

## Look at all the titles
faculty %>%
        filter(rank_admin != "None") %>%
        count(rank_admin)

## Look at "Presidents"
faculty %>%
        filter(rank_admin == "President") %>%
        select(full_name, job_title, annual_salary_rate)

## Some Presidents" suspicious. Subset and show salary, descending
faculty %>%
        filter(rank_admin == "President") %>%
        select(full_name, job_title, annual_salary_rate) %>%
   # The bottom 4 should not be executives at all. Pull names as "dead_presidents"
        arrange(desc(annual_salary_rate)) %>%
        filter(annual_salary_rate < 120000 | is.na(annual_salary_rate)) %>%
        pull(full_name) -> dead_presidents

## Give them rank "None
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                faculty$full_name %in% dead_presidents ~ "None",
                TRUE ~ as.character(rank_admin)))

##Check remaining
faculty %>%
        filter(rank_admin == "President") %>%
        select(full_name, job_title, annual_salary_rate)%>%
        arrange(desc(annual_salary_rate)) %>%
   # Good. Let's tag the VPs
        pull(full_name) %>%
        .[c(2, 3, 4, 6)] -> VPs

## Change their titles to "Vice President"
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                faculty$full_name %in% VPs & faculty$rank_admin == 
                        "President" ~ "Vice President",
                TRUE ~ as.character(rank_admin)))

## Tag "Associate VP"s
faculty %>%
        filter(rank_admin == "President") %>%
        select(full_name, job_title, annual_salary_rate)%>%
        arrange(desc(annual_salary_rate)) %>%
        pull(full_name) %>%
        .[2:4] -> assoc_VPs

## Change their titles to "Associate VP"
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                faculty$full_name %in% assoc_VPs & faculty$rank_admin == 
                        "President" ~ "Associate VP",
                TRUE ~ as.character(rank_admin)))

## Tag "Assistant VP"s
faculty %>%
        filter(rank_admin == "President") %>%
        select(full_name, job_title, annual_salary_rate)%>%
        arrange(desc(annual_salary_rate)) %>%
        pull(full_name) %>%
        .[2] -> assist_VPs

## Change their titles to "Assistant VP"
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                faculty$full_name %in% assist_VPs & faculty$rank_admin == 
                        "President" ~ "Assistant VP",
                TRUE ~ as.character(rank_admin)))

## Define the opposite of "%in%" because it's bugging me
`%!in%` <- Negate(`%in%`)

## Get a printout of these executives
faculty %>%
        filter(rank_admin %!in% c("None", "Director", "Manager")) %>%
        select(full_name, job_title, rank_admin, annual_salary_rate) %>%
        arrange(desc(annual_salary_rate))

# Clean up "Directors" ---------------------------------------------------------

## Look at the lowest paid "Directors"
faculty %>%
        filter(rank_admin == "Director") %>%
        select(full_name, home_orgn, job_title, annual_salary_rate) %>%
        arrange(annual_salary_rate)

## In lowest 200, "Resident Director"s and similar titles from HHS 
## need to be removed
faculty <- faculty %>%
        mutate(rank_admin = case_when(
        faculty$rank_admin == "Director" & str_detect(faculty$job_title, pattern = 
                        "Resident Director|Residential Director|Admin Assistant|
                        Area Dir|Residential Area Director|Assistant to|Exe Asst to") ~ "None",
                        TRUE ~ as.character(rank_admin)))

## That got rid of 22

## Graph the salary distribution of remaining (way too many) directors
faculty %>%
        filter(rank_admin %in% c("Director", "Manager")) %>%
        ggplot(aes(annual_salary_rate, fill = rank_admin)) +
        geom_histogram()

faculty %>%
        filter(rank_admin %in% c("Manager", "Director")) %>%
        ggplot(aes(reorder(rank_admin, annual_salary_rate), annual_salary_rate)) +
        geom_boxplot()

## Scott Barnes threw this all off. He's a VP not a Director. Fix:
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                faculty$full_name == "Barnes, Scott" ~ "Vice President", 
                TRUE ~ as.character(faculty$rank_admin)))

## Recheck executives
faculty %>%
        filter(rank_admin %!in% c("None", "Director", "Manager")) %>%
        select(full_name, job_title, rank_admin, annual_salary_rate) %>%
        arrange(desc(annual_salary_rate)) 

## Find Manager outliers
faculty %>%
        filter(rank_admin == "Manager") %>%
        arrange(desc(annual_salary_rate)) %>%
        select(full_name, job_title, home_orgn, annual_salary_rate) %>%
        head(10)
## Mostly Project and Business Center

faculty %>%
        filter(rank_admin == "Manager") %>%
        arrange(annual_salary_rate) %>%
        select(full_name, job_title, home_orgn, annual_salary_rate) %>%
        head(10)
## Mostly Housing & Dining

## Find Director outliers
faculty %>%
        filter(rank_admin == "Director") %>%
        arrange(desc(annual_salary_rate)) %>%
        select(full_name, job_title, home_orgn, annual_salary_rate) %>%
        head(10)
## Normal, except raises "Dean" question

faculty %>%
        filter(rank_admin == "Director") %>%
        arrange(annual_salary_rate) %>%
        select(full_name, job_title, home_orgn, annual_salary_rate) %>%
        head(10)
## These are mostly in athletics

## Boxplot, get rid of the football coach
faculty %>%
        filter(annual_salary_rate < 1500000) %>%
        ggplot(aes(reorder(rank_admin, annual_salary_rate), annual_salary_rate)) +
        geom_boxplot()

## Get rid of "Nones"
faculty %>%
        filter(rank_admin != "None") %>%
        ggplot(aes(reorder(rank_admin, annual_salary_rate), annual_salary_rate)) +
        geom_boxplot()

## Get rid of "Nones" (Includes football coach) and Athletic Director
AD_name <- "Barnes, Scott"
faculty %>%
        filter(rank_admin != "None", full_name != AD_name) %>%
        ggplot(aes(reorder(rank_admin, annual_salary_rate), annual_salary_rate)) +
        geom_boxplot() +
        scale_y_continuous(labels = comma) +
        labs(x = "Administrative Rank", y = "Annual Salary") +
        theme_minimal()

# The Deans --------------------------------------------------------------------

## Find them
deans <- faculty %>% 
        filter(str_detect(faculty$job_title, pattern = "Dean"), appointee == FALSE) %>%
## Holy shit there can't be 83 of them 
        arrange(annual_salary_rate) %>%
        pull(full_name)

## Quick manual scan, select the executive assistants as "dean_nied"
dean_nied <- deans[c(1:10, 12:16, 18, 21)]        

## Turn their administrative ranks to Dean except for dean_nied
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                str_detect(faculty$job_title, pattern = "Dean") & appointee == FALSE &
                        faculty$full_name %!in% dean_nied ~ "Dean",
                TRUE ~ as.character(rank_admin)))

## Pull names of "Deans" sorted by salary
deans2 <- faculty %>%
        filter(rank_admin == "Dean") %>%
        select(full_name, job_title, annual_salary_rate, tenure, home_orgn) %>%
        pull(full_name)

## Manually tag assistant and associates
assistant_deans <- deans2[c(46, 50, 55:58, 60:61, 63:66)]
associate_deans <- deans2[c(10, 13, 16:17, 19:20, 23:38, 40:45, 47:49, 51:54, 59, 62)]

## Rename them accordingly
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                full_name %in% associate_deans ~ "Associate Dean",
                full_name %in% assistant_deans ~ "Assistant Dean",
                TRUE ~ as.character(rank_admin)))

## Check titles again
faculty %>%
        count(rank_admin)

# Investigate Notable "Nones" --------------------------------------------------

faculty %>%
        filter(rank_admin == "None", rank_academic == "None", appointee == FALSE) %>%
        arrange(desc(annual_salary_rate)) %>%
        select(full_name, job_title, annual_salary_rate, home_orgn)

## Ok, they're sports coaches. Lets remove them
faculty %>%
        filter(rank_admin == "None", rank_academic == "None", appointee == FALSE,
               home_org_code != "YIA") %>%
        arrange(desc(annual_salary_rate)) %>%
        select(full_name, job_title, annual_salary_rate, home_orgn) 

## I need to account for "School Head", "Head"s, "Provost"s, and capture "VP"s

## The most common titles with "Head
faculty %>%
        filter(rank_admin == "None", home_org_code != "YIA") %>%
        filter(str_detect(job_title, pattern = "Head|head")) %>%
        count(job_title) %>%
        arrange(desc(n))

## Add two ranks
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                job_title == "Department Head" ~ "Department Head",
                job_title == "School Head" ~ "School Head",
                TRUE ~ as.character(rank_admin)))

## Are the remainder highly paid?
faculty %>%
        filter(rank_admin == "None", home_org_code != "YIA") %>%
        filter(str_detect(job_title, pattern = "Head|head")) %>%
        filter(annual_salary_rate > 80000) 

# Exec Assistant Interlude -----------------------------------------------------

## You know what I'm getting tired of executive assistants
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                str_detect(faculty$job_title, pattern = "Assistant to|
                        Executive Assist|Exec Asst|Executive Asst|ExecAsst|
                        Exec Assist|Exec. Assistant|Admin Ass|
                        Administrative Assistant|Assistant To|Asst to|
                        Executive Secretary|AsstBoard Sec") ~ "Executive Assistant",
                TRUE ~ as.character(rank_admin)))
faculty %>%
        count(rank_admin)

## Find some more
faculty %>%
        filter(rank_admin == "None", rank_academic == "None") %>%
        group_by(job_title) %>%
        select(job_title, annual_salary_rate) %>%
        arrange(annual_salary_rate)

faculty %>%
        filter(rank_admin == "Executive Assistant")

## Some at the top need to be removed (Customer Feedback)           
faculty <- faculty %>%
        mutate(rank_admin = case_when(full_name %in% c("Casey, Patrick M", 
                                "Bruce, Gigi Ann", "Humphreys, Jennifer M") ~ "None",
                                      TRUE ~ as.character(rank_admin)))
faculty <- faculty %>%
        mutate(rank_admin = case_when(rank_admin == "Executive Assistant" & 
                        str_detect(job_title, pattern = "Admin|ant to|sst to") & 
                                full_name != "Scholl, John" ~ "Administrative Assistant",
                                      TRUE ~ as.character(rank_admin)))

# Plotting Practice ------------------------------------------------------------

## Histogram of salaries between assistants
faculty %>%
        filter(rank_admin %in% c("Executive Assistant", "Administrative Assistant")) %>%
        ggplot(aes(rank_admin, annual_salary_rate)) + 
        geom_boxplot() +
        scale_y_continuous(labels = comma) +
        theme_minimal()

## Boxplot
faculty %>%
        filter(rank_admin %in% c("Executive Assistant", "Administrative Assistant")) %>%
        ggplot(aes(annual_salary_rate, fill = rank_admin)) + 
        geom_histogram() 

## Plot Salary vs Tenure w/ Trend line for Exec Ass
faculty %>%
        filter(rank_admin == "Executive Assistant") %>%
        ggplot(aes(tenure, annual_salary_rate)) +
        geom_point() +
        geom_smooth(method = lm, se = FALSE) +
        labs(title = "Executive Assistant", y = "Salary") +
        scale_y_continuous(labels = comma) +
        theme_minimal()

## Plot Salary vs Tenure w/ Trend line for Admin Ass        
faculty %>%
        filter(rank_admin == "Administrative Assistant") %>%
        ggplot(aes(tenure, annual_salary_rate)) +
        geom_point() +
        geom_smooth(method = lm, se = FALSE) +
        labs(title = "Administrative Assistant", y = "Salary") +
        scale_y_continuous(labels = comma) +
        theme_minimal()

# Lost VPs ---------------------------------------------------------------------

## I was having issues below, multiple jobs still throw me off when they have
## different titles. Realize I can create job IDs myself. Lets try.

## Give them an ID that is simply their row number in faculty data frame
faculty <- faculty %>%
        mutate(ID = seq(1: nrow(faculty))) %>%
        mutate(ID = as.character(ID))

##Filter out the paid "Nones"
Nones <-  faculty %>%
        filter(rank_admin == "None", appointee == FALSE)

## Logical vector if their title contains "VP"
lost_VPs <- str_detect(Nones$job_title, pattern = "VP")

## Temp data frame to look at them, descending by salary
lost_VPs_df <- Nones %>%
        filter(lost_VPs) %>%
        arrange(desc(annual_salary_rate))

## Manually decide who is what rank, pull ID instead of full_name
lost_VP_IDs <-lost_VPs_df %>%
        pull(ID)

## Collect IDs by new title
AVP <- lost_VP_IDs[c(2, 4:5, 7:11, 13:14)]
VP <- lost_VP_IDs[c(1, 3, 6, 12)]
assistant_VP <- lost_VP_IDs[15]

## Change their title based on their IDs
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                ID %in% VP ~ "Vice President",
                ID %in% AVP ~ "Associate VP",
                ID %in% assistant_VP ~ "Assistant VP",
                TRUE ~ as.character(rank_admin)))

## Check that it worked with one example
faculty %>%
        filter(full_name == "Vignos, Scott Anand") %>%
        select(rank_admin)

rm(lost_VPs_df, Nones)

## Check in on the executives
faculty %>%
        filter(rank_admin %in% c("Assistant VP", "Associate VP", "Vice President", 
                                 "President")) %>%
        select(full_name, rank_admin, annual_salary_rate) %>%
        arrange(desc(annual_salary_rate))

# Where's Your Head At?  -------------------------------------------------------

## Find paid job titles with "Head", not in athletics, and not any form of 
## "Advisor", and no current admin rank
head_IDs <- faculty %>%
        filter(rank_admin == "None", appointee == FALSE, home_org_code != "YIA",
               str_detect(job_title, pattern = "Head"),
               str_detect(job_title, pattern = "Advisor", negate = TRUE)) %>%
        arrange(desc(annual_salary_rate)) %>%
        pull(ID)

## What "Head" ranks have I made already?
faculty %>%
        count(rank_admin)
## School Head, Department Head

## Sort the IDS for tagging
assoc_head <- head_IDs[c(1, 2, 17)]
assoc_dept <- head_IDs[c(3, 6, 14, 18)]
assoc_school <- head_IDs[c(4, 5, 7, 9, 12, 15)]
dept_head <- head_IDs[c(8, 19, 20, 22, 23)]
school_head <- head_IDs[c(10, 11, 13, 16)]
assist_school <- head_IDs[c(21, 24)]

## Tag you're it
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                ID %in% assoc_head ~ "Associate Head",
                ID %in% assoc_dept ~ "Associate Department Head",
                ID %in% assoc_school ~ "Associate School Head",
                ID %in% dept_head ~ "Department Head",
                ID %in% school_head ~ "School Head",
                ID %in% assist_school ~ "Assistant School Head",
                TRUE ~ as.character(rank_admin)))

## Current Look at admin ranks
faculty %>%
        count(rank_admin, sort = T) 

# Who else is interesting? -----------------------------------------------------
faculty %>%
        filter(rank_admin == "None", rank_academic == "None", appointee == FALSE, 
               home_org_code != "YIA") %>%
        arrange(desc(annual_salary_rate)) %>%
        pull(ID) -> stragglers
## Chairs, Provosts

## Capture them
vice_provosts <- stragglers[c(2, 4, 6, 7)]
assoc_vice_prov <- stragglers[c(11, 22, 24, 69)]
assist_vice_prov <- stragglers[17]
chair <- c(916, 3029, 2716, 4096, 2892, 824, 3318, 2840)

## Final Director's Cut
faculty %>%
        filter(rank_admin == "None", rank_academic == "None", appointee == FALSE, 
               home_org_code != "YIA") %>% 
        filter(str_detect(job_title, pattern = "Dir|dir")) %>%
        arrange(desc(annual_salary_rate)) %>%
        pull(ID) %>%
        .[1:3] -> direc

## Assign these titles
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                ID %in% vice_provosts ~ "Vice Provost",
                ID %in% assoc_vice_prov ~ "Associate Vice Provost",
                ID %in% assist_vice_prov ~ "Assistant Vice Provost",
                ID %in% chair ~ "Chair",
                ID %in% direc ~ "Director",
                TRUE ~ as.character(rank_admin)))

## Look at our work
faculty %>% count(rank_admin, sort = T)
## I wonder if we still have too many directors and managers

faculty %>% filter(rank_admin == "Director")
## Yes.

## Get the assistant directors
asst_dir <- faculty %>% 
        filter(rank_admin == "Director", str_detect(job_title, "Asst|Assist")) %>%
        pull(ID)

## Get the associate directors
assoc_dir <- faculty %>% 
        filter(rank_admin == "Director", str_detect(job_title, pattern = "Asso")) %>%
        pull(ID)

## Assign them
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                ID %in% asst_dir ~ "Assistant Director",
                ID %in% assoc_dir ~ "Associate Director",
                TRUE ~ as.character(rank_admin)))

## Lets do the same for Managers
asst_mgr <- faculty %>% 
        filter(rank_admin == "Manager", str_detect(job_title, "Asst|Assist")) %>%
        pull(ID)

## Get the associate directors
assoc_mgr <- faculty %>% 
        filter(rank_admin == "Manager", str_detect(job_title, pattern = "Asso")) %>%
        pull(ID)

## Assign them
faculty <- faculty %>%
        mutate(rank_admin = case_when(
                ID %in% asst_mgr ~ "Assistant Manager",
                ID %in% assoc_mgr ~ "Associate Manager",
                TRUE ~ as.character(rank_admin)))

# Massive Org Cleaning ----------------------------------------------------------------

## Count the unique jobs
unique(faculty$home_orgn) # 276
unique(faculty$job_orgn) # 282
## They don't match

## Find isolated organizations
isolated_home_orgn <- faculty$home_orgn[faculty$home_orgn %!in% faculty$job_orgn]
length(isolated_home_orgn) # 97
isolated_job_orgn <- faculty$job_orgn[faculty$job_orgn %!in% faculty$home_orgn]        
length(isolated_job_orgn) # 45

## I already have home_org_code, lets get job_org_code
faculty <- faculty %>%
        mutate(job_org_code = str_extract(
                job_orgn, pattern = START %R% WRD %R% WRD %R% WRD))

## Count unique home and job codes
unique(faculty$home_org_code) # 106
unique(faculty$job_org_code) # Also 106 but they're different

## Find isolated codes
isolated_home_org_code <- faculty$home_org_code[faculty$home_org_code %!in% faculty$job_org_code]
length(isolated_home_org_code) # 1 - "APD - Aquaculture CRSP"
isolated_job_org_code <- faculty$job_org_code[faculty$job_org_code %!in% faculty$home_org_code]        
length(isolated_job_org_code) # 1 - "ABR - Bioresources Research"
## No idea if this matters yet

## Collect combined orgs and org codes
all_orgs <- union(faculty$home_orgn, faculty$job_orgn) # 293
all_org_codes <- union(faculty$home_org_code, faculty$job_org_code) # 107

## Ok this might be easier than I thought. All codes that start with A are 
## College of Agriculture. 

## ------------- Home Orgs first 

## Snag all 11 Colleges:
agriculture <- startsWith(faculty$home_org_code, "A")
business <-  startsWith(faculty$home_org_code, "B")
libarts <- startsWith(faculty$home_org_code, "C")
engineering <- startsWith(faculty$home_org_code, "E")
forestry <- startsWith(faculty$home_org_code, "F")
health <- startsWith(faculty$home_org_code, "H")
education <- startsWith(faculty$home_org_code, "K")
earth <- startsWith(faculty$home_org_code, "O")
pharmacy <- startsWith(faculty$home_org_code, "P")
science <- startsWith(faculty$home_org_code, "S")
vet <- startsWith(faculty$home_org_code, "V")

## Other divisions:
affairs <- startsWith(faculty$home_org_code, "M")
athletics <- startsWith(faculty$home_org_code, "Y")
tech <- startsWith(faculty$home_org_code, "J")
research  <- startsWith(faculty$home_org_code, "R") | faculty$home_org_code == "XMS"
marketing  <- startsWith(faculty$home_org_code, "N")
extension <- startsWith(faculty$home_org_code, "T")
enrollment <- startsWith(faculty$home_org_code, "XEM")
library <- startsWith(faculty$home_org_code, "DLB") 
finance <- startsWith(faculty$home_org_code, "Q") | faculty$home_org_code == "ZSS"
president <- startsWith(faculty$home_org_code, "U")
graduate <- startsWith(faculty$home_org_code, "G")
provost <- faculty$home_org_code %in% c("XPV", "WHC", "INT", "XUS", "DDA", "DAA")

## Other locations:
cascades  <- startsWith(faculty$home_org_code, "L")
ecampus <- startsWith(faculty$home_org_code, "XEC")

## Put them in a new column:
faculty <- faculty %>%
        mutate(home_category = case_when(
                agriculture ~ "College of Agriculture",
                business ~ "College of Business",
                libarts ~ "College of Liberal Arts",
                engineering ~ "College of Engineering",
                forestry ~ "College of Forestry",
                health ~ "College of Public Health and Human Services",
                education ~ "College of Education",
                earth ~ "College of Earth, Ocean, and Atmospheric Sciences",
                pharmacy ~ "College of Pharmacy",
                science ~ "College of Science",
                vet ~ "College of Veterinary Medicine",
                affairs ~ "Division of Student Affairs",
                athletics ~ "Intercollegiate Athletics",
                tech ~ "University Information and Technology",
                research ~ "Research Office",
                marketing ~ "University Relations and Marketing",
                cascades ~ "OSU - Cascades",
                extension ~ "Extension and Engagement",
                ecampus ~ "Ecampus",
                enrollment ~ "Enrollment Management",
                library ~ "Library",
                finance ~ "Finance and Administration",
                president ~ "Office of the President",
                graduate ~ "Graduate School",
                provost ~ "Office of the Provost",
                TRUE ~ "Other")
        )

## How many did I get?        
faculty %>%
        count(home_category, sort = TRUE)

## ---------------- Now Job Category 

## Snag all 11 Colleges:
J_agriculture <- startsWith(faculty$job_org_code, "A")
J_business <-  startsWith(faculty$job_org_code, "B")
J_libarts <- startsWith(faculty$job_org_code, "C")
J_engineering <- startsWith(faculty$job_org_code, "E")
J_forestry <- startsWith(faculty$job_org_code, "F")
J_health <- startsWith(faculty$job_org_code, "H")
J_education <- startsWith(faculty$job_org_code, "K")
J_earth <- startsWith(faculty$job_org_code, "O")
J_pharmacy <- startsWith(faculty$job_org_code, "P")
J_science <- startsWith(faculty$job_org_code, "S")
J_vet <- startsWith(faculty$job_org_code, "V")

## Other divisions:
J_affairs <- startsWith(faculty$job_org_code, "M")
J_athletics <- startsWith(faculty$job_org_code, "Y")
J_tech <- startsWith(faculty$job_org_code, "J")
J_research  <- startsWith(faculty$job_org_code, "R") | faculty$job_org_code == "XMS"
J_marketing  <- startsWith(faculty$job_org_code, "N")
J_extension <- startsWith(faculty$job_org_code, "T")
J_admissions <- startsWith(faculty$job_org_code, "XEM")
J_library <- startsWith(faculty$job_org_code, "DLB") 
J_finance <- startsWith(faculty$job_org_code, "Q") | faculty$job_org_code == "ZSS"
J_president <- startsWith(faculty$job_org_code, "U")
J_graduate <- startsWith(faculty$job_org_code, "G")
J_provost <- faculty$job_org_code %in% c("XPV", "WHC", "INT", "XUS", "DDA", "DAA")

## Other locations:
J_cascades  <- startsWith(faculty$job_org_code, "L")
J_ecampus <- startsWith(faculty$job_org_code, "XEC")

## Put them in a new column:
faculty <- faculty %>%
        mutate(job_category = case_when(
                J_agriculture ~ "College of Agriculture",
                J_business ~ "College of Business",
                J_libarts ~ "College of Liberal Arts",
                J_engineering ~ "College of Engineering",
                J_forestry ~ "College of Forestry",
                J_health ~ "College of Public Health and Human Services",
                J_education ~ "College of Education",
                J_earth ~ "College of Earth, Ocean, and Atmospheric Sciences",
                J_pharmacy ~ "College of Pharmacy",
                J_science ~ "College of Science",
                J_vet ~ "College of Veterinary Medicine",
                J_affairs ~ "Division of Student Affairs",
                J_athletics ~ "Intercollegiate Athletics",
                J_tech ~ "University Information and Technology",
                J_research ~ "Research Office",
                J_marketing ~ "University Relations and Marketing",
                J_cascades ~ "OSU - Cascades",
                J_extension ~ "Extension and Engagement",
                J_ecampus ~ "Ecampus",
                J_admissions ~ "Enrollment Management",
                J_library ~ "Library",
                J_finance ~ "Finance and Administration",
                J_president ~ "Office of the President",
                J_graduate ~ "Graduate School",
                J_provost ~ "Office of the Provost",
                TRUE ~ "Other")
        )

## Now how many don't match?
faculty %>%
        filter(appointee == FALSE) %>%
        filter(home_category != job_category) %>%
        group_by(home_category, job_category) %>%
        count(sort = T)

faculty %>% filter(home_category != job_category) %>%
        group_by(home_category, job_category) %>%
        count(sort = T)

# Last thing, change the IDs to start with "F-"

faculty <- faculty %>%
        mutate(ID = paste0("F-", ID))

## Good for now. 
save(faculty, file = "faculty_df2.RData")
