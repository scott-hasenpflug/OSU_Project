#*******************************************************************************
# Import Staff
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
# 1 - Import PDF
#*******************************************************************************

raw_import2 <- pdf_text("staff_pdf.pdf")

#*******************************************************************************
# 2 - Create Data Frame
#*******************************************************************************

# Make a vector of jobs --------------------------------------------------------
        
# I have a character vector of each page. Split off the header
split_header2 <- str_split(raw_import2, pattern = "\n\n\n")

# Remove the header
for (i in 1:length(split_header2)) {
        split_header2[[i]] <- split_header2[[i]][-1]    
}

# Unlist to split again
split_header2 <- unlist(split_header2)

# Split by dashed line. This separates individuals (but not jobs)
split_line2 <- split_header2 %>%
        str_split("---------------------------------------------------------------------------------")

# Remove first element in list if it is not a job
for (i in 1:length(split_line2)) {
        if (str_length(split_line2[[i]][1]) < 15){
                split_line2[[i]] <- split_line2[[i]][-1]    
        }
}

# Unlist to split again
split_line2 <- unlist(split_line2)

# Too long (multiple jobs)
split_line2[20]
split_line2[701]
split_line2[277]
split_line2[703]

# Too short (additional job cut off at top of page)
split_line2[702]

# Split again for additional jobs
temp2 <- split_line2 %>%
        str_split("\n\n")

# Unlist
temp2 <- unlist(temp2)

# We now have 3 types of strings: "    ", names, and jobs

# Remove "    "s
temp2 <- temp2[temp2 != "    "]

# Now I have character vectors with either a job or a name. Keep for now
one_job2 <- temp2

# Initialize a data frame ------------------------------------------------------

# Create initial variable names based on parsing plan
Variable_Names2 <- c("name", "first_hired", "home_orgn", "adj_service_date", "job_orgn", 
                    "job_type", "job_title", "posn_suff", "appt_percent", "months_length", 
                    "monthly_salary_rate", "hourly_salary_rate")

# Create dummy vector
dummy2 <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")

# Build the data frame
staff <- NULL
staff <- rbind(staff, dummy2)
colnames(staff) <- Variable_Names2

# Parse and bind to data frame -------------------------------------------------

# Loop over every vector in list one_job, extracting each variable, putting them into a 
# vector, and binding it to the bottom of the dataframe
for (i in 1:length(one_job2)) {
        name <- str_match(one_job2[i], pattern = "Name: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[,-1]
        first_hired <- str_match(one_job2[i], pattern = "First Hired: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        home_orgn <- str_match(one_job2[i], pattern = "Home Orgn: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% or(SPC, "Adj"))[,-1]
        adj_service_date <- str_match(one_job2[i], pattern = "Adj Service Date: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        job_orgn <- str_match(one_job2[i], pattern = "Job Orgn: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[,-1]
        job_type <- str_match(one_job2[i], pattern = "Job Type: " %R% capture(one_or_more(WRD)))[, -1]
        job_title <- str_trim(str_match(one_job2[i], pattern = "Job Title: " %R% capture(one_or_more(char_class(WRD, SPACE, PUNCT))) %R% "Posn")[, -1])
        posn_suff <- str_match(one_job2[i], pattern = "Posn-Suff: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[, -1]
        appt_percent <- str_match(one_job2[i], pattern = "Appt Percent:" %R% zero_or_more(SPC) %R% capture(one_or_more(DGT)))[, -1]
        months_length <- str_trim(str_match(one_job2[i], pattern = "Appt: Classified" %R% capture(zero_or_more(char_class(WRD, NOT_WRD))) %R% or("F", "H"))[-1])
        monthly_salary_rate <- str_match(one_job2[i], pattern = "Full-Time Monthly Salary:" %R% one_or_more(SPC) %R% capture(one_or_more(DGT) %R% DOT %R% one_or_more(DGT)))[, -1]
        hourly_salary_rate <- str_match(one_job2[i], pattern = "Hourly Rate:" %R% one_or_more(SPC) %R% capture(one_or_more(DGT) %R% DOT %R% one_or_more(DGT)))[, -1]
        
        entry2 <- c(name, first_hired, home_orgn, adj_service_date, job_orgn, 
               job_type, job_title, posn_suff, appt_percent, months_length, monthly_salary_rate, hourly_salary_rate)
        
        staff <- rbind(staff, entry2)
} 

# Get rid of the dummy row
staff <- as_tibble(staff) %>%
        .[-1, ]

# I need to get the subset of rows that HAS a name, then fill first 4 rows 
# downwards, then delete that subset

staff <- staff %>% 
        fill(name, first_hired, home_orgn, adj_service_date)

staff <- staff[!is.na(staff$job_orgn), ]

# Save for now -----------------------------------------------------------------
 
save(staff, file = "staff_df.RData") 
 




