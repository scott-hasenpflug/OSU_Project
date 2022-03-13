#*******************************************************************************
# Import Faculty
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

raw_import <- pdf_text("faculty_pdf.pdf")

#*******************************************************************************
# 2 - Create Data Frame
#*******************************************************************************

# Make a vector of jobs --------------------------------------------------------

# I have a character vector of each page. Split off the header
split_header <- str_split(raw_import, pattern = "\n\n\n")

# Remove the header
for (i in 1:length(split_header)) {
        split_header[[i]] <- split_header[[i]][-1]    
}

# Unlist to split again
split_header <- unlist(split_header)

# Split by dashed line. This separates individuals (but not jobs)
split_line <- split_header %>%
        str_split("---------------------------------------------------------------------------------")

# Remove new nonsense first item in the list. For some employees who have multiple 
# jobs and split them over 2 pages, we have a problem. The first item is a job. 
# In all other cases the first item in the list is nonsense and we need to remove 
# it. Nonsense cases are < 15 characters so we only remove those.
for (i in 1:length(split_line)) {
        if (str_length(split_line[[i]][1]) < 15){
                split_line[[i]] <- split_line[[i]][-1]    
        }
}

# Unlist to split again
split_line <- unlist(split_line)

# Split again for additional jobs
temp <- split_line %>%
        str_split("mo\n\n    ")

# Unlist
temp <- unlist(temp)

# temp has a lot of "" characters now. Remove these
one_job <- temp[temp != ""]
# We now have a character vector with one element for every job.

# Initialize a data frame ------------------------------------------------------

# Create initial variable names based on parsing plan
Variable_Names <- c("name", "first_hired", "home_orgn", "adj_service_date", "job_orgn", 
                    "job_type", "job_title", "posn_suff", "rank_name", "rank_effective_date", 
                    "appt_begin_date", "appt_percent", "appt_end_date", "annual_salary_rate", 
                    "months_length")

# Create dummy vector
dummy <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")

# Build the data frame
faculty <- NULL
faculty <- rbind(faculty, dummy)
colnames(faculty) <- Variable_Names

# Parse and bind to data frame -------------------------------------------------

# Loop to capture strings, assign to values, and bind to new data frame
for (i in 1:length(one_job)) {
        name <- str_match(one_job[i], pattern = "Name: " %R% capture(
                lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[,-1]
        first_hired <- str_match(one_job[i], pattern = "First Hired: " %R% capture(
                one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        home_orgn <- str_match(one_job[i], pattern = "Home Orgn: " %R% capture(
                lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% or(SPC, "Adj"))[,-1]
        adj_service_date <- str_match(one_job[i], pattern = "Adj Service Date: " %R% capture(
                one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        job_orgn <- str_match(one_job[i], pattern = "Job Orgn: " %R% capture(
                lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[,-1]
        job_type <- str_match(one_job[i], pattern = "Job Type: " %R% capture(
                one_or_more(WRD)))[, -1]
        job_title <- str_trim(str_match(one_job[i], pattern = "Job Title: " %R% capture(
                one_or_more(char_class(WRD, SPACE, PUNCT))) %R% "Posn")[, -1])
        posn_suff <- str_match(one_job[i], pattern = "Posn-Suff: " %R% capture(
                lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[, -1]
        rank_name <- str_trim(str_match(one_job[i], pattern = "Rank: " %R% capture(
                one_or_more(char_class(WRD, SPACE, PUNCT))) %R% "Rank E")[, -1])
        rank_effective_date <- str_match(one_job[i], pattern = "Rank Effective Date: " %R% capture(
                one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        appt_begin_date <- str_match(one_job[i], pattern = "Appt Begin Date: " %R% capture(
                one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        appt_percent <- str_match(one_job[i], pattern = "Appt Percent:" %R% zero_or_more(SPC) %R% capture(
                one_or_more(DGT)))[, -1]
        appt_end_date <- str_match(one_job[i], pattern = "Appt End Date: " %R% capture(
                one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        annual_salary_rate <- str_match(one_job[i], pattern = "Annual Salary Rate:" %R% one_or_more(SPC) %R% capture(
                one_or_more(DGT) %R% DOT %R% one_or_more(DGT)))[, -1]
        months_length <- str_match(one_job[i], pattern = capture(
                one_or_more(DGT)) %R% or((" mo"), (SPC) %R% END))[, -1]
        
        entry <- c(name, first_hired, home_orgn, adj_service_date, job_orgn, 
                   job_type, job_title, posn_suff, rank_name, rank_effective_date, 
                   appt_begin_date, appt_percent, appt_end_date, annual_salary_rate, 
                   months_length)
        
        faculty <- rbind(faculty, entry)
} 

# Remove dummy row
faculty <- as_tibble(faculty) %>%
        .[-1, ]

# Save for now -----------------------------------------------------------------

save(faculty, file = "faculty_df.RData")
