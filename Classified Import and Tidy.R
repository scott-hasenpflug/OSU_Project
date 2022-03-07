#############################################################
## Classified Employee data wrangling and tidying
## Scott Hasenpflug
## hasenpflug7@gmail.com
## 06MAR2022
#############################################################

##############################
# 0 - Load librairies
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
data_path <- "C:/Users/hasen/Desktop/R_WD/OSU Project/" 
data_file <- "classified_output.pdf" 

##############################
# 2 - Import
##############################

input_data <- pdf_text(paste0(data_path, data_file))

##############################
# 3 - Tidy
##############################

# input_data is a character vector, each representing one page of the report

# Split the strings to separate each employee
pages_listed <- input_data %>%
        str_split("---------------------------------------------------------------------------------")

# Check first vector
pages_listed[[1]]

# Loop over each vector in pages_listed, deleting the first element which is always the header
for (i in 1:length(pages_listed)) {
        pages_listed[[i]] <- pages_listed[[i]][-1]
}

# Check first vector to ensure success
pages_listed[[1]]

# Some people have multiple jobs. For now I want only employees with 1 job. Un-list ages_listed
pages_unlisted <- unlist(pages_listed)
# pages_unlisted is a character vector with with each string representing one employee

# I only want employees with one job for now. An easy way to tell is string length:
sort(str_length(pages_unlisted), decreasing = TRUE)

# Manually find the length cutoff between one and multiple jobs. Here, it was ~ 415
cutoff <- 415
 
# Single job employees' strings are always under (cutoff) in length. Create logical vectors by length:
short <- str_length(pages_unlisted) < cutoff
long <- str_length(pages_unlisted) >= cutoff

# Subset single and multiple job employees using those logical vectors
single <- pages_unlisted[short]
multiple <- pages_unlisted[long]

# Re-list both character vectors by number of jobs
one_job <- list(single)
multiple_jobs <- list(multiple)

# Create target dataframe ***This is not a good way***

        # Define variable names for the target dataframe
        Variable_Names <- c("name", "first_hired", "home_orgn", "adj_service_date", "job_orgn", 
                            "job_type", "job_title", "posn_suff", "appt_percent", "months_length", "monthly_salary_rate", "hourly_salary_rate")
        # Create dummy vector
        x <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")
        # Build the dataframe
        df <- NULL
        df <- rbind(df, x)
        colnames(df) <- Variable_Names

# Loop over every vector in list one_job, extracting each variable, putting them into a 
# vector, and binding it to the bottom of the dataframe
for (i in 1:length(one_job[[1]])) {
        name <- str_match(one_job[[1]][i], pattern = "Name: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[,-1]
        first_hired <- str_match(one_job[[1]][i], pattern = "First Hired: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        home_orgn <- str_match(one_job[[1]][i], pattern = "Home Orgn: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% or(SPC, "Adj"))[,-1]
        adj_service_date <- str_match(one_job[[1]][i], pattern = "Adj Service Date: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        job_orgn <- str_match(one_job[[1]][i], pattern = "Job Orgn: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[,-1]
        job_type <- str_match(one_job[[1]][i], pattern = "Job Type: " %R% capture(one_or_more(WRD)))[, -1]
        job_title <- str_trim(str_match(one_job[[1]][i], pattern = "Job Title: " %R% capture(one_or_more(char_class(WRD, SPACE, PUNCT))) %R% "Posn")[, -1])
        posn_suff <- str_match(one_job[[1]][i], pattern = "Posn-Suff: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[, -1]
        appt_percent <- str_match(one_job[[1]][i], pattern = "Appt Percent:" %R% zero_or_more(SPC) %R% capture(one_or_more(DGT)))[, -1]
        months_length <- str_trim(str_match(one_job[[1]][i], pattern = "Appt: Classified" %R% capture(zero_or_more(char_class(WRD, NOT_WRD))) %R% or("F", "H"))[-1])
        monthly_salary_rate <- str_match(one_job[[1]][i], pattern = "Full-Time Monthly Salary:" %R% one_or_more(SPC) %R% capture(one_or_more(DGT) %R% DOT %R% one_or_more(DGT)))[, -1]
        hourly_salary_rate <- str_match(one_job[[1]][i], pattern = "Hourly Rate:" %R% one_or_more(SPC) %R% capture(one_or_more(DGT) %R% DOT %R% one_or_more(DGT)))[, -1]
        
        x <- c(name, first_hired, home_orgn, adj_service_date, job_orgn, 
               job_type, job_title, posn_suff, appt_percent, months_length, monthly_salary_rate, hourly_salary_rate)
        
        df <- rbind(df, x)
} 

# Get rid of the first dummy row
df <- as_tibble(df) %>%
        .[-1,]

# Add columns of separated first and last names (drops middle and suffixes), keep full names as full_name 
df <- df %>%
        mutate(full_name = name) %>%
        separate(name, c("last_name", "first_name"), sep = ", ") %>%
        mutate(first_name = word(first_name))

# Clean up months_length
df <- df %>%
        mutate(months_length = if_else(months_length != "-Intermittent (J)", months_length, "Intermittent")) %>%
        mutate(months_length = if_else(months_length != "-Limited Duration (J", months_length, "Limited")) %>%               
        mutate(months_length = if_else(months_length != "-Seasonal (J)", months_length, "Seasonal"))

# Convert salaries from character to numeric
df$monthly_salary_rate <- as.numeric(as.character(df$monthly_salary_rate))
df$hourly_salary_rate <- as.numeric(as.character(df$hourly_salary_rate))

# Convert dates from strings
df <- df %>%
        mutate(first_hired = dmy(first_hired), 
               adj_service_date = dmy(adj_service_date))

# Create home organization code variable
df <- df %>%
        mutate(home_org_code = str_extract(home_orgn, pattern = START %R% WRD %R% WRD %R% WRD))

# Create hourly equivalent of salary to use in comparison (assume 120 hr month)
df <- df %>%
        mutate(monthly_salary_equivalent = hourly_salary_rate * 120)

# Distribute Salary into bins

# Monthly
quantile(df$monthly_salary_rate, probs = seq(0, 1, 0.2), na.rm = TRUE)

df <- df %>%
        mutate(monthly_salary_bin = case_when(
                is.na(monthly_salary_rate) ~ "NA",
                between(monthly_salary_rate, 0, 3824.99) ~ "Very Low", 
                between(monthly_salary_rate, 3825, 4197.99) ~ "Low",
                between(monthly_salary_rate, 4198, 4834.99) ~ "Medium",
                between(monthly_salary_rate, 4835, 6132.99) ~ "High",
                between(monthly_salary_rate, 6133, 9604) ~ "Very High",
                TRUE ~ "Other")) %>%
        mutate(monthly_salary_bin = factor(monthly_salary_bin, 
                                           levels = c("NA", "Very Low", 
                                                      "Low", "Medium", "High", 
                                                      "Very High", "Other")))

# Hourly
quantile(df$hourly_salary_rate, probs = seq(0, 1, 0.2), na.rm = TRUE)

df <- df %>%
        mutate(hourly_salary_bin = case_when(
                is.na(hourly_salary_rate) ~ "NA",
                between(hourly_salary_rate, 0, 16.30) ~ "Very Low", 
                between(hourly_salary_rate, 16.31, 19.24) ~ "Low",
                between(hourly_salary_rate, 19.25, 21.06) ~ "Medium",
                between(hourly_salary_rate, 21.07, 24.22) ~ "High",
                between(hourly_salary_rate, 24.23, 56.74) ~ "Very High",
                TRUE ~ "Other")) %>%
        mutate(hourly_salary_bin = factor(hourly_salary_bin, 
                                          levels = c("NA", "Very Low", 
                                                     "Low", "Medium", "High", 
                                                     "Very High", "Other")))

# Distribution of tenure into 5 yr bins

# Calculate tenure as # of years since first hired, rounded down
df <- df %>%
        mutate(tenure = floor((as.numeric((today() - first_hired)/365))))

# Create labels for the 5 yr tenure bins
tenure_bin_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40+")

# Create a column of the bin value based on tenure
df <- df %>%
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

# Save the data frame as classified_df
classified_df <- df
save(classified_df, file = "classified_df.RData")




