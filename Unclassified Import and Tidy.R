# Load necessary packages
library(tidyverse)
library(pdftools)
library(rebus)
library(lubridate)
library(tidytext)
library(skimr)
library(scales)
library(knitr)
#

# Import the file
raw_import <- pdf_text("unclassified_output.pdf")
# raw_import is a character vector of 150 strings, each representing one page of
# the report

# Split the strings by the dashed line that separates each employee
pages_listed <- raw_import %>%
        str_split("---------------------------------------------------------------------------------")
# pages_listed is a list of 1050 character vectors 5 to 7 strings long (header 
# plus 4-6 employees)

# Loop over each vector in pages_listed, deleting the first element which is always the header
for (i in 1:1050) {
        pages_listed[[i]] <- pages_listed[[i]][-1]
}
# pages_listed is now a list of 1050 character vectors 4 to 6 strings long

# Some people have multiple jobs. For now I want only employees with 1 job. Unlist pages_listed
pages_unlisted <- unlist(pages_listed)
# pages_unlisted is a character vector with 5958 strings representing each employee

# Single job employees' strings are always in the 500s in length. Create logical vectors by length:
short <- str_length(pages_unlisted) < 600
long <- str_length(pages_unlisted) >= 600

# Subset single and multiple job employees using those logical vectors
single <- pages_unlisted[short] # 5688 employees 
multiple <- pages_unlisted[long] # 270 employees

# Re-list both vectors
one_job <- list(single)
multiple_jobs <- list(multiple)

# Create target dataframe ***This is not a good way***

# Define variable names for the target dataframe
Variable_Names <- c("name", "first_hired", "home_orgn", "adj_service_date", "job_orgn", 
                    "job_type", "job_title", "posn_suff", "rank_name", "rank_effective_date", "appt_begin_date",
                    "appt_percent", "appt_end_date", "annual_salary_rate", "months_length")
# Create dummy vector
x <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")
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
        rank_name <- str_trim(str_match(one_job[[1]][i], pattern = "Rank: " %R% capture(one_or_more(char_class(WRD, SPACE, PUNCT))) %R% "Rank E")[, -1])
        rank_effective_date <- str_match(one_job[[1]][i], pattern = "Rank Effective Date: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        appt_begin_date <- str_match(one_job[[1]][i], pattern = "Appt Begin Date: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        appt_percent <- str_match(one_job[[1]][i], pattern = "Appt Percent:" %R% zero_or_more(SPC) %R% capture(one_or_more(DGT)))[, -1]
        appt_end_date <- str_match(one_job[[1]][i], pattern = "Appt End Date: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        annual_salary_rate <- str_match(one_job[[1]][i], pattern = "Annual Salary Rate:" %R% one_or_more(SPC) %R% capture(one_or_more(DGT) %R% DOT %R% one_or_more(DGT)))[, -1]
        months_length <- str_match(one_job[[1]][i], pattern = capture(one_or_more(DGT)) %R% SPC %R% "mo")[, -1]
        
        x <- c(name, first_hired, home_orgn, adj_service_date, job_orgn, 
               job_type, job_title, posn_suff, rank_name, rank_effective_date, appt_begin_date,
               appt_percent, appt_end_date, annual_salary_rate, months_length)
        
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


# Convert salary from character to numeric
df$annual_salary_rate <- as.numeric(as.character(df$annual_salary_rate))

# Convert dates from strings
df <- df %>%
        mutate(first_hired = dmy(first_hired), 
               adj_service_date = dmy(adj_service_date),
               rank_effective_date = dmy(rank_effective_date),
               appt_begin_date = dmy(appt_begin_date))

# Create home organization code variable (277 -> 106 may help)
df <- df %>%
        mutate(home_org_code = str_extract(home_orgn, pattern = START %R% WRD %R% WRD %R% WRD))

# Create factor variable for short (9 mo) vs long (12 mo) contracts
df <- df %>%
        mutate(mo_label = recode_factor(months_length, `9` = "short", `12` = "long"))

# Distribute Salary into bins
df <- df %>%
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

# Fix adj_service_date typo
df <- df %>%
        mutate(appt_begin_date = replace(appt_begin_date, appt_begin_date == "0221-01-01", "2021-01-01")) 

# Fix adj_service_date typo
df <- df %>%
        mutate(adj_service_date = replace(adj_service_date, adj_service_date == "0200-05-04", "2020-05-04")) 

# Calculate tenure as # of years since first hired (rounded down) 
# Figure it out better in lubridate***
df <- df %>%
        mutate(tenure = floor((as.numeric((today() - first_hired)/365))))

# Create labels for the bins
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

# Save the data frame as unclassified_df
unclassified_df <- df
save(unclassified_df, file = "unclassified_df.RData")