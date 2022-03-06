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
classified_file <- pdf_text("classified_output.pdf")
# raw_import is a character vector of 150 strings, each representing one page of
        # the report

# Split the strings by the dashed line that separates each employee
classified_pages_listed <- classified_file %>%
        str_split("---------------------------------------------------------------------------------")
# pages_listed is a list of 235 character vectors 5 to 7 strings long (header 
        # plus 4-6 employees)

# Loop over each vector in classified_pages_listed, deleting the first element which is always the header
for (i in 1:235) {
        classified_pages_listed[[i]] <- classified_pages_listed[[i]][-1]
}
# classified_pages_listed is now a list of 1050 character vectors 4 to 6 strings long

# Some people have multiple jobs. For now I want only employees with 1 job. Unlist classified_pages_listed
classified_pages_unlisted <- unlist(classified_pages_listed)
# pages_unlisted is a character vector with 5958 strings representing each employee

# Single job employees' strings are always under 415 in length. Create logical vectors by length:
short <- str_length(classified_pages_unlisted) < 415
long <- str_length(classified_pages_unlisted) >= 415

# Subset single and multiple job employees using those logical vectors
single <- classified_pages_unlisted[short] # 1403 employees 
multiple <- classified_pages_unlisted[long] # 4 employees

# Re-list both vectors
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
        months_length <- str_match(one_job[[1]][i], pattern = "Appt: Classified " %R% capture(one_or_more(DGT)))[, -1]
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

# Convert salaries from character to numeric
df$monthly_salary_rate <- as.numeric(as.character(df$monthly_salary_rate))
df$hourly_salary_rate <- as.numeric(as.character(df$hourly_salary_rate))

# Convert dates from strings
df <- df %>%
        mutate(first_hired = dmy(first_hired), 
               adj_service_date = dmy(adj_service_date))

# Create home organization code variable (277 -> 106 may help)
df <- df %>%
        mutate(home_org_code = str_extract(home_orgn, pattern = START %R% WRD %R% WRD %R% WRD))

# Create factor variable for short (9 mo) vs long (12 mo) contracts
#df <- df %>%
        #mutate(mo_label = recode_factor(months_length, `9` = "short", `10` = "ten", `12` = "long"))

######################### Exploratory Analysis ################################

# Look at the structure of our dataframe
str(df)
glimpse(df)
view(df)

# Generate some summary statistics
skimr::skim(df)

# Who is the highest paid?
## Monthly
df %>% filter(monthly_salary_rate == max(df$monthly_salary_rate, na.rm = TRUE)) %>%
        select(first_name, last_name, monthly_salary_rate)
## Hourly
df %>% filter(hourly_salary_rate == max(df$hourly_salary_rate, na.rm = TRUE)) %>%
        select(first_name, last_name, hourly_salary_rate)

#!!! What's the most common title? 
df %>%
        count(job_title, sort = TRUE)
        
        # Graph of titles with more than 30 holders
common_titles <- df %>%
        count(job_title, sort = TRUE) %>%
        filter(n > 30)

ggplot(common_titles, aes(n, reorder(job_title, n))) +
        geom_col() +
        labs(title = "Most Common Titles", 
             y = "Title", 
             x = "Total") +
        theme_minimal()

# How many people are working outside their home organization?
not_loaned <- sum(df$home_orgn == df$job_orgn)         
loaned <- sum(df$home_orgn != df$job_orgn) 
percent(loaned/(loaned + not_loaned))

# Which home organization has the highest average salary?
## Monthly
df %>%
        group_by(home_orgn) %>%
        summarize(Min = min(monthly_salary_rate, na.rm = TRUE),
                  Mean = mean(monthly_salary_rate, na.rm = TRUE),
                  Median = median(monthly_salary_rate, na.rm = TRUE),
                  Max = max(monthly_salary_rate, na.rm = TRUE),
                  Employees = n()) %>%
        ungroup() %>%
        arrange(desc(Mean)) 
## Hourly
df %>%
        group_by(home_orgn) %>%
        summarize(Min = min(hourly_salary_rate, na.rm = TRUE),
                  Mean = mean(hourly_salary_rate, na.rm = TRUE),
                  Median = median(hourly_salary_rate, na.rm = TRUE),
                  Max = max(hourly_salary_rate, na.rm = TRUE),
                  Employees = n()) %>%
        ungroup() %>%
        arrange(desc(Mean))

# What is the relationship between length of employment and salary?
## Monthly
df %>%
        ggplot(aes(first_hired, monthly_salary_rate, na.rm = TRUE)) +
                geom_point(alpha = .3) +
                geom_smooth(method = lm, se = FALSE)
## Hourly
df %>%
        ggplot(aes(first_hired, hourly_salary_rate, na.rm = TRUE)) +
        geom_point(alpha = .3) +
        geom_smooth(method = lm, se = FALSE)

#-------------------end progress-----------------------------------------------

# What is the median salary of a professor vs an instructor?
median(df$annual_salary_rate[professor_rank], na.rm = TRUE)
median(df$annual_salary_rate[instructor_rank], na.rm = TRUE)

# What is the mean salary of "presidents" across different organisations?
df %>%
        mutate(Prez = president_title) %>%
        filter(Prez == TRUE) %>%
        group_by(home_orgn) %>%
        summarize(Presidents = n(), Salary = mean(annual_salary_rate)) %>%
        ungroup() %>%
        arrange(desc(Salary)) %>%
        mutate(Salary = dollar(Salary)) %>%
        rename(Organization = home_orgn) %>%
        rename("Average Salary" = Salary) %>%
        kable()
        
        
        # salary distribution of "directors"?
df %>%
        mutate(Direc = director_title) %>%
        filter(Direc == TRUE) %>%
        group_by(home_orgn) %>%
        summarize(Directors = n(), Mean_Salary = mean(annual_salary_rate)) %>%
        ungroup() %>%
        arrange(desc(Mean_Salary)) %>% # how do I print this?
        ggplot(aes(Mean_Salary)) +
        geom_histogram(binwidth = 7500) +
        labs(title = '"Director" Salary Distribution', x = "Mean Salary", y = "Count")

## Let's look at these appointment titles
        # Are they paid? No
df$annual_salary_rate[courtesy_title]
df$annual_salary_rate[emeritus_title]
        # Create a logical vector combining these two
appointment_title <- courtesy_title|emeritus_title

## Tasks: 

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

#------------------start progress----------------------------

# Distribution of tenure into 5 yr bins

skim(df$first_hired)
skim(df$adj_service_date)

# Calculate tenure as # of years since first hired (rounded down) ***This is janky. 
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

# Check how they are distributed
table(df$tenure_bin)

# Quick plot
ggplot(df, aes(tenure_bin)) +
        geom_bar()

# Plot by example organization
# Which is the biggest?
count(df, home_org_code, sort = TRUE)

# Plot by MHD
df %>%
        filter(home_org_code == "MHD") %>%
        ggplot(aes(tenure_bin)) +
        geom_bar()

# Compare the top two, HHS and CLA
df %>%
        filter(home_org_code %in% c("MHD", "TEX")) %>%
        ggplot(aes(tenure_bin, fill = home_org_code)) +
        geom_bar(position = "dodge")

# Summary of tenure by home_org_code
        # Create a table of the count per bin per code
table(df$home_org_code, df$tenure_bin)

        # Turn it into a tibble
Tenure_Summary <- table(df$home_org_code, df$tenure_bin) %>%
        as.data.frame() %>%
        tibble() %>%
        pivot_wider(names_from = "Var2", values_from ="Freq")

        # Make a version with a percentage of total per bin per code
Tenure_Summary_Prop <- Tenure_Summary %>%
        mutate(total = (`0-4` + `5-9` + `10-14` + `15-19` + `20-24` + `25-29` + `30-34` + `35-39` + `40+`)) %>%
        mutate(`0-4` = round(`0-4`/total, digits = 3),
               `5-9` = round(`5-9`/total, digits = 3),
               `10-14` = round(`10-14`/total, digits = 3),
               `15-19` = round(`15-19`/total, digits = 3),
               `20-24` = round(`20-24`/total, digits = 3),
               `25-29` = round(`25-29`/total, digits = 3),
               `30-34` = round(`30-34`/total, digits = 3),
               `35-39` = round(`35-39`/total, digits = 3),
               `40+` = round(`40+`/total, digits = 3)) %>%
                .[,-11]

# Check if there are any duplicate names
df$full_name[duplicated(df$full_name)]
        # Nope
        
############################## To-do ##########################################

