# Load necessary packages
library(tidyverse)
library(pdftools)
library(rebus)
library(lubridate)
library(tidytext)
library(skimr)
library(scales)
library(knitr)

# Load data frame
load("classified_df.RData")
df <- classified_df
rm(classified_df)

##############################
# Exploratory Analysis
##############################

# Look at the structure of our dataframe
str(df)
glimpse(df)

# Generate some summary statistics
skimr::skim(df)

# Who is the highest paid?

# Monthly
df %>% filter(monthly_salary_rate == max(df$monthly_salary_rate, na.rm = TRUE)) %>%
        select(first_name, last_name, monthly_salary_rate)
# Hourly
df %>% filter(hourly_salary_rate == max(df$hourly_salary_rate, na.rm = TRUE)) %>%
        select(first_name, last_name, hourly_salary_rate)

#What's the most common title? 
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
rm(loaned, not_loaned)

# Which home organization has the highest average salary?

# Monthly
df %>%
        group_by(home_orgn) %>%
        summarize(Min = min(monthly_salary_rate, na.rm = TRUE),
                  Mean = mean(monthly_salary_rate, na.rm = TRUE),
                  Median = median(monthly_salary_rate, na.rm = TRUE),
                  Max = max(monthly_salary_rate, na.rm = TRUE),
                  Employees = n()) %>%
        ungroup() %>%
        arrange(desc(Mean)) 

# Hourly
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

# Monthly
df %>%
        ggplot(aes(first_hired, monthly_salary_rate, na.rm = TRUE)) +
        geom_point(alpha = .3) +
        geom_smooth(method = lm, se = FALSE)
# Hourly
df %>%
        ggplot(aes(first_hired, hourly_salary_rate, na.rm = TRUE)) +
        geom_point(alpha = .3) +
        geom_smooth(method = lm, se = FALSE)

# Check how tenures are distributed
table(df$tenure_bin)

# Make a table of tenure per bin by organization
table(df$home_org_code, df$tenure_bin)

# Quick plot of overall distribution
ggplot(df, aes(tenure_bin)) +
        geom_bar()

# Examine individual organizations
# Which is the biggest?
count(df, home_org_code, sort = TRUE)

# Plot by largest organization
df %>%
        filter(home_org_code == pull(count(df, home_org_code, sort = TRUE)[1, 1])) %>%
        ggplot(aes(tenure_bin)) +
        geom_bar()

# Compare the top two
df %>%
        filter(home_org_code %in% pull(count(df, home_org_code, sort = TRUE)[1:2, 1])) %>%
        ggplot(aes(tenure_bin, fill = home_org_code)) +
        geom_bar(position = "dodge")

# Check if there are any duplicate names
df$full_name[duplicated(df$full_name)]


############################## old unused code from classified #############################

# What is the median salary of a professor vs an instructor?
#median(df$annual_salary_rate[professor_rank], na.rm = TRUE)
#median(df$annual_salary_rate[instructor_rank], na.rm = TRUE)

# What is the mean salary of "presidents" across different organisations?
#df %>%
#mutate(Prez = president_title) %>%
#filter(Prez == TRUE) %>%
#group_by(home_orgn) %>%
#summarize(Presidents = n(), Salary = mean(annual_salary_rate)) %>%
#ungroup() %>%
#arrange(desc(Salary)) %>%
#mutate(Salary = dollar(Salary)) %>%
#rename(Organization = home_orgn) %>%
#rename("Average Salary" = Salary) %>%
#kable()


# salary distribution of "directors"?
#df %>%
#        mutate(Direc = director_title) %>%
#        filter(Direc == TRUE) %>%
#        group_by(home_orgn) %>%
#        summarize(Directors = n(), Mean_Salary = mean(annual_salary_rate)) %>%
#        ungroup() %>%
#        arrange(desc(Mean_Salary)) %>% # how do I print this?
#        ggplot(aes(Mean_Salary)) +
#        geom_histogram(binwidth = 7500) +
#        labs(title = '"Director" Salary Distribution', x = "Mean Salary", y = "Count")

## Let's look at these appointment titles
# Are they paid? No
#df$annual_salary_rate[courtesy_title]
#df$annual_salary_rate[emeritus_title]
# Create a logical vector combining these two
#appointment_title <- courtesy_title|emeritus_title
#
#
# Turn it into a tibble
#Tenure_Summary <- table(df$home_org_code, df$tenure_bin) %>%
#        as.data.frame() %>%
#        tibble() %>%
#        pivot_wider(names_from = "Var2", values_from ="Freq")

# Make a version with a percentage of total per bin per code
#Tenure_Summary_Prop <- Tenure_Summary %>%
#        mutate(total = (`0-4` + `5-9` + `10-14` + `15-19` + `20-24` + `25-29` + `30-34` + `35-39` + `40+`)) %>%
#       mutate(`0-4` = round(`0-4`/total, digits = 3),
#               `5-9` = round(`5-9`/total, digits = 3),
#              `10-14` = round(`10-14`/total, digits = 3),
#             `15-19` = round(`15-19`/total, digits = 3),
#               `20-24` = round(`20-24`/total, digits = 3),
#               `25-29` = round(`25-29`/total, digits = 3),
#               `30-34` = round(`30-34`/total, digits = 3),
#               `35-39` = round(`35-39`/total, digits = 3),
#               `40+` = round(`40+`/total, digits = 3)) %>%
#        .[,-11]