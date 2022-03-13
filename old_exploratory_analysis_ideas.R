# Who is the highest paid?
df %>% filter(annual_salary_rate == max(df$annual_salary_rate, na.rm = TRUE)) %>%
        select(first_name, last_name, annual_salary_rate)

# What's the most common title?
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
df %>%
        group_by(home_orgn) %>%
        summarize(Min = min(annual_salary_rate, na.rm = TRUE),
                  Mean = mean(annual_salary_rate, na.rm = TRUE),
                  Median = median(annual_salary_rate, na.rm = TRUE),
                  Max = max(annual_salary_rate, na.rm = TRUE),
                  Employees = n()) %>%
        ungroup() %>%
        arrange(desc(Mean)) %>%
        mutate(Min = dollar(round(Min)), Mean = dollar(round(Mean)), 
               Median = dollar(round(Median)), Max = dollar(round(Max)))


# What is the relationship between length of employment and salary?
df %>%
        filter(annual_salary_rate < 350000) %>%
        filter(annual_salary_rate > 25000) %>%
        ggplot(aes(first_hired, annual_salary_rate, na.rm = TRUE)) +
                geom_point(alpha = .3)

# Make some logical vectors indicating if certain words appear in Rank or Title
senior_rank <- str_detect(df$rank_name, pattern = "Senior")
assistant_rank <- str_detect(df$rank_name, pattern = "Assistant")
professor_rank <- str_detect(df$rank_name, pattern = "Professor")
associate_rank <- str_detect(df$rank_name, pattern = "Associate")
instructor_rank <- str_detect(df$rank_name, pattern = "Instructor")


manager_title <- str_detect(df$job_title, pattern = or("Manager", "Mgr"))
president_title <- str_detect(df$job_title, pattern = or("President", "Pres.", "Pres-"))
courtesy_title <- str_detect(df$job_title, pattern = "Courtesy Appointment")
emeritus_title <- str_detect(df$job_title, pattern = "Emeritus Appointment")

#This will include the logical vector as a column in the df
#df2 <- df %>%
        #mutate(emeritus = str_detect(df$job_title, pattern = "Emeritus Appointment"))

# Check how tenures are distributed
table(df$tenure_bin)

# Make a table of tenure per bin by organization
table(df$home_org_code, df$tenure_bin)

#Quick plot of overall distribution
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

skim(df$rank_effective_date)
skim(df$appt_begin_date) # Has typo
skim(df$first_hired)
skim(df$adj_service_date) # Has typo



# Check how they are distributed
table(df$tenure_bin)

# Quick plot
ggplot(df, aes(tenure_bin)) +
        geom_bar()

        # Same plot but eliminate appointments
df %>%
        filter(appointment_title == FALSE) %>%
        ggplot(aes(tenure_bin)) +
        geom_bar()

        # Appointments only now
df %>%
        filter(appointment_title == TRUE) %>%
        ggplot(aes(tenure_bin)) +
        geom_bar()

## Hmmm who do these new appointments work for?
df %>%
        filter(appointment_title == TRUE) %>%
        count(home_orgn) %>%
        arrange(desc(n))

# Plot by example organization
# Which is the biggest?
count(df, home_org_code, sort = TRUE)

# Plot by HHS
df %>%
        filter(home_org_code == "HHS") %>%
        ggplot(aes(tenure_bin)) +
        geom_bar()

# Compare the top two, HHS and CLA
df %>%
        filter(home_org_code %in% c("HHS", "CLA")) %>%
        ggplot(aes(tenure_bin, fill = home_org_code)) +
        geom_bar(position = "dodge")
        
        # Same but eliminate appointments
df %>%
        filter(appointment_title == FALSE) %>%
        filter(home_org_code %in% c("HHS", "CLA")) %>%
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
        # They're worth double checking

unique(df$rank_name)
df %>%
        count(rank_name) %>%
        view()
