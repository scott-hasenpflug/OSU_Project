df2 <- df %>%
        separate(name, c("last_name", "first_name"), sep = ", ") %>%
        mutate(first_name = word(first_name))

kable(df$name[duplicated(df$name)])

scale_y_continuous(labels = scales::percent)

df %>%
        filter(job_title == "Courtesy Appointment") %>%
        quantile(annual_salary_rate, na.rm = TRUE)

        
df <- df %>%
        select(-tenure_bins)

df %>%
        subset(!appointment_title) %>%
        ggplot(aes(x = tenure_bin, fill = mo_label)) +
        geom_bar()

df %>%
        filter(job_type == "S") %>%
        view()

df <- df %>%
        mutate(salary_bin = case_when(
                is.na(annual_salary_rate) ~ "Unpaid",
                between(annual_salary_rate, 0, 45000) ~ "Very Low", 
                between(annual_salary_rate, 45001, 58000) ~ "Low",
                between(annual_salary_rate, 58001, 100000) ~ "Medium",
                between(annual_salary_rate, 100001, 150000) ~ "High",
                between(annual_salary_rate, 150001, 300000) ~ "Very High",
                TRUE ~ "Top Tier")) %>%
        mutate(salary_bin = factor(salary_bin, levels = c("Unpaid", "Very Low", "Low", "Medium", "High", "Very High", "Top Tier")))

df %>%
        ggplot(aes(salary_bin, fill = tenure_bin)) +
        geom_bar()

dfs <- df %>%
        filter(job_type == "S")
dfo <- df %>%
        filter(job_type == "O")

quantile(df$monthly_salary_rate, na.rm = TRUE)
quantile(dfo$annual_salary_rate, na.rm = TRUE)

rm(dfo, dft, dfs)

df %>%
        filter(is.na(df$months_length)) %>%
        view()

str_trim(str_match(one_job[[1]][1], pattern = "Appt: Classified" %R% capture(zero_or_more(char_class(WRD, NOT_WRD))) %R% "F")[-1])

df2 <- df %>%
        mutate(months_length = str_replace_all(months_length, "-Intermittent (J)", "Intermittent"))

sum(df$months_length == "-Intermittent (J)")

df <- df %>%
        mutate(months_length = if_else(months_length != "-Intermittent (J)", months_length, "Intermittent")) %>%
        mutate(months_length = if_else(months_length != "-Limited Duration (J", months_length, "Limited")) %>%               
        mutate(months_length = if_else(months_length != "-Seasonal (J)", months_length, "Seasonal"))

df %>%
        mutate(Direc = director_title) %>%
        filter(Direc == TRUE) %>%
        filter(annual_salary_rate < 300000) %>%
        ggplot(aes(annual_salary_rate)) +
        geom_histogram()

df %>%
        mutate(Direc = director_title) %>%
        filter(Direc == TRUE) %>%
        group_by(home_orgn) %>%
        summarize(Directors = n(), Mean_Salary = mean(annual_salary_rate)) %>%
        arrange(desc(Mean_Salary)) %>%
        view()

        filter(between(Mean_Salary, 130))
        
        df %>%
                mutate(Direc = director_title) %>%
                filter(Direc == TRUE) %>%
                group_by(home_orgn) %>%
                summarize(Directors = n(), Mean_Salary = mean(annual_salary_rate), 
                          Max_Salary = max(annual_salary_rate),
                          Min_Salary = min(annual_salary_rate),
                          Range = (max(annual_salary_rate)/min(annual_salary_rate))-1) %>%
                ungroup() %>%
                arrange(desc(Mean_Salary)) %>%
                filter(between(Mean_Salary, 130000, 150000)) %>%
                write_csv("director_spike.csv")

#------- 07 MAR -----Fix Unclassified Parsing-----------------------------------
        Variable_Names <- c("name", "first_hired", "home_orgn", "adj_service_date", "job_orgn", 
                            "job_type", "job_title", "posn_suff", "rank_name", "rank_effective_date", "appt_begin_date",
                            "appt_percent", "appt_end_date", "annual_salary_rate", "months_length")
        
        x <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")
        # Build the dataframe
        df <- NULL
        df <- rbind(df, x)
        colnames(df) <- Variable_Names
        
        # Loop over every vector in list one_job, extracting each variable, putting them into a 
        # vector, and binding it to the bottom of the dataframe
        for (i in 1:length(individual_jobs[[1]])) {
                name <- str_match(individual_jobs[[1]][i], pattern = "Name: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[,-1]
                first_hired <- str_match(individual_jobs[[1]][i], pattern = "First Hired: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
                home_orgn <- str_match(individual_jobs[[1]][i], pattern = "Home Orgn: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% or(SPC, "Adj"))[,-1]
                adj_service_date <- str_match(individual_jobs[[1]][i], pattern = "Adj Service Date: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
                job_orgn <- str_match(individual_jobs[[1]][i], pattern = "Job Orgn: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[,-1]
                job_type <- str_match(individual_jobs[[1]][i], pattern = "Job Type: " %R% capture(one_or_more(WRD)))[, -1]
                job_title <- str_trim(str_match(individual_jobs[[1]][i], pattern = "Job Title: " %R% capture(one_or_more(char_class(WRD, SPACE, PUNCT))) %R% "P")[, -1])
                posn_suff <- str_match(individual_jobs[[1]][i], pattern = "Posn-Suff: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[, -1]
                rank_name <- str_trim(str_match(individual_jobs[[1]][i], pattern = "Rank: " %R% capture(one_or_more(char_class(WRD, SPACE, PUNCT))) %R% "Rank E")[, -1])
                rank_effective_date <- str_match(individual_jobs[[1]][i], pattern = "Rank Effective Date: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
                appt_begin_date <- str_match(individual_jobs[[1]][i], pattern = "Appt Begin Date: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
                appt_percent <- str_match(individual_jobs[[1]][i], pattern = "Appt Percent:" %R% zero_or_more(SPC) %R% capture(one_or_more(DGT)))[, -1]
                appt_end_date <- str_match(individual_jobs[[1]][i], pattern = "Appt End Date: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
                annual_salary_rate <- str_match(individual_jobs[[1]][i], pattern = "Annual Salary Rate:" %R% one_or_more(SPC) %R% capture(one_or_more(DGT) %R% DOT %R% one_or_more(DGT)))[, -1]
                months_length <- str_match(individual_jobs[[1]][i], pattern = capture(one_or_more(DGT)))[, -1]
                
                x <- c(name, first_hired, home_orgn, adj_service_date, job_orgn, 
                       job_type, job_title, posn_suff, rank_name, rank_effective_date, appt_begin_date,
                       appt_percent, appt_end_date, annual_salary_rate, months_length)
                
                df <- rbind(df, x)
        } 
        
        df <- as_tibble(df) %>%
                .[-1,]

oh_shit <- str_split(pages_unlisted, pattern = "\n\n")
     
##################### This shit makes faculty! ##############################

raw_import <- pdf_text("unclassified_output.pdf")
split_header <- str_split(raw_import, pattern = "\n\n\n")
for (i in 1:length(split_header)) {
        split_header[[i]] <- split_header[[i]][-1]    
}

split_header <- unlist(split_header)

split_line <- split_header %>%
        str_split("---------------------------------------------------------------------------------")

glimpse(faculty)

for (i in 1:length(split_line)) {
        if (str_length(split_line[[i]][1]) < 15){
                split_line[[i]] <- split_line[[i]][-1]    
        }
}

split_line <- unlist(split_line)

temp <- split_line %>%
        str_split("mo\n\n    ")

temp <- unlist(temp)

one_job <- temp[temp != ""]


Variable_Names <- c("name", "first_hired", "home_orgn", "adj_service_date", "job_orgn", 
                    "job_type", "job_title", "posn_suff", "rank_name", "rank_effective_date", "appt_begin_date",
                    "appt_percent", "appt_end_date", "annual_salary_rate", "months_length")
# Create dummy vector
dummy <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")
# Build the dataframe
faculty <- NULL
faculty <- rbind(faculty, dummy)
colnames(faculty) <- Variable_Names

for (i in 1:6299) {
        name <- str_match(one_job[i], pattern = "Name: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[,-1]
        first_hired <- str_match(one_job[i], pattern = "First Hired: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        home_orgn <- str_match(one_job[i], pattern = "Home Orgn: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% or(SPC, "Adj"))[,-1]
        adj_service_date <- str_match(one_job[i], pattern = "Adj Service Date: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        job_orgn <- str_match(one_job[i], pattern = "Job Orgn: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[,-1]
        job_type <- str_match(one_job[i], pattern = "Job Type: " %R% capture(one_or_more(WRD)))[, -1]
        job_title <- str_trim(str_match(one_job[i], pattern = "Job Title: " %R% capture(one_or_more(char_class(WRD, SPACE, PUNCT))) %R% "Posn")[, -1])
        posn_suff <- str_match(one_job[i], pattern = "Posn-Suff: " %R% capture(lazy(zero_or_more(char_class(WRD, NOT_WRD)))) %R% SPC %R% SPC)[, -1]
        rank_name <- str_trim(str_match(one_job[i], pattern = "Rank: " %R% capture(one_or_more(char_class(WRD, SPACE, PUNCT))) %R% "Rank E")[, -1])
        rank_effective_date <- str_match(one_job[i], pattern = "Rank Effective Date: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        appt_begin_date <- str_match(one_job[i], pattern = "Appt Begin Date: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        appt_percent <- str_match(one_job[i], pattern = "Appt Percent:" %R% zero_or_more(SPC) %R% capture(one_or_more(DGT)))[, -1]
        appt_end_date <- str_match(one_job[i], pattern = "Appt End Date: " %R% capture(one_or_more(DGT) %R% "\\-" %R% one_or_more(WRD) %R% "\\-" %R% one_or_more(DGT)))[, -1]
        annual_salary_rate <- str_match(one_job[i], pattern = "Annual Salary Rate:" %R% one_or_more(SPC) %R% capture(one_or_more(DGT) %R% DOT %R% one_or_more(DGT)))[, -1]
        months_length <- str_match(one_job[6], pattern = capture(one_or_more(DGT)) %R% or((" mo"), (SPC) %R% END))[, -1]
        
        entry <- c(name, first_hired, home_orgn, adj_service_date, job_orgn, 
               job_type, job_title, posn_suff, rank_name, rank_effective_date, appt_begin_date,
               appt_percent, appt_end_date, annual_salary_rate, months_length)
        
        faculty <- rbind(faculty, entry)
} 

faculty <- as_tibble(faculty) %>%
        .[-1, ]

faculty$annual_salary_rate <- as.numeric(as.character(faculty$annual_salary_rate))
faculty$appt_percent <- as.numeric(as.character(faculty$appt_percent))
faculty$months_length <- as.numeric(as.character(faculty$months_length))

faculty <- faculty %>%
        mutate(full_name = name) %>%
        separate(name, c("last_name", "first_name"), sep = ", ") %>%
        mutate(first_name = word(first_name))

faculty <- faculty %>% fill(last_name, first_name, first_hired, home_orgn, full_name)

faculty <- faculty %>%
        mutate(first_hired = dmy(first_hired), 
               adj_service_date = dmy(adj_service_date),
               rank_effective_date = dmy(rank_effective_date),
               appt_begin_date = dmy(appt_begin_date))

faculty <- faculty %>%
        mutate(job_type = factor(job_type, levels = c("P", "S", "O"))) %>%
        mutate(months_length = factor(months_length, levels = c("9", "12")))

faculty <- faculty %>%
        mutate(appt_begin_date = replace(appt_begin_date, appt_begin_date == "0221-01-01", "2021-01-01")) %>%
        mutate(adj_service_date = replace(adj_service_date, adj_service_date == "0200-05-04", "2020-05-04")) %>%
        mutate(home_orgn = replace(home_orgn, home_orgn == "***Change to 302000 eff 7/1/17", "ENS - Sch Nuclear Sci & Engr")) %>%
        mutate(home_org_code = replace(home_org_code, is.na(home_org_code), "ENS"))

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
faculty <- faculty %>%
        mutate(home_org_code = str_extract(home_orgn, pattern = START %R% WRD %R% WRD %R% WRD))

faculty <- faculty %>%
        mutate(tenure = floor((as.numeric((today() - first_hired)/365))))

tenure_bin_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40+")

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

faculty %>%
        relocate(last_name, first_name, job_title, job_orgn, annual_salary_rate, 
                 appt_percent, months_length, salary_bin, rank_name, job_type, 
                 home_orgn, tenure, tenure_bin, first_hired, adj_service_date,
                 rank_effective_date, appt_begin_date, appt_end_date, posn_suff,
                 home_org_code, full_name)


save(faculty, file = "faculty_df.RData")

head(faculty)
skim(faculty)
str(faculty)
glimpse(faculty)
view(faculty)

