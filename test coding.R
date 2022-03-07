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
