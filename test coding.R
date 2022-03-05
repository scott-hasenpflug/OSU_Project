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

quantile(dfs$annual_salary_rate, na.rm = TRUE)
quantile(dfo$annual_salary_rate, na.rm = TRUE)

rm(dfo, dft, dfs)
