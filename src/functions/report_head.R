report_head <- function(group1 = type.employee, group2 = full.time, group3 = NULL, 
                        group4 = NULL, sort = Headcount) {
  osu %>%
    group_by({{group1}}, {{group2}}, {{group3}}, {{group4}}) %>%
    summarize(Headcount = length(unique(name.full)),
              FTE = sum(percent.time)/100,
              Anualized = accounting(median(pay.annualized)),
              Actual = accounting(median(pay.actual)),
              Tenure = median(tenure.yrs)) %>%
    mutate(full.time = case_when(
      full.time == "TRUE" ~ "Full-time",
      full.time == "FALSE" ~ "Part-time"
    )) %>%
    arrange(desc({{sort}})) %>% 
    rename(Type = type.employee, `Status` = full.time)
}