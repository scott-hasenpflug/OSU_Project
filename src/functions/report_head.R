report_head <- function(group1 = type.employee, group2 = full.time, group3 = NULL, 
                        group4 = NULL, sort = Headcount) {
  osu %>%
    group_by({{group1}}, {{group2}}, {{group3}}, {{group4}}) %>%
    summarize(Headcount = length(unique(name.full)),
              FTE = sum(percent.time)/100,
              Anualized = accounting(median(pay.annualized)),
              Actual = accounting(median(pay.actual)),
              Tenure = median(tenure.yrs)) %>%
    arrange(desc({{sort}})) %>% 
    rename(Type = type.employee, `Full Time` = full.time)
}