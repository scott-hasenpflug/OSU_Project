report_spend <- function(group = home.category, sort = Cost) {
  budget <- sum(osu$pay.actual)
  osu %>%
    group_by({{group}}) %>%
    summarise(Cost = accounting(sum(pay.actual),0), 
              Share = percent(sum(pay.actual)/sum(budget)),
              `per FTE` = accounting(sum(pay.actual)/(sum(percent.time)/100),0),
              `per Headcount` = accounting(sum(
                pay.actual)/length(unique(name.full)),0)) %>%
    relocate({{sort}}, .after = {{group}}) %>%
    arrange(desc({{sort}})) %>% 
    rename(Division = home.category)
}