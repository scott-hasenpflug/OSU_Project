#*******************************************************************************
# OSU Exploratory Analysis
# Scott Hasenpflug
# hasenpflug7@gmail.com
# 14MAR2022
#*******************************************************************************

#*******************************************************************************
# 0 - Load libraries
#*******************************************************************************

# Library() calls --------------------------------------------------------------

library(tidyverse)
library(pdftools)
library(rebus)
library(lubridate)
library(tidytext)
library(skimr)
library(scales)
library(knitr)
library(stringr)
library(tibble)
library(gender)
library(formattable)
library(swatches)

# End Section 0 ----

#*******************************************************************************
# 1 - Source/Transform Data Frame 
#*******************************************************************************

# Load -------------------------------------------------------------------------

load("osu_wth_appt_df.RData")

# Fix the remaining unknown genders --------------------------------------------

# Since the gender package seems to link to rate limited websites I already
#   exceeded in testing, I am finalizing the gender assignment in this script
#   so I don't need to run script 5 again.

# Make a list of ids of "Unknowns" which I determined to be male with Google 
#   and eyeballs.
male_fac <- c(2692, 5504, 3960, 3361, 4159, 6150, 2613, 2783, 988, 976, 3281, 
              2766, 451, 4993, 6206, 986, 6227, 963, 2451, 2452, 164, 5150,
              6270, 2489, 6207, 3410, 5906, 3474, 5569, 1039)
male_fac <- paste0("F-", male_fac)
male_staff <- c(884, 1395, 13, 735)
male_staff <- paste0("S-", male_staff)
male_list <- c(male_fac, male_staff)

osu_wth_appt <- osu_wth_appt %>%
  mutate(gender= case_when(
    id %in% male_list ~ "Male",
    gender == "Unknown" & !(id %in% male_list) ~ "Female",
    TRUE ~ as.character(gender)
  ))

# Annualize Part-time salary ---------------------------------------------------

# Make pay.annualized equal the pay rate for hourly and monthly employees if 
#   they were full-time (pay.annualized). Round both "pay" columns. 
#   Move full.time between pay.actual (The actual salary cost of the employee) 
#   and pay.annualized for ease of reading. Remove pay.monthly.equiv since it is
#    now redundant.
osu_wth_appt <- osu_wth_appt %>%
  mutate(pay.annualized = case_when(
    pay.annualized == 0 & is.na(pay.monthly) ~ pay.hourly * 2080,
    pay.annualized == 0 & is.na(pay.hourly) ~ pay.monthly * 12,
    TRUE ~ as.numeric(pay.annualized)
  )) %>%
  mutate(pay.actual = round(pay.actual),
         pay.annualized = round(pay.annualized)) %>%
  relocate(full.time, .after = pay.actual)

# Logical "Football" -----------------------------------------------------------

osu_wth_appt <- osu_wth_appt %>%
  mutate(football = str_detect(job.title, "Football|FB"))

# Capture coach ranks in rank.ath -----------------------------------------------
osu_wth_appt <- osu_wth_appt %>%
  mutate(rank.ath = case_when(
    str_detect(job.title, "Head Coach") & !(str_detect(
      job.title, "Associate")) ~ "Head Coach",
    str_detect(job.title, "Head Coach") & str_detect(
      job.title, "Associate|Assoc HC") ~ "Assoc Head Coach",
    str_detect(job.title, "Asst Coach|Assistant Coach|Assoc Coach|Cond Coach") 
    ~ "Assistant Coach",
    TRUE ~ "None"),
         job.title = case_when(
    name.full == "Foster, Allison K" ~ "Assoc Head Coach-Women's Rowing",
    TRUE ~ as.character(job.title)))

# Turn ADs into VPs ------------------------------------------------------------

# Assumption I'm making that they're equivalent
osu_wth_appt <-  osu_wth_appt%>%
  mutate(rank.admin = case_when(
    str_detect(job.title, "Assoc AD|AssocAD") ~ "Associate AD",
    str_detect(job.title, "Assistant AD|Asst AD") ~ "Assistant AD",
    TRUE ~ as.character(rank.admin)
  ))

# Categorize rank.admin --------------------------------------------------------

sen_exec <- c("Dean", "President", "Vice President", "Vice Provost") 
exec <- c("Assistant Vice Provost", "Assistant VP", "Associate Vice Provost",
      "Associate VP")
dir_lev <- c("Assistant Director", "Associate AD", "Associate Dean",
      "Associate Director", "Associate Head", "Associate School Head", 
      "Chair", "Department Head", "Director", "School Head")
man_lev <- c("Assistant Dean", "Assistant AD", "Assistant Manager",
      "Assistant School Head", "Associate Department Head", "Associate Manager",
      "Manager")
asst <- c("Administrative Assistant", "Executive Assistant")

osu_wth_appt <- osu_wth_appt %>%
  mutate(exec.cat = case_when(
    rank.admin %in% sen_exec ~ "Senior Executive",
    rank.admin %in% exec ~ "Executive",
    rank.admin %in% dir_lev ~ "Director Level",
    rank.admin %in% man_lev ~ "Manager Level",
    rank.admin %in% asst ~ "Support",
    TRUE ~ "None"
  ))

# Remove "Lecturers" -----------------------------------------------------------

osu_wth_appt <- osu_wth_appt %>%
  mutate(rank.acad = case_when(
    rank.acad == "Lecturer" ~ "Instructor",
    TRUE ~ as.character(rank.acad)))

# Fix part-time monthly annualized pay -----------------------------------------

osu_wth_appt <- osu_wth_appt %>%
  mutate(pay.annualized = case_when(full.time == FALSE & !is.na(pay.monthly) 
      ~ as.numeric(12* (pay.monthly / (percent.time/100))),
    TRUE ~ as.numeric(pay.annualized)
  ))

# Split and save data frames again ---------------------------------------------
# Save data frames separately so I can analyze without appointees or fellows
osu <- osu_wth_appt %>%
  filter(appointee == FALSE, pay.actual != 0, job.title != "Graduate Fellow")

save(osu, file = "osu_df.RData")
save(osu_wth_appt, file = "osu_wth_appt_df.RData")

# osu is the working data frame. osu_wth_appt is saved in the WD but I will 
#   remove it from the global environment for this script, along with other 
#   unneeded values
rm(osu_wth_appt, male_fac, male_list, male_staff, asst, dir_lev, exec, man_lev,
   sen_exec)


# Load osu ---------------------------------------------------------------------
load("osu_df.RData")
# End Section 1 ----

#*******************************************************************************
# 2 - Custom Values
#*******************************************************************************
# Start ----
# Colors -----------------------------------------------------------------------

# https://communications.oregonstate.edu/brand-guide/visual-identity/colors

Palette <- read_ase("OSU_rgb_2018.ase", use_names = TRUE, .verbose = FALSE)

beaver.Orange <- "#D73F09"
paddletail.Black <- "#000000"
pane.Stand <-  "#4A773C"
high.Tide <- "#00859B"
luminance <- "#FFB500#"
stratosphere <- "#006A8E"
reindeer.Moss <- "#C4D6A4"
seafoam <- "#B8DDE1"
candela <- "#FDD26E"
moondust <- "#C6DAE7"
hop.Bine <- "#AA9D2E"
rogue.Wave <- "#0D5257"
solar.Flare <- "#D3832B"
star.Canvas <- "#003B5C"
till <- "#B7A99A"
coastline <- "#A7ACA2"
high.Desert <- "#7A6855"
crater <- "#8E9089"

# Fonts ------------------------------------------------------------------------

# https://communications.oregonstate.edu/brand-guide/visual-identity/typography

# "Stratum 2" (display font, headlines at 18 points or larger, no body copy) 
#   "Impact as a substitute.

# "Georgia" (serif font. Best for headliners and subheads, not body)

# "Kievit" (smaller headlines, body copy and captions)
#   "Verdana" as a substitute.

# Business correspondence: 
#   Serif: "Georgia"
#   Sans Serif: "Calibri"

# Lists ------------------------------------------------------------------------
colleges <- {c("College of Agriculture" = "Agriculture",
                   "College of Business" = "Business",
                   "College of Earth, Ocean, and Atmospheric Sciences" = "EO&A",
                   "College of Education" = "Education",
                   "College of Engineering" = "Engineering",
                   "College of Forestry" = "Forestry",
                   "College of Liberal Arts" = "LibArts",
                   "College of Pharmacy" = "Pharmacy",
                   "College of Public Health and Human Services" = "Health",
                   "College of Science" = "Science",
                   "College of Veterinary Medicine" = "Veterinary")}

# End Section 2 ----

#*******************************************************************************
# 3 - Custom Functions
#*******************************************************************************

# Complete =====================================================================
# Headcount/FTE ----------------------------------------------------------------
report_head <- function(group1 = home.category, group2 = NULL, group3 = NULL, 
                        group4 = NULL, sort = headcount) {
  osu %>%
    group_by({{group1}}, {{group2}}, {{group3}}, {{group4}}) %>%
    summarize(headcount = length(unique(name.full)),
              fte = sum(percent.time)/100,
              median_annualized_pay = accounting(median(pay.annualized)),
              median_actual_pay = accounting(median(pay.actual)),
              average_tenure = mean(tenure.yrs)) %>%
    arrange(desc({{sort}}))
}
# Test
report_head(sort = median_annualized_pay) %>% filter(headcount < fte)
# How are 6 headcounts lower than FTE?

# A lot of people have personal headcounts over 1
osu %>% group_by(name.full) %>% summarize(fte = sum(percent.time/100)) %>%
  filter(fte > 1) %>% arrange(desc(fte))

# This shows who they work for. Most have jobs in the same org but not all
osu %>% group_by(name.full) %>% 
  summarize(fte = sum(percent.time/100), org = list(job.category)) %>%
  arrange(desc(fte)) # %>% view()

# Spending ---------------------------------------------------------------------
report_spend <- function(group = home.category, sort = cost) {
  budget <- sum(osu$pay.actual)
  osu %>%
    group_by({{group}}) %>%
    summarise(cost = accounting(sum(pay.actual),0), 
              share = percent(sum(pay.actual)/sum(budget)),
              fte = accounting(sum(pay.actual)/(sum(percent.time)/100),0),
              head = accounting(sum(
                pay.actual)/length(unique(name.full)),0)) %>%
    relocate({{sort}}, .after = {{group}}) %>%
    arrange(desc({{sort}}))
}
# Test
report_spend()

# Incomplete ===================================================================
# Tenure -----------------------------------------------------------------------
report_tenure <- function(var) {
  # Action
}

# End Section 3 ----

#*******************************************************************************
# 4 - Customer Requests
#*******************************************************************************

# * Headcount/FTE ------------------------------------------------------------

# Headcount
#   I wrote a function: report_head

# Examples:
report_head()
report_head(type.employee)
report_head(type.employee, full.time)
report_head(type.employee, full.time, loaner)
report_head(type.employee, full.time, loaner, job.type)
report_head(type.employee, full.time, loaner, job.type, sort = median_annualized_pay)
report_head(sort = average_tenure)


# * Gender Splits --------------------------------------------------------------
#   Examine differences in gender.

# Annualized Salary by Gender and Type
osu %>%
  group_by(gender, type.employee) %>%
  summarize(Median_Salary = accounting(median(pay.annualized),0),
            Average_Salary = accounting(mean(pay.annualized),0),
            Count = n()) %>%
  arrange(desc(Median_Salary))

# By employee type
osu %>%
  group_by(type.employee) %>%
  summarize(Male = percent(sum(gender == "Male")/n(), .1),
            Female = percent(sum(gender == "Female")/n(), .1),
            Count = n())

# Percent tenured by gender
osu %>%
  filter(type.employee == "Faculty") %>%
  group_by(gender) %>%
  summarize(Tenured = percent(sum(tenure.log == TRUE) / 
                                sum(tenure.log == FALSE), 1),
            Count = n())


# Tenure v Salary --------------------------------------------------------------

# Messing around. See Script 7.
osu %>%
  filter(pay.annualized < 500000) %>%
  ggplot(aes(tenure.yrs, pay.annualized)) +
  geom_point(position = "jitter") + 
  scale_y_continuous(labels = comma)

osu %>%
  filter(pay.annualized < 500000) %>%
  ggplot(aes(tenure.yrs, pay.annualized, color = type.employee)) +
  geom_point(position = "jitter", alpha = .05) + 
  scale_y_sqrt(labels = comma, n.breaks = 10) + 
  scale_x_binned(n.breaks = 20) +
  theme_classic()

# Salary of professor v other instructor by employment length ------------------

## Combined
osu %>%
  filter(rank.acad != "None") %>%
  group_by(rank.acad) %>%
  summarize(tenure = tenure.yrs, pay = pay.annualized) %>%
  ggplot(aes(tenure, pay, color = rank.acad)) +
    geom_point(position = "jitter") +
    scale_y_continuous(labels = comma) +
    theme_classic()

## Facet wrapped by rank
osu %>%
  filter(rank.acad != "None") %>%
  group_by(rank.acad) %>%
  summarize(tenure = tenure.yrs, pay = pay.annualized) %>%
  ggplot(aes(tenure, pay, color = rank.acad)) +
  geom_point(position = "jitter") +
  scale_y_continuous(labels = comma) +
  theme_classic() +
  facet_wrap(~rank.acad)


# * Is the % of tenured faculty consistent across units? -------------------------

tenure_spread <- osu %>%
  filter(str_detect(job.category, "College of")) %>%
  group_by(job.category) %>%
  summarize(Tenured = percent(sum(tenure.log)/n(),1)) %>%
  arrange(desc(Tenured))

ggplot(tenure_spread, aes(x= reorder(job.category, -Tenured), Tenured)) + 
  geom_col(fill = beaver.Orange) + 
  scale_x_discrete(NULL, labels = colleges) +
  scale_y_continuous(NULL, labels = percent_format(accuracy = 1)) +
  theme_minimal() + 
  labs(title = "Percent of Faculty Tenured", subtitle =  "by College")



# Layer median salary on top of above graph * ----------------------------------

tenure_spread2 <- osu %>%
  filter(str_detect(job.category, "College of")) %>%
  group_by(job.category) %>%
  summarize(Tenured = percent(sum(tenure.log)/n(),1))

med_pay <-osu %>%
  filter(str_detect(job.category, "College of") & tenure.log == TRUE) %>%
  group_by(job.category) %>%
  summarize(Salary = median(pay.annualized))

# This data frame has the right data for the chart
layered_try <- full_join(tenure_spread2, med_pay) 
rm(tenure_spread2, med_pay)

#Try 1
ggplot(layered_try, aes(x= reorder(job.category, -Tenured), Tenured)) + 
  geom_col(fill = beaver.Orange) + 
  geom_point(aes(size = Salary)) +
  scale_x_discrete(NULL, labels = colleges) +
  scale_y_continuous(NULL, labels = percent_format(accuracy = 1)) +
  theme_minimal() + 
  labs(title = "Percent of Faculty Tenured", subtitle =  "by College")

ggplot(layered_try, aes(x= reorder(job.category, -Tenured), Tenured)) + 
  geom_segment(aes(x=job.category, xend=job.category, y=0, yend=Tenured)) +
  geom_point(aes(color = beaver.Orange, size = Salary)) +
  scale_size_area(max_size = 30) +
  scale_x_discrete(NULL, labels = colleges) +
  scale_y_continuous(NULL, labels = percent_format(accuracy = 1)) +
  theme_minimal() + 
  labs(title = "Percent of Faculty Tenured", subtitle =  "by College")



# * Pay rates for different types of faculty * ---------------------------------

#Admin
osu %>%
  filter(rank.admin != "None") %>%
  group_by(rank.admin) %>%
  summarize(Pay = median(pay.annualized)) %>%
  arrange(desc(Pay)) %>%
  ggplot(aes(x= reorder(rank.admin, -Pay), Pay)) + 
    geom_col(fill = paddletail.Black) +
    theme(axis.text.x = element_text(angle = 90)) +
    theme_minimal() +
    labs(title = "Median Annualized Salart", subtitle =  "by Administrative Rank")

#Academic
osu %>%
  filter(rank.acad != "None") %>%
  group_by(rank.acad) %>%
  summarize(Pay = median(pay.annualized)) %>%
  arrange(desc(Pay)) %>%
  ggplot(aes(x= reorder(rank.acad, -Pay), Pay)) + 
    geom_col(fill = beaver.Orange) +
    theme(axis.text.x = element_text(angle = 90)) +
    theme_minimal()



# Overlap in compensation rates across Faculty and Staff * ---------------------

# Between all of Faculty and Staff
## Removed execs, head coaches, and everyone football
osu %>%
  filter(rank.ath != "Head Coach", football == FALSE, exec.cat == "None") %>%
  group_by(type.employee) %>%
  summarize(pay = pay.annualized) %>%
  ggplot(aes(type.employee, pay)) +
    geom_boxplot() + 
    scale_y_continuous(labels = comma)

a <- osu %>%
  filter(type.employee == "Faculty", rank.ath != "Head Coach", football == FALSE, exec.cat == "None")
quantile(a$pay.annualized)

b <- osu %>%
  filter(type.employee == "Staff")
quantile(b$pay.annualized)

## IQR overlap: $55,071 - $60,792


# * Compare IT compensation specifically ---------------------------------------

osu %>% 
  filter(job.category  == "University Information and Technology") %>% 
  group_by(type.employee) %>%
  summarize(Salary = median(pay.annualized))

osu %>% 
  filter(job.category  == "University Information and Technology") %>% 
  group_by(type.employee) %>%
  summarize(pay = pay.annualized) %>%
  ggplot(aes(type.employee, pay)) +
  geom_boxplot() + 
  scale_y_continuous(labels = comma)




# Director position title inflation * ------------------------------------------

unique(osu$rank.admin)

osu %>%
  filter(rank.admin %in% c("Director", "Assistant Director", 
                           "Associate Director")) %>%
  count(type.employee)

osu %>%
  filter(rank.admin %in% c("Director", "Assistant Director", 
                           "Associate Director")) %>%
  group_by(job.category, rank.admin) %>%
  summarize(median(pay.annualized))
# How are we defining "inflation?




# * Percentage of titles: Manager, Director, Executive Director, AVP/VP --------

osu %>%
  filter(rank.admin != "None") %>%
  group_by(rank.admin) %>%
  summarize(Number = n(), Percent = percent(n() / nrow(osu)), 
            Of_Ranks = percent(n()/sum(osu$rank.admin != "None"))) %>%
  arrange(desc(Percent))


# * How many VPs and Deans (senior leaders) have been there over 5 years? ------

osu %>%
  filter(rank.admin != "None") %>%
  group_by(rank.admin) %>%
  summarize(Number = n(), Over_5 = sum(tenure.yrs >= 5), 
             Percent = Over_5/n()) %>%
  arrange(desc(Percent))


# * Where has the most hiring occured in the last 2 years (by unit and job)? -----

osu %>%
  group_by(job.category) %>%
  summarize(Newbies = percent(sum(tenure.yrs <= 1)/n(), 1), 
            newbieCount = sum(tenure.yrs <= 1), totalCount = n()) %>%
  arrange(desc(Newbies))

osu %>%
  filter(exec.cat != "None") %>%
  group_by(exec.cat) %>%
  summarize(Newbies = percent(sum(tenure.yrs <= 1)/n(), 1), 
            newbieCount = sum(tenure.yrs <= 1), totalCount = n()) %>%
  arrange(desc(Newbies))


# * What is the ratio of professors to instructors in all colleges? ------------

osu %>%
  filter(rank.acad != "None", str_detect(job.category, "College")) %>%
  group_by(job.category) %>%
  summarize(Professors = sum(rank.acad %in% c("Professor", 
                              "Associate Professor", "Assistant Professor")),
            nonProfs = sum(rank.acad %in% c("Instructor", "Researcher",
                                            "Lecturer")), 
            Ratio = paste0(round(Professors/nonProfs, 2)," : ","1")) %>%
  arrange(desc(Ratio))


# * What colleges have the highest salaries for professors? * ------------------

# Arrange the x axis
profAxis <- c("Professor", "AssocProfessor", "AssistProfessor",
              "Instructor", "Researcher")

# Median Salary for Academics by College
acadSalary <- osu %>% 
  filter(rank.acad != "None", str_detect(job.category, "College")) %>%
  group_by(rank.acad) %>%
  summarize(Median = median(pay.annualized)) %>%
  arrange(desc(Median))

# Median Salary for Academics by College
byCollege <- osu %>% 
  filter(rank.acad != "None", str_detect(job.category, "College")) %>%
  group_by(job.category) %>%
  summarize(Median = median(pay.annualized)) %>%
  arrange(desc(Median))

# Median Salary for Academics by College and Position
collegeSalary <- osu %>% 
  filter(rank.acad != "None", str_detect(job.category, "College")) %>%
  group_by(job.category) %>%
  summarize(Professor = median(pay.annualized[rank.acad == "Professor"]),
            AssocProfessor = median(
              pay.annualized[rank.acad == "Associate Professor"]),
            AssistProfessor = median(
              pay.annualized[rank.acad == "Assistant Professor"]),
            Researcher = median(
              pay.annualized[rank.acad == "Researcher"], na.rm = TRUE),
            Instructor = median(
              pay.annualized[rank.acad == "Instructor"])) %>%
  arrange(desc(Professor))

# Median Salary for Academics by College and Position in Rank Order
rankedSalary <- osu %>% 
  filter(rank.acad != "None", str_detect(job.category, "College")) %>%
  group_by(job.category) %>%
  summarize(Professor = median(
    pay.annualized[rank.acad == "Professor"]),
            AssocProfessor = median(
              pay.annualized[rank.acad == "Associate Professor"]),
            AssistProfessor = median(
              pay.annualized[rank.acad == "Assistant Professor"]),
            Researcher = median(
              pay.annualized[rank.acad == "Researcher"], na.rm = TRUE),
            Instructor = median(
              pay.annualized[rank.acad == "Instructor"])) %>%
  arrange(desc(Professor)) %>%
  mutate(Professor = rank(-Professor),
         AssocProfessor = rank(-AssocProfessor),
         AssistProfessor = rank(-AssistProfessor),
         Researcher = rank(-Researcher),
         Instructor = rank(-Instructor))

# Salary Bump Chart
collegeSalary2 <- pivot_longer(collegeSalary, cols = Professor:Instructor)

ggplot(collegeSalary2, aes(x = name, y = value, group = job.category)) +
  geom_line(aes(color = job.category)) +
  scale_x_discrete(limits = profAxis) +
  theme_classic()

# Salary Bump Chart
collegeSalary2 <- pivot_longer(collegeSalary, cols = Professor:Instructor)

ggplot(collegeSalary2, aes(x = name, y = value, group = job.category)) +
  geom_line(aes(color = job.category)) +
  scale_x_discrete(limits = profAxis) +
  theme_classic()

# Rank Order Bump Chart (Needs a ton of work)
rankedSalary2 <- pivot_longer(rankedSalary, cols = Professor:Instructor)

ggplot(rankedSalary2, aes(x = name, y = value, group = job.category)) +
  geom_line(aes(color = job.category)) +
  scale_x_discrete(limits = profAxis) +
  scale_y_reverse() +
  theme_classic()




# * Range of compensation and tenure for exec/admin assistants -------------------

## Compensation
osu %>%
  filter(exec.cat == "Support") %>%
  group_by(rank.admin) %>%
  summarize(pay = pay.annualized) %>%
  ggplot(aes(rank.admin, pay)) +
    geom_boxplot() +
    scale_y_continuous(labels = comma)

## Tenure
osu %>%
  filter(exec.cat == "Support") %>%
  group_by(rank.admin) %>%
  summarize(Tenure = tenure.yrs) %>%
  ggplot(aes(rank.admin, Tenure)) +
    geom_boxplot() 
  

osu %>% count(rank.admin == "Executive Assistant")

# * Most common staff titles? --------------------------------------------------

osu %>%
  filter(type.employee == "Staff") %>%
  count(job.title, sort = TRUE)


# Headcount visual per school by job bracket and employee class ----------------
# Distribution of salaries (by bin) of Director titles vs. Manager -------------
# More granular salary and tenure bins -----------------------------------------
# Network diagram - What orgs hire from what orgs ------------------------------
# Network diagram - Relationship between Extension Service and College ---------
# End Section 4 ----

#*******************************************************************************
# 5 - My Exploration
#*******************************************************************************

# Check for duplicate full names * -----------------------------------------------
osu$name.full[duplicated(osu$name.full)]

#doesn't work because of multiple jobs. Come back later'

# Can I break out tenure/tenure track? -----------------------------------------
# Add title of person's supervisor to some degree, maybe just school prez ------
# Salary model -------- Needs Update -------------------------------------------

# Original model
mdl_pay_osu <- lm(pay.annualized ~ gender + type.employee + tenure.yrs +
                  loaner + percent.time + rank.admin + rank.acad + job.category
                  + senior + job.type, osu)

# Filter out head football coach
osu_no_hfc <- osu %>%
  filter(name.full != "Smith, Jonathan C")

# Filter out top paid in athletics (mostly coaches but captures AD and astaff)
osu_no_top_coaches <-  osu %>%
  filter(job.code != "YIA" | pay.annualized < 150000)

# Filter out all in athletic department
osu_no_YIA <-  osu %>%
  filter(job.code != "YIA")

# Original model
mdl_pay_osu <- lm(pay.annualized ~ gender + type.employee + tenure.yrs +
                  loaner + percent.time + rank.admin + rank.acad + job.category 
                  + senior + job.type, osu)

mdl_pay_osu %>%
  glance() %>%
  pull("adj.r.squared")
# 0.510914

# Model no head football coach
mdl_pay_no_hfc <- lm(pay.annualized ~ gender + type.employee + tenure.yrs +
                       loaner + percent.time + rank.admin + rank.acad + job.category + 
                       senior + job.type, osu_no_hfc)

mdl_pay_no_hfc %>%
  glance() %>%
  pull("adj.r.squared")
# 0.6515625

# Model no coaches
mdl_pay_no_coaches <- lm(pay.annualized ~ gender + type.employee + tenure.yrs +
                           loaner + percent.time + rank.admin + rank.acad + job.category + 
                           senior + job.type, osu_no_top_coaches)

mdl_pay_no_coaches %>%
  glance() %>%
  pull("adj.r.squared")
# 0.745358

# Model no athletic department
mdl_pay_no_YIA <- lm(pay.annualized ~ gender + type.employee + tenure.yrs +
                       loaner + percent.time + rank.admin + rank.acad + job.category + 
                       senior + job.type, osu_no_YIA)

mdl_pay_no_YIA %>%
  glance() %>%
  pull("adj.r.squared")
# 0.7493115

# Massive improvements when removing HFC and then top coaches from the model.
#   I don't need to do this though, I just need to capture their coach status
#   in admin.rank, and possibly their association with football specifically,
#   then re-run the model. Eliminating all of YIA did not improve the model
#   after I had removed the top coaches.

# Next: Go back to script 5 and capture in rank.admin: 
#     (Sr) Assoc ADs
#     (Asst/Assoc) Head coach or other top coaches
#     Maybe a logical for "football"
