#*******************************************************************************
# OSU Plotting Exploration and Practice
# Scott Hasenpflug
# hasenpflug7@gmail.com
# 21MAR2022
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
library(treemap)
library(ggridges)
library(gghighlight)

# End Section 0 ----

#*******************************************************************************
# 1 - Source Data Frame 
#*******************************************************************************

# Load -------------------------------------------------------------------------

load("osu_df.RData")

# Fix Factors ------------------------------------------------------------------

osu <- osu %>% 
  mutate(gender = as_factor(gender),
         type.employee = as_factor(type.employee),
         job.category = as_factor(job.category),
         rank.admin = as_factor(rank.admin),
         rank.acad = as_factor(rank.acad),
         job.type = as_factor(job.type),
         home.code = factor(home.code),
         job.code = factor(job.code),
         home.org = as_factor(home.org),
         job.org = as_factor(job.org),
         home.category = as_factor(home.category),
         contract.length = as_factor(contract.length),
         rank.name = as_factor(rank.name),
         rank.ath = as_factor(rank.ath),
         exec.cat = as_factor(exec.cat))


osu <- osu %>% 
  mutate(gender = fct_relevel(gender, c("Male", "Female")),
         type.employee = fct_relevel(type.employee, c("Faculty", "Staff")),
         rank.acad = fct_relevel(rank.acad, c("Professor", 
                    "Associate Professor",  "Assistant Professor", "Researcher",
                    "Instructor", "None")),
         job.type = fct_relevel(job.type, c("P", "S", "O")),
         contract.length = fct_relevel(contract.length, c("12", "10", "9", 
                                       "Seasonal", "Intermittent", "Limited")),
         rank.ath = fct_relevel(rank.ath, c("Head Coach", "Assoc Head Coach", 
                                            "Assistant Coach", "None")),
         exec.cat = fct_relevel(exec.cat, "Senior Executive", "Executive", 
                          "Director Level", "Manager Level", "Support", "None"))

osu <- osu %>% 
  mutate(
    rank.acad = as.ordered(rank.acad),
    job.type = as.ordered(job.type),
    contract.length = as.ordered(contract.length),
    rank.ath = as.ordered(rank.ath),
    exec.cat = as.ordered(exec.cat))


# Add Race ----------------------------------------------------------------

race <- predict_race(osu$name.first, surname = FALSE, probability = FALSE) %>% 
  select(likely_race)

race2 <- predict_race(osu$name.last, surname = TRUE, probability = FALSE) %>%
  select(likely_race) %>%
  mutate(likely_race2 = likely_race) %>%
  select(likely_race2)

races <- cbind(race, race2)

keep_race2 <- races %>%
  mutate(keep = case_when(
    likely_race == likely_race2 ~ likely_race,
    is.na(likely_race) & !is.na(likely_race2) ~ likely_race2,
    !is.na(likely_race) & is.na(likely_race2) ~ likely_race,
    likely_race == "white" & likely_race2 != "white" & !is.na(likely_race2) 
    ~ likely_race2,
    likely_race != "white" & likely_race2 == "white" & !is.na(likely_race) 
    ~ likely_race,
    TRUE ~ "Unknown"
  )) %>%
  mutate(race = keep) %>% 
  select(race) 

osu <- cbind(osu, keep_race2) %>% 
  relocate(race, .after = gender)

# Save Again --------------------------------------------------------------

save(osu, file = "osu_df.RData")

# End Section 1 ----

#*******************************************************************************
# 2 - Custom Values
#*******************************************************************************

# Colors -----------------------------------------------------------------------

# https://communications.oregonstate.edu/brand-guide/visual-identity/colors

Palette <- read_ase("OSU_rgb_2018.ase", use_names = TRUE, .verbose = FALSE)
Palette2 <- read_ase("OSU_rgb_2018.ase", use_names = FALSE, .verbose = FALSE)

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
# 3 - Plots
#*******************************************************************************

# ========================== HISTOGRAMS ----------------------------------------
# https://r-graph-gallery.com/histogram.html

# Most basic -------------------------------------------------------------------
osu %>%
  ggplot(aes(x = pay.annualized)) + 
  geom_histogram()
# Head football coach throws it off. Remove.

osu %>%
  filter(job.title != "Head Coach-Football") %>%
  ggplot(aes(x = pay.annualized)) + 
  geom_histogram()
# Execs and coaches still skew it right. President makes almost $1M

# Lets say we don't want to include coaches or execs at all.
osu %>%
  filter(rank.ath == "None" & exec.cat == "None") %>%
  ggplot(aes(x = pay.annualized)) + 
  geom_histogram()
# I don't like the scientific notation on the x axis

osu %>%
  filter(rank.ath == "None" & exec.cat == "None") %>%
  ggplot(aes(x = pay.annualized)) + 
  geom_histogram() +
  scale_x_continuous(labels = comma, n.breaks = 10) +
  # Ok but this is boring. Add a theme.
  theme_classic() +
  # Label the title and axes
  labs(title = "Annualized Salaries", subtitle = "No Executives or Coaches",
       x  = "Salary", y = "Count")

# Last thing, play with binwidth
osu %>%
  filter(rank.ath == "None" & exec.cat == "None") %>%
  ggplot(aes(x = pay.annualized)) + 
  # Set binwidth to 3000 (dollars), fill to orange, outline to black
  geom_histogram(binwidth = 3000, fill = beaver.Orange, color = 
                   paddletail.Black) +
  scale_x_continuous(labels = comma, n.breaks = 10) +
  theme_classic() +
  labs(title = "Annualized Salaries", subtitle = "No Executives or Coaches",
       x  = "Salary", y = "Count")

# Mirror -----------------------------------------------------------------------

# This isn't working
# osu %>%
#   filter(rank.ath == "None" & exec.cat == "None") %>%
#   ggplot() + 
#   geom_histogram(aes(x = pay.annualized, y = ..density..), binwidth = 3000, 
#                  fill = beaver.Orange, color = paddletail.Black) +
#   geom_histogram(aes(x = pay.actual, y = -..density..), binwidth = 3000, 
#                  fill = paddletail.Black, color = beaver.Orange) +
#   scale_x_continuous(labels = comma, n.breaks = 10) +
#   theme_classic() +
#   labs(title = "Annualized Salaries", subtitle = "No Executives or Coaches",
#        x  = "Salary", y = "Count")

# Multi ------------------------------------------------------------------------

osu %>%
  filter(rank.ath == "None" & exec.cat == "None") %>%
  ggplot(aes(x = pay.annualized, fill = type.employee)) + 
  geom_histogram(binwidth = 4000, position = "identity", alpha = .5) +
  scale_fill_manual(values = c(beaver.Orange, paddletail.Black)) +
  scale_x_continuous(labels = comma, n.breaks = 10) +
  theme_classic() +
  labs(title = "Annualized Salaries", subtitle = "By Staff/Faculty - No Executives or Coaches",
       x  = "Salary", y = "Count", fill = "Type")

# =========================== BARPLOTS -----------------------------------------
# https://r-graph-gallery.com/barplot.html

# Data -------------------------------------------------------------------------
tenure_spread <- osu %>%
  filter(str_detect(job.category, "College of")) %>%
  group_by(job.category) %>%
  summarize(Tenured = percent(sum(tenure.log)/n(),1)) %>%
  arrange(desc(Tenured))

# Most Basic -------------------------------------------------------------------

ggplot(tenure_spread, aes(job.category, Tenured)) + 
  geom_bar(stat = "identity")
# Needs ordering. Can't read labels

# Change colors ----------------------------------------------------------------
ggplot(tenure_spread, aes(job.category, Tenured)) + 
  geom_bar(fill = beaver.Orange, color = paddletail.Black, stat = "identity") +
  theme_classic()

ggplot(tenure_spread, aes(job.category, Tenured)) + 
  geom_bar(stat = "identity", fill = Palette[1:11], color = paddletail.Black) +
  theme_classic()
# The color conveys no meaning. BAD

# Reorder, width, rotate labels ------------------------------------------------
ggplot(tenure_spread, aes(x= reorder(job.category, -Tenured), Tenured)) + 
  geom_bar(stat = "identity", fill = Palette[1:11], color = paddletail.Black,
           width = .75) +
  scale_x_discrete(NULL, labels = colleges) +
  scale_y_continuous(NULL, labels = percent_format(accuracy = 1)) +
  labs(title = "Percent of Faculty Tenured", subtitle =  "by College") +
  theme(axis.text.x = element_text(angle = 45), panel.background = 
          element_blank())
# The color conveys no meaning. BAD. Need to flip to read labels

# Coord Flip, limit set --------------------------------------------------------
ggplot(tenure_spread, aes(x= reorder(job.category, Tenured), Tenured)) + 
geom_bar(stat = "identity", fill = beaver.Orange, color = paddletail.Black,
         width = .5) +
  scale_x_discrete(NULL, labels = colleges) +
  scale_y_continuous(NULL, labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Percent of Faculty Tenured", subtitle =  "by College") +
  theme(panel.background = 
          element_blank()) +
  coord_flip()

# =========================== TREEMAPS -----------------------------------------
# https://r-graph-gallery.com/treemap.html

# Data -------------------------------------------------------------------------
treemap_data <- osu %>%
  filter(str_detect(osu$job.category, "College")) %>%
  group_by(job.category, gender) %>%
  summarize(Salary = sum(pay.actual))

# Most basic -------------------------------------------------------------------
treemap(treemap_data,
        index = "job.category",
        vSize = "Salary",
        type = "index",
        )

# Subgroups --------------------------------------------------------------------
treemap(treemap_data,
        index = c("job.category", "gender"),
        vSize = "Salary",
        type = "index",
)
# This is obviously ugly. 

# Customized appearance --------------------------------------------------------

# More at https://r-graph-gallery.com/236-custom-your-treemap.html

treemap(treemap_data,
        index = "job.category",
        vSize = "Salary",
        type = "index",
        palette = Palette,
        fontsize.labels = 15,    # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        #fontcolor.labels = "bg.labels",    # Color of labels
        fontface.labels = 2,      # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        align.labels = c("center", "top"), # Where to place labels in the rectangle?
        inflate.labels = FALSE, # If true, labels are bigger when rectangle is bigger.   
        title = "Total Salary per College"
)

# ============================ VIOLIN ------------------------------------------
# https://r-graph-gallery.com/violin.html

# Most Basic -------------------------------------------------------------------
# https://r-graph-gallery.com/95-violin-plot-with-ggplot2.html

osu %>%
  filter(rank.acad != "None") %>%
  ggplot(aes(rank.acad, pay.annualized, fill = rank.acad)) +
  geom_violin()

# Control Group Order w/ dplyr -------------------------------------------------
# https://r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html

osu %>%
  filter(rank.acad != "None") %>%
  mutate(rank.acad = factor(rank.acad, levels = c("Professor", 
                            "Associate Professor", "Assistant Professor",
                            "Researcher", "Instructor"))) %>%
  ggplot(aes(rank.acad, pay.annualized, fill = rank.acad)) +
  geom_violin() 
  #gghighlight(rank.acad == "Professor")

# Horizontal -------------------------------------------------------------------
# https://r-graph-gallery.com/violin_horizontal_ggplot2.html

osu %>%
  filter(rank.acad != "None") %>%
  mutate(rank.acad = factor(rank.acad, levels = c("Professor", 
                                  "Associate Professor", "Assistant Professor",
                                  "Researcher", "Instructor"))) %>%
  ggplot(aes(rank.acad, pay.annualized, fill = rank.acad)) +
  geom_violin() +
  scale_y_continuous(labels = comma, n.breaks = 6) +
  scale_x_discrete(limits = rev) +
  gghighlight(rank.acad == "Researcher") +
  coord_flip()

# With Boxplot and other tweaks ------------------------------------------------
# https://r-graph-gallery.com/violin_and_boxplot_ggplot2.html

osu %>%
  filter(rank.acad != "None") %>%
  mutate(rank.acad = factor(rank.acad, levels = c("Professor", 
                                  "Associate Professor", "Assistant Professor",
                                  "Researcher", "Instructor"))) %>%
  ggplot(aes(rank.acad, pay.annualized, fill = rank.acad)) +
  geom_violin(fill = beaver.Orange, width = 1.2) +
  geom_boxplot(width = .2, color = "white", fill = "white", alpha = 0.3,
               outlier.shape = NA) + 
  scale_y_continuous(labels = comma, n.breaks = 6) +
  xlab("") +
  ylab("") +
  theme(legend.position = "none") + 
  theme_classic()

# Grouped Violin ---------------------------------------------------------------
# https://r-graph-gallery.com/violin_grouped_ggplot2.html

osu %>%
  filter(rank.acad != "None") %>%
  mutate(rank.acad = factor(rank.acad, levels = c("Professor", 
                                  "Associate Professor", "Assistant Professor",
                                  "Researcher", "Instructor"))) %>%
  ggplot(aes(fill = gender, x = rank.acad, y = pay.annualized)) +
  geom_violin(position = "dodge") +
  scale_y_continuous(labels = comma, n.breaks = 6) +
  xlab("") +
  ylab("") +
  theme(legend.position = "none") + 
  theme_classic()

# ======================== RIDGELINE CHARTS -------------------------------------
# https://r-graph-gallery.com/ridgeline-plot

# Data -------------------------------------------------------------------------
years <- osu %>%
  filter(str_detect(job.category, "College of")) %>%
  filter(rank.acad != "None") %>%
  group_by(job.category)

# Example ----------------------------------------------------------------------
# Ordered by mean tenure, line at mean
years %>%
  ggplot(aes(x = tenure.yrs, y = reorder(job.category, tenure.yrs, mean))) +
  geom_density_ridges(fill = beaver.Orange) +
  scale_y_discrete(labels = colleges) +
  geom_vline(xintercept = mean(years$tenure.yrs), linetype = "dotted") +
  geom_text(aes(12.5, .75), label = "OSU Mean", show.legend = FALSE) +
  labs(title = "Tenure", subtitle = "Sorted by Mean", y = NULL, x = "Years") +
  theme_minimal() 

# Can't get gghighlight to work

# =========================== BUMP CHARTS --------------------------------------
# Set-up -----------------------------------------------------------------------

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

# Bump Chart --------------------------------------------------------------------
collegeSalary2 <- pivot_longer(collegeSalary, cols = Professor:Instructor)

ggplot(collegeSalary2, aes(x = name, y = value, group = job.category)) +
  geom_line(aes(color = job.category)) +
  scale_x_discrete(limits = profAxis) +
  theme_classic()

# Rank Order Bump Chart --------------------------------------------------------
rankedSalary2 <- pivot_longer(rankedSalary, cols = Professor:Instructor)

# This needs a ton of work
ggplot(rankedSalary2, aes(x = name, y = value, group = job.category)) +
  geom_line(aes(color = job.category)) +
  scale_x_discrete(limits = profAxis) +
  scale_y_reverse() +
  theme_classic()

# ========================== SCATTERPLOTS --------------------------------------
# https://r-graph-gallery.com/scatterplot

# Most Basic --------------------------------------------------------------

osu %>%
  filter(pay.annualized < 500000) %>% 
  ggplot(aes(date.adj.serv, pay.annualized)) +
  scale_y_sqrt() +
  scale_x_date() +
  geom_point(position = "jitter", alpha = .10)

# Split by Color ----------------------------------------------------------

osu %>%
  filter(pay.annualized < 500000) %>% 
  ggplot(aes(date.adj.serv, pay.annualized, color = type.employee)) +
  scale_y_sqrt() +
  scale_x_date() +
  geom_point(position = "jitter", alpha = .15) +
  scale_colour_manual(values = c(beaver.Orange, paddletail.Black))

# Label the Outliers ------------------------------------------------------

osu %>%
  filter(pay.annualized < 500000) %>% 
  ggplot(aes(date.adj.serv, pay.annualized)) +
  scale_y_sqrt() +
  scale_x_date() +
  geom_point(position = "jitter", alpha = .15) +
  geom_text(data = osu %>% 
               filter((pay.annualized > 400000 & pay.annualized < 500000)|
                        date.adj.serv < "1973-01-01"), 
             aes(label = name.full), nudge_y = -20, nudge_x = 1000)

# Trendline ---------------------------------------------------------------

osu %>%
  filter(pay.annualized < 500000) %>% 
  ggplot(aes(date.adj.serv, pay.annualized)) +
  scale_y_sqrt() +
  scale_x_date() +
  geom_point(position = "jitter", alpha = .15) +
  geom_text(data = osu %>% 
              filter((pay.annualized > 400000 & pay.annualized < 500000)|
                       date.adj.serv < "1973-01-01"), 
            aes(label = name.full), nudge_y = -20, nudge_x = 1000) +
  geom_smooth(method = "lm", color = beaver.Orange)

# ========================== BUBBLE PLOTS --------------------------------------
# https://r-graph-gallery.com/bubble-chart

# Most Basic --------------------------------------------------------------

osu %>%
  filter(str_detect(job.category, "College")) %>%
  group_by(job.category) %>%
  summarize(nFaculty = n(), MedPay = median(pay.annualized), 
            percentTenured = (sum(tenure.log)/n())) %>%
  ggplot(aes(nFaculty, MedPay, size = percentTenured)) +
  geom_point(alpha = .5) +
  scale_size(range = c(9, 24), name = "Percent Tenured")


# ========================== 2D DENSITY PLOTS ----------------------------------
# https://r-graph-gallery.com/2d-density-chart

# Most Basic --------------------------------------------------------------

osu %>%
  filter(rank.ath == "None" & exec.cat == "None") %>%
  ggplot(aes(y = pay.annualized, x = (today() - date.adj.serv))) + 
  geom_bin2d(bins = 70) + 
  scale_fill_distiller(palette = "Oranges")

# ========================== BOXPLOTS ------------------------------------------
# https://r-graph-gallery.com/boxplot

# Most Basic --------------------------------------------------------------

osu %>% 
  filter(rank.acad != "None") %>% 
  ggplot(aes(rank.acad, pay.annualized)) +
  geom_boxplot(fill = beaver.Orange)

# With Scatterplot --------------------------------------------------------

osu %>% 
  filter(rank.acad != "None") %>% 
  ggplot(aes(rank.acad, pay.annualized)) +
  geom_boxplot(fill = beaver.Orange, outlier.shape = NA) +
  geom_jitter(color = paddletail.Black, size = .1, alpha = .3)

# With Notches ------------------------------------------------------------

osu %>% 
  filter(rank.acad != "None") %>% 
  ggplot(aes(rank.acad, pay.annualized)) +
  geom_boxplot(fill = beaver.Orange) +
  gghighlight(rank.acad == "Associate Professor")
  # Overlapping notches = low significance in difference in medians
  
# Highlighted ------------------------------------------------------------

osu %>% 
  filter(rank.acad != "None") %>% 
  ggplot(aes(rank.acad, pay.annualized)) +
  geom_boxplot(fill = beaver.Orange, notch = TRUE, notchwidth = .7)

# Overlapping notches = low significance in difference in medians

# Faceted -----------------------------------------------------------------

osu %>% 
  filter(rank.acad != "None") %>% 
  ggplot(aes(rank.acad, pay.annualized, fill = gender)) +
  geom_boxplot() +
  facet_wrap(~ rank.acad, scale = "free")

# Oohhh
osu %>% group_by(rank.acad, gender) %>% summarize(median(tenure.yrs))

# ==== LOLLIPOP PLOT -----------------------------------------------------------
# https://r-graph-gallery.com/lollipop-plot

# Most Basic --------------------------------------------------------------

osu %>% 
  group_by(job.category) %>% 
  summarize(Org = job.category, Budget = sum(pay.actual), Pax = n()) %>% 
  ggplot(aes(x = reorder(Org, Pax), y = Budget)) +
  geom_point() +
  geom_segment(aes(x = Org, xend = Org, y = 0, yend = Budget)) +
  coord_flip()

# Cleveland Dot Plot  ----------------------------------------------------------

osu %>% 
  group_by(job.category) %>% 
  summarize(Org = job.category,
            Male = median(pay.annualized[gender == "Male"]),
            Female = median(pay.annualized[gender == "Female"])) %>% 
  ggplot() +
  geom_segment(aes(x = Org, xend = Org, y = Male, yend = Female), 
               color="grey") +
  geom_point(aes(x = Org, y = Male), color = "blue", size=3 ) +
  geom_point(aes(x = Org, y = Female), color = "pink", size=3 ) +
  coord_flip() +
  theme_classic()

# Highlighted -------------------------------------------------------------

osu %>% 
  group_by(job.category) %>% 
  summarize(Org = job.category,
            Male = median(pay.annualized[gender == "Male"]),
            Female = median(pay.annualized[gender == "Female"])) %>% 
  ggplot() +
  geom_segment(aes(x = Org, xend = Org, y = Male, yend = Female), 
               color="grey") +
  geom_point(aes(x = Org, y = Male), color = "blue", size=3 ) +
  geom_point(aes(x = Org, y = Female), color = "pink", size=3 ) +
  coord_flip() +
  theme_classic() +
  gghighlight(Female > Male)

# Ordered by absolute value of difference in pay --------------------------
osu %>% 
  group_by(job.category) %>% 
  summarize(Org = job.category,
            Male = median(pay.annualized[gender == "Male"]),
            Female = median(pay.annualized[gender == "Female"])) %>% 
  ggplot() +
  geom_segment(aes(x = reorder(Org, abs(Male - Female)), xend = Org, y = Male, 
                   yend = Female), color="grey") +
  geom_point(aes(x = Org, y = Male), color = "blue", size=3) +
  geom_point(aes(x = Org, y = Female), color = "pink", size=3) +
  coord_flip() +
  theme_classic() +
  gghighlight(Female > Male)


