# Library Calls -----------------------------------------------------------

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
library(ggrepel)
library(DT)
library(plotly)
library(bookdown)

# Custom Values -----------------------------------------------------------

#   Colors

Palette <- read_ase("src/OSU_rgb_2018.ase", use_names = TRUE, .verbose = FALSE)

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

#   Lists

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

# Theme  ------------------------------------------------------------------

standard <- theme_set(theme_classic())
theme_set(standard)
#theme_update()
