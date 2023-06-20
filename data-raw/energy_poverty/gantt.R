library(ganttrify)
library(tibble)
library(dplyr)
library(lubridate)
library(ggplot2)

pdate <- function(week) {
  b <- make_date(year = 2023, month = 7, day = 10)
  b + dweeks(week - 1)
}

project <- tribble(
  ~wp, ~activity, ~start_date, ~end_date,
  "Conceptualization", "Theory (assemblage?)", pdate(1), pdate(3),
  "Conceptualization", "Analytical framework", pdate(1), pdate(3),
  "Conceptualization", "Literature review", pdate(2), pdate(4),
  "Conceptualization", "Trust + social cohesion", pdate(1), pdate(2),
  "Methodology", "Data cleaning", pdate(1), pdate(2),
  "Methodology", "Adding noise", pdate(2), pdate(4),
  "Methodology", "GWPCA", pdate(4), pdate(7),
  "Methodology", "Multilevel SEM", pdate(6), pdate(10),
  "Results", "Reporting", pdate(7), pdate(10),
  "Results", "Visualization", pdate(7), pdate(11),
  "Results", "Discussion / Linking to theory", pdate(10), pdate(15),
  "Results", "Policy recommendations", pdate(13), pdate(15),
  "Dissemination", "Deliverable 4.6", pdate(5), pdate(13),
  "Dissemination", "1st paper", pdate(1), pdate(15),
  "Dissemination", "2nd paper", pdate(1), pdate(16),
  "Dissemination", "Tool module", pdate(10), pdate(13)
)

spots <- tribble(
  ~activity, ~spot_type, ~spot_date,
  "Deliverable 4.6", "X", as.Date("2023-09-30"),
  "Tool module", "X", as.Date("2023-09-30"),
  "Conceptualization", "ASAP!", as.Date("2023-07-20"),
  "Methodology", "Take time", as.Date("2023-08-12"),
  "Results", "Possibly less time needed", as.Date("2023-09-20"),
  "Dissemination", "Continuous work", as.Date("2023-08-30"),
  "Theory (assemblage?)", "J", as.Date("2023-07-18"),
  "Analytical framework", "J", as.Date("2023-07-18"),
  "Literature review", "J", as.Date("2023-07-24"),
  "Trust + social cohesion", "J,D", as.Date("2023-07-14"),
  "Data cleaning", "J", as.Date("2023-07-13"),
  "Adding noise", "J", as.Date("2023-07-24"),
  "GWPCA", "J", as.Date("2023-08-10"),
  "Multilevel SEM", "J,S", as.Date("2023-08-28"),
  "Reporting", "J,S", as.Date("2023-08-31"),
  "Visualization", "J,S", as.Date("2023-09-04"),
  "Discussion / Linking to theory", "J,D?", as.Date("2023-09-28"),
  "Policy recommendations", "J", as.Date("2023-10-08"),
  "Deliverable 4.6", "J,S,D", as.Date("2023-09-01"),
  "1st paper", "J,D,S", as.Date("2023-08-31"),
  "2nd paper", "J,D,S", as.Date("2023-08-31"),
  "Tool module", "J", as.Date("2023-09-22")
)

gantt <- (ganttrify(
  project,
  spots,
  project_start_date = as.Date("2023-07-10"),
  by_date = TRUE,
  exact_date = TRUE,
  month_number_label = FALSE,
  spot_border = 0.1
) +
  ggtitle("GRETA plans for July - October")) %>%
  ggsave(filename = "gantt.png")


# Conceptualization
# - What is energy vulnerability?
#   - Conceptually (assemblage geographies)
#   - Analytically
# - Literature review on energy vulnerability indices  
# - What is the link between energy vulnerability, trust and social cohesion?
# 
# Methodology
# - Data cleaning
# - Adding noise
# - GWPCA
# - Multilevel SEM
# 
# 
# Results
# - Reporting
# - Visualization
# - Discussion / Linking to theory
# - Policy recommendations
# 
# Dissemination
# - 1st paper
# - 2nd paper
# - D4.6
# - GRETA Analytics module
