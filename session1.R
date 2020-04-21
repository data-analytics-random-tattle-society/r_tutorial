setwd("~/work/covid19/tutorial")

library(tidyverse)
library(lubridate)
library(janitor)

## 1. Read the data from the repository
d_official <- read_csv("https://raw.githubusercontent.com/rahulnyk/covid19_india_data/master/covid_19_india.csv") 

## The data is in wide format with respect to the Status. 
## We need to clean it a little
d_wide_official <- d_official %>%
  ## 2. Rename State/UnionTerritory column
  rename(StateUt = "State/UnionTerritory") %>%
  ## 3. Change the Date column from character string to Date type
  mutate( Date = dmy(Date) ) %>%
  ## 4. Since there are several rows per day per state, choose the max out of all. 
  ##    Calculate number of hospitalized patients. 
  group_by(StateUt, Date) %>% 
  summarize(
    Total =  max(ConfirmedIndianNational),
    Recovered = max(Cured), 
    Deceased = max(Deaths),
    Hospitalized = Total-Recovered-Deceased
  ) %>% 
  ## 5. Add a new column to indicate source of the data. 
  ##    This will be helpful when we add other data sources to same table. 
  mutate(Source = "Ministry of Health and Family Welfare")

## 6. Pivot the data to long format
d_long_official <- d_wide_official %>% 
pivot_longer(
  c(Total, Recovered, Deceased, Hospitalized), 
  names_to = 'Status', 
  values_to = 'Cumulative'
  )

