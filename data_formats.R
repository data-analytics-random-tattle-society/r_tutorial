library(tidyverse)
library(lubridate)
library(janitor)

d_official <- read_csv("https://raw.githubusercontent.com/rahulnyk/covid19_india_data/master/covid_19_india.csv") 

d_wide_official <- d_official %>%
  rename(StateUt = "State/UnionTerritory") %>%
  mutate( Date = dmy(Date) ) %>%
  group_by(StateUt, Date) %>% 
  ## Chosing the max number in case there are multiple entries for same date. 
  summarize(
    Total =  max(ConfirmedIndianNational),
    Recovered = max(Cured), 
    Deceased = max(Deaths),
    Hospitalized = Total-Recovered-Deceased
  ) %>% mutate(Source = "Ministry of Health and Family Welfare")

d_long_official <- d_wide_official %>% pivot_longer(c(Total, Recovered, Deceased, Hospitalized), names_to = 'Status', values_to = 'Cumulative')
  

p <- ggplot(data = d_long_official, aes(x = StateUt, y = Cumulative, fill = Status)) +
  geom_bar(stat = 'identity') + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p)


# 
# 
# ### World
# 
# jhu_url_confirmed <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
#                            "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
#                            "time_series_covid19_confirmed_global.csv", sep = "")
# jhu_url_recovered <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
#                            "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
#                            "time_series_covid19_recovered_global.csv", sep = "")
# jhu_url_deaths <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
#                         "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
#                         "time_series_covid19_deaths_global.csv", sep = "")
# d1 <- read_csv(jhu_url_confirmed) %>%
#   mutate(Type = 'Total')
# 
# d2 <- read_csv(jhu_url_recovered) %>%
#   mutate(Type = 'Recovered')
# 
# d3 <- read_csv(jhu_url_deaths) %>%
#   mutate(Type = 'Deaths') 
# 
# dj <- rbind(d1, d2, d3) %>%
#   rename(StateUt = "Province/State", Country = "Country/Region") %>% 
#   pivot_longer(-c(Type, StateUt, Country, Lat, Long), names_to = "Date", values_to = "Total") %>%
#   pivot_wider(names_from = Type, values_from = Total, values_fill = list(Total = 0)) %>%
#   mutate(Hospitalized = Total-Recovered-Deaths) %>% mutate( Date = as.Date(Date, format="%m/%d/%y") )
# 
# rm(d1, d2, d3)
# 
# dji <- dj %>% filter(Country == 'India')
# 
# 
# ### Crowd Sourced Data
# 
# url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSc_2y5N0I67wDU38DjDh35IZSIS30rQf7_NYZhtYYGU1jJYT6_kDx4YpF-qw0LSlGsBYP8pqM_a1Pd/pub?output=csv'
# d_crowd_long <- read_csv(url) %>% clean_names() %>%
#   select(-c("source_1", "source_2", "source_3", "backup_notes", "notes", "estimated_onset_date")) %>%
#   filter(! is.na(date_announced) ) %>%
#   mutate(Date = dmy(date_announced)) %>%
#   select(Date, detected_state, detected_city, detected_district, age_bracket, current_status) %>%
#   rename(
#     StateUt = "detected_state",
#     District = "detected_district",
#     City = "detected_city",
#     Status = "current_status", 
#     AgeBracket = "age_bracket"
#   )
# 
# 
