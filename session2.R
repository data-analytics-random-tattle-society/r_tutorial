
library(tidyverse)
library(lubridate)
library(janitor)

jhu_url_confirmed <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                           "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                           "time_series_covid19_confirmed_global.csv", sep = "")
jhu_url_recovered <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                           "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                           "time_series_covid19_recovered_global.csv", sep = "")
jhu_url_deaths <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                        "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                        "time_series_covid19_deaths_global.csv", sep = "")
d1 <- read_csv(jhu_url_confirmed) %>%
  mutate(Type = 'Total')

d2 <- read_csv(jhu_url_recovered) %>%
  mutate(Type = 'Recovered')

d3 <- read_csv(jhu_url_deaths) %>%
  mutate(Type = 'Deceased') 

## 1. Bind the data by rows
d <- rbind(d1, d2, d3) %>%
  rename(StateUt = "Province/State", Country = "Country/Region") %>% 
  ## 2. Pivot it longer so the Dates and the Cumulative melt into rows
  pivot_longer( 
    -c(Type, StateUt, Country, Lat, Long), 
    names_to = "Date", 
    values_to = "Cumulative") 

data <- d %>%
  ## 3. Pivot data data wider again but with respect to the Status
  ##    This is done to calculate the number of Hospitalized patients
  pivot_wider(
    names_from = Type, 
    values_from = Cumulative, 
    values_fill = list(Cumulative = 0) ) %>%
  ## 4. Calculate Hospitalized patients
  mutate(Hospitalized = Total-Recovered-Deceased) %>% 
  mutate( Date = mdy(Date)) %>% 
  ## 5. Select only the countries which are severely affected
  filter(Hospitalized  > 10000 ) %>%
  ## 6. Select the data for yesterday
  filter( Date == Sys.Date()-1 ) %>%
  ## 7. Pivot it longer again so that the Status and Cumulative melt into columns
  pivot_longer(
    -c(StateUt, Country, Lat, Long, Date),
    names_to = 'Status',
    values_to = 'Cumulative') 

## 8. ..... And Plot !!
p <- ggplot(
    data = data, 
    aes(x = Country, y = Cumulative/1000, fill = Status)
  ) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Status) + 
  ## using minimal theme for cleaner plots
  ## Other options include theme_bw, theme_classic, theme_dark, etc.
  theme_minimal() + 
  theme(
    ## changing the angle of x axis labels so they do not overlap
    axis.text.x = element_text(angle = 30, hjust = 1)
  ) +
  ## Labeling y axis
  ylab('Cumulative (Thousands)') 

print(p)



