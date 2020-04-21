
##  Read the data from John Hokins University repository
##  Note that the past function just concatinates string with a delimiter specified by 'sep'
jhu_url_confirmed <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                             "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                             "time_series_covid19_confirmed_global.csv", sep = "")
d_wide_jhu <- read_csv(jhu_url_confirmed)

## 1. This data has one column for every date. 
##    Change the dataframe such that the dates are pivoted into a column 
##    i.e. The instead of having one column per date, have two columnes as follows
##    -> 'Date' column for all the dates
##    -> 'Cumulative' column for the numbers corrosponding to those dates. 

## Hint
# d_long <- d_wide_jhu %>% 
#   pivot_longer(
#     c( ___ ), 
#     names_to = ___ , 
#     values_to = ___
#     )

