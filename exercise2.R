
library(tidyverse)
library(lubridate)
library(janitor)
library(naniar)

d_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv')

drop_columns <- c('long', 'lat', 'lat_long', 'zip_codes', 
                  'community_districts', 'borough_boundaries', 
                  'city_council_districts', 'police_precincts', 'color_notes')
data <- d_raw %>% select(-drop_columns) %>% 
  mutate(date = mdy(date)) %>% 
  ## combination_of_primary_and_highlight_color has may colors separated by '+'. 
  ## It will be helpful to put them is separate columns
  separate(
    combination_of_primary_and_highlight_color, 
    c('color_1', 'color_2', 'color_3') ) %>%
  ## Some of the colors are just empty strings. we will replace them with NA
  replace_with_na(list(color_1 = '', color_2 = '', color_3 = ''))

identifiers <- c('unique_squirrel_id', 'hectare', 
                 'shift', 'date', 'hectare_squirrel_number', 
                 'age', 'primary_fur_color', 'highlight_fur_color',
                 'color_1', 'color_2', 'color_3', 'location', 
                 'above_ground_sighter_measurement')

# list of activities squirrel was observed doing. 
activities <- c('running', 'chasing', 'climbing', 'eating', 'foraging', 'other_activities')

# list of sounds squirrel was observed making.
## kuks => general musing
## quaas => can indicate the presence of a ground predator such as a dog.
## moans => can indicate the presence of an air predator such as a hawk.
sounds <- c('kuks', 'quaas', 'moans')

# Well... tails. 
## tails flags and twitches may indicate a predator sighting. 
tail_signs <- c('tail_flags', 'tail_twitches')

# Human interraction. 
interraction <- c('approaches', 'indifferent', 'runs_from', 'other_interactions')


d_activity <- data %>% 
  # select only identifiers and activities column
  select(c(identifiers, activities)) %>%
  select(-c(other_activities)) %>%
  pivot_longer(-identifiers, names_to = 'Activity', values_to = 'Value') %>%
  # remove all rows with false value
  filter(Value == T)


p <- ggplot(data = d_activity, aes(x=Activity, fill=shift)) + 
  geom_bar(position = 'dodge')
print(p)
## Conclusion, Eating and foraging is best done after noon, 
## Chasing climbing and running can be done any time. 

### Exerise 2.1
# Do a similar analysis for the squirrel sounds and human interractions
### Exercise 2.2
# The data has a lot of interesting insights, 
# play with it to find correlations, for example 
# which color squirrels interract better with humans?
# what kind of squirrels spend more time on the ground




