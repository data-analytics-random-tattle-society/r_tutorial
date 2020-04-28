
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


squirrel_colors = tibble(Name = c('Gray', 'Cinnamon', 'Black', 'White', NA), color_code = c("#bdbdbd", "#D2691E", "#2e2d2c", "#dbd9d7", "#678aa6"))

data <- left_join(data, squirrel_colors, by = c("primary_fur_color" = "Name") ) 

identifiers <- c('unique_squirrel_id', 'hectare', 
                 'shift', 'date', 'hectare_squirrel_number', 
                 'age', 'primary_fur_color', 'highlight_fur_color',
                 'color_1', 'color_2', 'color_3', 'location', 
                 'above_ground_sighter_measurement', 'color_code')

# list of activities the squirrels were observed doing. 
activities <- c('running', 'chasing', 'climbing', 'eating', 'foraging', 'other_activities')

# list of sounds squirrels were observed making.
## kuks => general musing
## quaas => can indicate the presence of a ground predator such as a dog.
## moans => can indicate the presence of an air predator such as a hawk.
sounds <- c('kuks', 'quaas', 'moans')

# Well... tails. 
## tails flags and twitches may indicate a predator sighting. 
tail_signs <- c('tail_flags', 'tail_twitches')

# Human interractions. 
interraction <- c('approaches', 'indifferent', 'runs_from', 'other_interactions')


d_fur <- data %>% 
  # select only identifiers and activities column
  select(c(identifiers, interraction)) %>%
  select(-c(other_interactions)) %>%
  pivot_longer(-identifiers, names_to = 'Interaction', values_to = 'Value') %>%
  # remove all rows with false value
  filter(Value == T) %>% 
  group_by(primary_fur_color, Interaction, color_code) %>%
  summarize(Population = n()) %>%
  ungroup() %>%
  group_by(primary_fur_color) %>%
  mutate(Pct = round(Population*100/sum(Population))) %>%
  ungroup() %>%
  filter(!is.na(primary_fur_color))


p <- ggplot(data = d_fur, aes(x=Interaction, y = Pct, fill=color_code)) + 
  # facet_wrap(~primary_fur_color) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_identity() +
  theme_minimal()
print(p)


