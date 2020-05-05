
library(naniar)
library(tidyverse)
library(janitor)
library(lubridate)
library(viridisLite)

if(F) {
  # data_india <- read_csv("https://query.data.world/s/uw2ftil4lmuccp446gxqezh4ot4hrx");
  data_world <- read_csv("https://query.data.world/s/edupprs3qxblrihhg3cqlgtcintzer");
}

df <- as_tibble(data_world) %>% clean_names() %>% mutate(dt = ymd(dt)) 

df_india <- df %>% 
  filter( country == "India" ) %>%
  mutate(month_name = months(dt), month_number = month(dt), year = year(dt)) %>% 
  mutate(decade = year - year %% 10 ) %>%
  select(-c(dt, latitude, longitude)) %>%
  filter(!is.na(average_temperature)) %>%
  group_by(city, month_name) %>% 
  mutate(deviation_from_mean = average_temperature - mean(average_temperature)) %>%
  ungroup()

# p1 <- ggplot(
#     data = df_india, 
#     aes(
#       x = decade, 
#       y = deviation_from_mean, 
#       group = reorder(month_name, month_number), 
#       color = reorder(month_name, month_number),
#       fill  = reorder(month_name, month_number)
#       )) + 
#   geom_smooth(se = F) + 
#   theme_minimal() + 
#   theme(
#     legend.title = element_blank()
#   ) + 
#   ylab('Deviation from Mean') +
#   xlab('Decade')
#   
# print(p1)


p2 <- ggplot(
  data = df_india, 
  aes(
    x = decade, 
    y = deviation_from_mean, 
    group = decade, 
    color = decade,
    fill  = decade
  )) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_minimal() + 
  theme(
    legend.position = 'off'
  ) + 
  ylab('Deviation from Mean') +
  xlab('Decade') + 
  ylim(c(-3, 3))

print(p2)


## These plots were created with data_world 
## Exercise: Study data_india (uncomment line 9 to read the data)