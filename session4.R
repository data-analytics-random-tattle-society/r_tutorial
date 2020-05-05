library(naniar)
library(tidyverse)
## Roling stones top 500 albums of all time
rebuild_data <- F
if (rebuild_data) {
  music <- read.csv(
    "https://query.data.world/s/nk72czqmyopg254j245xl7dss7xdjm", 
    header=TRUE, 
    stringsAsFactors=FALSE
  )
}

dm <- music %>% 
  group_by(Artist) %>% 
  mutate(num_of_features = n() ) %>% 
  ungroup() %>%
  filter(num_of_features >= 5) %>% 
  separate(Genre, c("G1", "G2", "G3") ) %>%
  separate(Subgenre, c("G4", "G5", "G6")) %>% 
  select(c(Artist, G1, G2, G3, G4, G5, G6)) %>%
  pivot_longer(-c(Artist), names_to = "G", values_to = "Genre", values_drop_na = T) %>%
  select(c(Artist, Genre)) %>%
  filter((Artist %in% c("The Beatles", "Bob Dylan"))) %>%
  group_by(Artist, Genre) %>%
  summarise(Count = n())


p <- ggplot(data = dm, aes(x = reorder(Genre, Count), fill = Artist, y = Count) ) +
  geom_bar(stat = 'identity', position = 'dodge') +
  # facet_wrap(~Artist) + 
  theme_minimal() + 
  coord_polar(theta = 'y')


print(p)




