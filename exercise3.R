
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

## Data has the following columns
# Position on the list
# Year of release
# Album name
# Artist name
# Genre name
# Subgenre name

## Ploting the number of albums with year and genre.
d_music <- music %>% separate(Genre, c('Genre') )
p <- ggplot(data = d_music, aes(x = Year, fill = Genre)) + 
  geom_bar() +
  theme_minimal()

print(p)

## Exercise 1
# 1. Find out who rocks the most overall
# 2. Find out who rocks the most decadewise. 
# 3. Except for Rock, what genre were most popular
# 4. Is there any correlation between the variety of genres 
#    and the success of the artists. 

