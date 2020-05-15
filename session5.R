
library(naniar)
library(tidyverse)
library(janitor)
library(lubridate)
library(viridisLite)
library(stringi)

## People 
names <- c('Sattu', 'Vivek', 'Rishi', 'Bhaskar', 'Rohit', 'Ragini', 'Rahul')
ids <- c(1, 2, 3, 4, 5, 6, 7)

users <- tibble(u_id = ids, user_name = names)
books_read <- read_csv('./datasets/books/user_books_read.csv')

## Books database.
b <- read_csv('./datasets/books/book_data.csv', col_types = cols(book_isbn = col_character())) %>% 
  filter(stri_enc_isascii(book_title)) %>%
  filter(stri_enc_isascii(book_desc)) %>%
  mutate(b_id = row_number() )

## Books with separated Genres. 
books <- b %>% 
  select(c('b_id', 'book_authors', "book_rating", "book_title", "genres")) %>%
  separate(genres, into = paste('g', (1:10), sep = '_'), sep = "([\\|])", extra="drop") %>%
  pivot_longer(-c(b_id, book_authors, book_rating, book_title), 
               names_to = 'g', 
               values_to = 'genre',
               values_drop_na = T) %>% 
  select(-c(g))

## Top genres
top_g <- books %>% group_by(genre) %>% summarise(count = n()) %>% top_n(30, count) 
top_b <- b %>% top_n(200, book_rating_count) %>% select(b_id, book_title)

## Books read by user

user_books_read <- inner_join(users, books_read, by = 'u_id')

## Favorite genres per user

user_books_genre <- left_join(user_books_read, books, by = 'book_title')

user_genre_summary <- user_books_genre %>% 
  group_by(user_name, genre) %>%
  summarise(count = n())

p <- ggplot(data = user_genre_summary, aes(x = reorder(genre, count), y = count)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~user_name) +
  coord_flip() +
  theme_minimal()

print(p)






