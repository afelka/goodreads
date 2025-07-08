library(ggimage)
library(dplyr)
library(rvest)
library(stringr)
library(RSelenium)
library(purrr)
library(lubridate)
library(DT)
library(ggplot2)
library(renv)

# Selenium Scraping is moved to python : goodreads_selenium_python.py

goodreads_list <- read.csv("goodreads_list_erdem.csv")

# create intervals for pages

goodreads_list <- goodreads_list %>% 
                  mutate(page_interval = case_when(no_of_pages < 100 ~ "0-99",
                                                   no_of_pages < 200 ~ "100-199",
                                                   no_of_pages < 300 ~ "200-299",
                                                   no_of_pages < 400 ~ "300-399",
                                                   no_of_pages < 500 ~ "400-499",
                                                   no_of_pages < 600 ~ "500-599",
                                                   no_of_pages < 700 ~ "600-699",
                                                   no_of_pages < 800 ~ "700-799",
                                                   no_of_pages < 900 ~ "800-899",
                                                   no_of_pages < 1000 ~ "900-999",
                                                   no_of_pages >= 1000 ~ "1000+")) %>% 
                  mutate(page_interval = factor(page_interval, levels = c("0-99",
                                                                         "100-199",
                                                                         "200-299",
                                                                         "300-399",
                                                                         "400-499",
                                                                         "500-599",
                                                                         "600-699",
                                                                         "700-799",
                                                                         "800-899",
                                                                         "900-999",
                                                                         "1000+")))

# most read authors #

most_read <- goodreads_list %>% group_by(name) %>%
                summarise(
                  no_of_books = n(),
                  my_avg_rating = round(mean(my_rating), 2),
                  avg_rating = round(mean(avg_rating), 2)
                ) %>%
                filter(no_of_books >= 10) %>% droplevels() %>% arrange(desc(no_of_books)) %>%
                rename(author_name = name)

datatable(most_read, options = list(pageLength = 15))


# 5-star, shortest 20 book 

shortest_five_stars <-  goodreads_list %>%
                        filter(my_rating == 5) %>% arrange(no_of_pages) %>%
                        head(20) %>% select(book_names, name, no_of_pages, avg_rating, my_rating) %>%
                        rename(book_name = book_names, author_name = name)

datatable(shortest_five_stars, options = list(pageLength = 20))

# 5-star, longest 20 book 

longest_five_stars <-  goodreads_list %>%
  filter(my_rating == 5) %>% arrange(desc(no_of_pages)) %>%
  head(20) %>% select(book_names, name, no_of_pages, avg_rating, my_rating) %>%
  rename(book_name = book_names, author_name = name)

datatable(longest_five_stars, options = list(pageLength = 20))

# 1-star books

one_star <- goodreads_list %>% filter(my_rating == 1) %>%
            arrange(avg_rating) %>%
            select(book_names, name, no_of_pages, avg_rating, my_rating) %>%
            rename(book_name = book_names, author_name = name)

datatable(one_star, options = list(pageLength = 20))

# long 2-star books

two_star <- goodreads_list %>% filter(my_rating == 2 & no_of_pages >= 500) %>%
  arrange(avg_rating) %>%
  select(book_names, name, no_of_pages, avg_rating, my_rating) %>%
  rename(book_name = book_names, author_name = name)

datatable(two_star, options = list(pageLength = 20))

# bar chart

goodreads_list_grouped <- goodreads_list %>% filter(!is.na(page_interval)) %>%
  group_by(page_interval) %>% summarise(no_of_books = n(), 
                                        my_average = round(mean(my_rating),2))

## define coeff for aligning second axis to 1 to 5 scale 
coeff <- round(max(goodreads_list_grouped$no_of_books) / 5,  -1)

## page interval and my_average rating on the y axes and page_interval on x
ggplot(goodreads_list_grouped, aes(x = page_interval, no_of_books)) + 
  geom_bar( stat = "identity", fill = "#143c8a") +
  geom_point(aes(y = my_average * coeff), color = "orange") +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, round(
      max(goodreads_list_grouped$no_of_books),
      digits = (1 - nchar(
        max(goodreads_list_grouped$no_of_books)
      ))
    )) ,
    # Features of the first axis
    name = "No of Books",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~. / coeff, name="My Avg Rating")
  ) + 
  theme_classic() +
  geom_text(aes(label = paste0(no_of_books, " (", my_average, ")")),
            vjust = -0.5,
            colour = "#143c8a",
            size = 3.5) +
  # geom_text(aes(label = my_average),
  #           colour = "orange") +
  labs(title = "Number of books by page interval (and my average rating)") +
  theme(
   axis.title.x = element_blank(),
   axis.text.x = element_text(angle = 45, vjust = 0.5)
  ) 

## using ggimage

goodreads_list_without_missing_page_info <-  goodreads_list %>%
  filter(!is.na(page_interval)) %>%
  group_by(page_interval) %>% 
  mutate(no_of_books = n(),
            my_average = round(mean(my_rating), 2))


ggplot(goodreads_list_without_missing_page_info, aes(x = page_interval, 1)) + 
  geom_image(aes(image=image_name), size=.05, position = "stack") +
  geom_point(aes(x= page_interval , y = no_of_books), color = "white") +
  geom_text(aes(y = no_of_books,label = paste0(no_of_books, " (", my_average, ")")),
            vjust = -4,
            colour = "#143c8a",
            size = 3.5) +
  theme_classic() +
  scale_y_continuous(
      limits = c(0, round(
      max(goodreads_list_grouped$no_of_books),
      digits = (1 - nchar(
        max(goodreads_list_grouped$no_of_books)
      ))
    ))) +
  labs(title = "Number of books by page interval (and my average rating)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  ) 

## top 10 authors' book covers

top_10 <- goodreads_list %>% filter(!is.na(page_interval)) %>%
  left_join(
    goodreads_list %>% group_by(name) %>%
      summarise(no_of_books_per_author = n()) %>% arrange(desc(no_of_books_per_author)) %>%
      mutate(rank = row_number())  ,
    by = "name"
  ) %>% filter(rank <= 10) %>%  rename(author_name = name) %>% group_by(author_name) %>% 
      mutate(book_number = row_number()) 

top_10$author_name <- with(top_10, reorder(author_name, desc(rank)))

ggplot(top_10, aes(book_number, author_name)) + 
  geom_image(aes(image=image_name), size=.05, position = "identity") +
  theme_classic()+
  labs(title = "Book covers of my top 10 most read authors") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(face="bold", size = 10)
  ) +
  geom_text(aes(x = no_of_books_per_author,label = no_of_books_per_author),
            hjust = -1.8,
            colour = "black",
            size = 6)
         
