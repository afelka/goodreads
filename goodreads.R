library(ggimage)
library(dplyr)
library(rvest)
library(stringr)
library(RSelenium)
library(purrr)
library(lubridate)
library(DT)
library(ggplot2)
library(ggrepel)
library(forcats)
library(ggtext)
library(scales)

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

ggsave("books_by_page_interval.png", width = 12, height = 6, dpi = 300)

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
            hjust = -1,
            colour = "black",
            size = 6)

ggsave("top_10_author_books.png", width = 14, height = 6, dpi = 300)
         
# Filter for the last 800 books as I have been registering them to Goodreads chronologically

last_800_books <- goodreads_list %>%
  filter(!is.na(date_added)) %>%
  mutate(date_added = mdy(date_added)) %>%
  arrange(desc(date_added)) %>% head(800)

# Split into groups of 50 books (chronologically)
last_800_books <- last_800_books %>% arrange(date_added) %>% 
  mutate(book_group = (row_number() - 1) %/% 50 + 1)

avg_per_50 <- last_800_books %>%
  group_by(book_group) %>%
  summarise(
    start_date = min(date_added, na.rm = TRUE),
    end_date = max(date_added, na.rm = TRUE),
    my_avg_rating = round(mean(my_rating, na.rm = TRUE), 2),
    n_books = n()
  )

# Plot my average rating over 50 books using geom_text_repel

ggplot(avg_per_50, aes(x = book_group, y = my_avg_rating)) +
  geom_line(color = "#143c8a", linewidth = 1.2) +
  geom_point(size = 3, color = "orange") +
  geom_text_repel(
    aes(label = my_avg_rating),
    size = 4,
    max.overlaps = Inf,
    nudge_y = 0.05,
    nudge_x = ifelse(avg_per_50$book_group == 10, 0.2, 0), # nudge group 10 to the right
    direction = "y",
    segment.size = 0.2,
    box.padding = 0.3,
    min.segment.length = 0
  ) +
  scale_x_continuous(
    breaks = avg_per_50$book_group,
    labels = paste0(
      "Group ", avg_per_50$book_group, "\n(",
      format(avg_per_50$start_date, "%Y-%m"), " - ",
      format(avg_per_50$end_date, "%Y-%m"), ")"
    )
  ) +
  labs(
    title = "My Average Rating per 50 Books (Last 800 Books)",
    x = "Book Groups (Chronological, 50 books each)",
    y = "My Average Rating"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 9)
  )

ggsave("my_average_rating_per_50_books.png", width = 12, height = 6, dpi = 300)

# Top 10 authors' book covers in the last 800 books

top_10_last800 <- last_800_books %>%
  filter(!is.na(page_interval)) %>%
  left_join(
    last_800_books %>%
      group_by(name) %>%
      summarise(no_of_books_per_author = n()) %>%
      arrange(desc(no_of_books_per_author)) %>%
      mutate(rank = row_number()),
    by = "name"
  ) %>%
  filter(rank <= 10) %>%
  rename(author_name = name) %>%
  group_by(author_name) %>%
  mutate(book_number = row_number())

top_10_last800$author_name <- with(top_10_last800, reorder(author_name, desc(rank)))

ggplot(top_10_last800, aes(book_number, author_name)) +
  geom_image(aes(image = image_name), size = .05, position = "identity") +
  theme_classic() +
  labs(title = "Book covers of my top 10 most read authors (Last 800 Books)") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "bold", size = 10)
  ) +
  geom_text(aes(x = no_of_books_per_author, label = no_of_books_per_author),
            hjust = -1,
            colour = "black",
            size = 6)

ggsave("top_10_author_books_last800.png", width = 14, height = 6, dpi = 300)


goodreads_list <- goodreads_list %>%
  filter(!is.na(date_added)) %>%
  mutate(date_added = mdy(date_added)) %>%
  arrange(desc(date_added)) %>%
  mutate(
    period = ifelse(row_number() <= n() / 2, "After Oct 2016", "Until Oct 2016")
  )

number_of_books_by_period <- goodreads_list %>% 
  filter(name != 'Agatha Christie') %>%
  group_by(name, period) %>% summarise(no_of_books = n()) %>% ungroup()

top_10_by_period <- number_of_books_by_period %>%
  group_by(period) %>%
  slice_max(order_by = no_of_books, n = 10) %>%
  ungroup()

top_authors_by_period <- top_10_by_period %>%
  pull(name) %>%
  unique()

top_books_by_period <- goodreads_list %>%
  filter(name %in% top_authors_by_period, !is.na(image_name)) %>%
  mutate(author_name = name) %>%
  group_by(author_name, period) %>%
  arrange(date_added) %>%
  mutate(book_number = row_number()) %>%
  ungroup()


top_books_by_period <- top_books_by_period %>%
  mutate(book_number_mirrored = ifelse(period == "Until Oct 2016", -book_number, book_number),
         period = factor(period, levels = c("Until Oct 2016", "After Oct 2016")))

author_totals <- top_books_by_period %>%
  count(author_name, name = "total_books")

top_books_by_period <- top_books_by_period %>%
  left_join(author_totals, by = "author_name") %>%
  mutate(author_name = fct_reorder(author_name, total_books))

author_period_status <- top_books_by_period %>%
  distinct(author_name, period) %>%
  group_by(author_name) %>%
  summarise(periods = paste(sort(unique(period)), collapse = ",")) %>%
  mutate(
    color = case_when(
      periods == "Until Oct 2016,After Oct 2016" ~ "blue",
      periods == "Until Oct 2016" ~ "red",
      periods == "After Oct 2016" ~ "orange",
      TRUE ~ "black"
    )
  )

author_colors <- setNames(author_period_status$color, author_period_status$author_name)

colored_labels <- paste0(
  "<span style='color:", author_colors[levels(factor(top_books_by_period$author_name))], "'>",
  levels(factor(top_books_by_period$author_name)),
  "</span>"
)

ggplot(top_books_by_period, aes(book_number_mirrored, factor(author_name, levels = levels(factor(author_name))))) +
  geom_image(aes(image = image_name), size = .02) +
  facet_wrap(~period, scales = "free_x") +
  theme_classic() +
  labs(
    title = "Book covers of top authors split by time period",
    subtitle = "Author names colored by period presence: Blue = both periods, Red = Until Oct 2016 only, Orange = After Oct 2016 only",
    x = NULL, y = NULL
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_markdown(face = "bold", size = 10),
    strip.text = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, face = "italic", color = "gray30")
  ) +
  scale_y_discrete(labels = colored_labels)

ggsave("top_10_author_books_split_by_period.png", width = 14, height = 6, dpi = 300)


year_read <- goodreads_list %>% filter(date_read != 'not set') %>%
             mutate(date_read_cleaned =  mdy(date_read, truncated = 1),
                                       year_read = year(date_read_cleaned)) %>%
             group_by(year_read) %>% summarise(total_ratings = sum(no_of_ratings),
                                               number_of_books = n()) %>%
             filter(year_read >= 2015 & year_read <= 2025) %>%
             mutate(avg_rating = total_ratings / number_of_books)

ggplot(year_read, aes(x = factor(year_read), y = total_ratings)) +
  geom_col(fill = "#69b3a2", width = 0.7) + 
  
  # 1. Format labels to show Millions (M) with a dot separator
  # scale = 1e-6 divides the number by 1,000,000
  geom_text(aes(label = label_number(suffix = "M", scale = 1e-6, 
                                     big.mark = ".", decimal.mark = ",", 
                                     accuracy = 0.1)(total_ratings)), 
            vjust = -0.5, 
            size = 4.5, 
            fontface = "bold") +
  
  # 2. Format Y-axis to match
  scale_y_continuous(
    labels = label_number(suffix = " M", scale = 1e-6, big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.15)) # Extra space at top for labels
  ) +
  
  theme_classic() +
  labs(
    title = "The Popularity Peak: Total Global Ratings of Books Read",
    subtitle = "Aggregated Goodreads ratings (In 2025 I read more popular titles)",
    x = "Year Finished",
    y = "Total Global Ratings"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 10)
  )

# Save the plot
ggsave("total_ratings_formatted.png", width = 12, height = 6, dpi = 300)

