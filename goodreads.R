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

### Goodreads read shelf , change to yours if you want to try the code ###

url <- "https://www.goodreads.com/review/list/102002329?ref=nav_mybooks"

### Setup Selenium with the newest chrome version ### 
rD <- RSelenium::rsDriver(browser = "chrome",
                          chromever =
                            system2(command = "wmic",
                                    args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                    stdout = TRUE,
                                    stderr = TRUE) %>%
                            stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
                            magrittr::extract(!is.na(.)) %>%
                            stringr::str_replace_all(pattern = "\\.",
                                                     replacement = "\\\\.") %>%
                            paste0("^",  .) %>%
                            stringr::str_subset(string =
                                                  binman::list_versions(appname = "chromedriver") %>%
                                                  dplyr::last()) %>%
                            as.numeric_version() %>%
                            max() %>%
                            as.character())

remDr <- rD$client

## set page load time out ## 
remDr$setTimeout(type="page load", milliseconds = 10000)

### Connect to Goodreads using email and password , might ask for captcha ### 

goodreads_sign_in <- "https://www.goodreads.com/user/sign_in"
remDr$navigate(goodreads_sign_in)

user_email <- remDr$findElement("css", "#user_email")
user_password <- remDr$findElement("css", "#user_password")

## pop up box to enter email & password
user_email$sendKeysToElement(list(rstudioapi::askForPassword()))
user_password$sendKeysToElement(list(rstudioapi::askForPassword()))

sign_in_button <- remDr$findElement("css", ".gr-button--large") 
sign_in_button$clickElement()


## navigate to read shelf ## 
remDr$navigate(url)

### Find out the maximum page number ### 

webElems0 <- remDr$findElements("xpath",'//div[@class="inter"]//a')

no_of_pages <-  unlist(lapply(webElems0, function(x) {x$getElementAttribute("href")})) %>% 
  str_extract("(?<=page\\=)\\s*\\-*[0-9.]+") %>%
  as.numeric(.) %>%
  max(.)


## Create empty dataframe with needed columns
goodreads_list <- data.frame(book_names = character(), 
                             author_pages = character(),
                             no_of_pages = character(),
                             avg_rating = character(),
                             my_rating = character(),
                             date_added = character(),
                             image_sources= character())
                             

## read the name of the book, page of the author, no_of_pages, avg_rating
## date added , my_rating and cover image sources from each page

for (i in 1:no_of_pages) {
  
### Create the new page dynamically ###   
new_url <- url %>% str_replace("shelf=read", paste0("page=",i,"&shelf=read"))

remDr$navigate(new_url)

webElems1 <- remDr$findElements("xpath",'//td[@class="field title"]//a')

book_names <- unlist(lapply(webElems1, function(x) {x$getElementAttribute("title")}))

webElems2 <- remDr$findElements("xpath",'//td[@class="field author"]//a')

author_pages <- unlist(lapply(webElems2, function(x) {x$getElementAttribute("href")}))

webElems3 <- remDr$findElements("xpath",'//td[@class="field num_pages"]//div[@class="value"]')

no_of_pages <- unlist(lapply(webElems3, function(x) {x$getElementText()})) %>% gsub( "[^[:digit:].]", "", .) %>%  as.numeric(.)

webElems4 <- remDr$findElements("xpath",'//td[@class="field avg_rating"]//div[@class="value"]')

avg_rating <- unlist(lapply(webElems4, function(x) {x$getElementText()})) %>% gsub( "[^[:digit:].]", "", .) %>%  as.numeric(.)

webElems5 <- remDr$findElements("xpath",'//td[@class="field date_added"]//span')

date_added <- unlist(lapply(webElems5, function(x) {x$getElementAttribute("title")})) %>% as.Date(., "%B %d, %Y")

webElems6 <- remDr$findElements("xpath",'//td[@class="field rating"]//div[@class="stars"]')

my_rating <- unlist(lapply(webElems6, function(x) {x$getElementAttribute("data-rating")}))

webElems7 <- remDr$findElements("xpath",'//td[@class="field cover"]//img')

image_sources <- unlist(lapply(webElems7, function(x) {x$getElementAttribute("src")}))

temp_list <- data.frame(book_names = book_names, 
                       author_pages = author_pages,
                       no_of_pages = no_of_pages,
                       avg_rating = avg_rating,
                       my_rating = my_rating,
                       date_added = date_added,
                       image_sources= image_sources)

goodreads_list <<- rbind(goodreads_list,temp_list)

}

## Download book covers (downloads covers to your computer, used later in plots)

for (i in 1:nrow(goodreads_list)) {
  
  image_url <- goodreads_list[i,7]
  
  goodreads_list$image_name[i] <- paste0("cover_",i,".png")
  
  download.file(image_url, destfile = goodreads_list$image_name[i] ,mode = "wb")
  
}

### Unique authors 
authors <- goodreads_list %>% select(author_pages) %>% unique()
authors$name <- ""

## Get names for each author, might take long depending on how many unique authors one has ##

for (i in 1:nrow(authors)) {
  
author_url <- authors[i,1]  

remDr$navigate(author_url)

Sys.sleep(3)

webElem <- remDr$findElement("xpath",'//h1[@class="authorName"]//span[@itemprop="name"]') 

authors$name[i] <- webElem$getElementText()[[1]]

}

### Close Selenium
remDr$close()
rD$server$stop()
 
## add author names to the goodreads_list 
goodreads_list <- goodreads_list %>% left_join(authors, by = "author_pages")  

## convert my_rating to numeric
goodreads_list$my_rating <- as.numeric(goodreads_list$my_rating)

# assign my_rating as rounded average of my_rating if it is 0
goodreads_list$my_rating <- if_else(goodreads_list$my_rating == 0 ,
                                    round(mean(goodreads_list$my_rating),0), goodreads_list$my_rating)

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

goodreads_list_rating_grouped <- goodreads_list %>% filter(!is.na(page_interval)) %>%
  group_by(my_rating) %>% summarise(rating_count = n()) %>% ungroup()

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

## create img_path to be used in geom_image
goodreads_list_without_missing_page_info$img_path <- paste0("./",goodreads_list_without_missing_page_info$image_name)

ggplot(goodreads_list_without_missing_page_info, aes(x = page_interval, 1)) + 
  geom_image(aes(image=img_path), size=.05, position = "stack") +
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

top_10$img_path <- paste0("./",top_10$image_name)

top_10$author_name <- with(top_10, reorder(author_name, desc(rank)))

ggplot(top_10, aes(book_number, author_name)) + 
  geom_image(aes(image=img_path), size=.05, position = "identity") +
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
         
