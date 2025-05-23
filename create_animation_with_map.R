library(dplyr)
library(stringr)
library(ggplot2)
library(maps)
library(ggimage)
library(magick)
library(av)

#read the csvs created scraping by Python 
goodreads_list <- read.csv("goodreads_list.csv")

#country column to authors.csv is added manually
authors <- read.csv2("authors.csv")

#combine goodreads_list and authors and add row_number
goodreads_list <- goodreads_list %>% left_join(authors, by = "author_pages") %>%
                                    arrange(desc(date_added)) %>% 
                                     mutate(row_number = n() - row_number() +1 ) %>%
                                     arrange(row_number)

#create img_path
goodreads_list$img_path <- paste0("./",goodreads_list$image_name)

#read world data from maps package
world <- map_data("world")


#create theme for ggplot
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5,),
  plot.subtitle = element_text(hjust = 0.5,)
 )

#for each book create a ggplot map by highlighting the country the author is originated
#also put the image of book cover in the middle of atlantic ocean
#also put the logo of the aarhus book club bottom left corner
for (i in 1:nrow(goodreads_list)) {
  
  #add abc.jpg which is the aarhus book club's logo
  selected_row <- goodreads_list %>% slice(i) %>% mutate(count = 1, img_abc = "./abc.jpg")
              
  world_joined_with_one_book <- left_join(world, selected_row, by = c("region" = "country")) 
  
  books_from_countries <- ggplot(data = world_joined_with_one_book, mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) +
    geom_polygon(aes(fill = count)) +
    geom_image(data = selected_row , aes(x = -30, y = 40 , group = 0, image=img_path), size=.09, position = "identity") +
    geom_image(data = selected_row , aes(x = -130, y = -55 , group = 0, image=img_abc), size=.2, position = "identity") +
    ggtitle(paste0(selected_row$book_names," (", selected_row$no_of_pages, " pages)", " by ", selected_row$name, " (", selected_row$country, ")") ) +
    theme(legend.position = "none") +
    labs(subtitle = paste0("Book ", selected_row$row_number, "/", max(goodreads_list$row_number) )) + 
    theme(plot.title = element_text(size = 8, face = "bold"),
          plot.subtitle = element_text(size = 8, face = "bold")) +
    plain
  
  plot_name <- paste0("book_", i, ".png")
  # Save the ggplot as a PNG file
  ggsave(plot_name, plot = books_from_countries, width = 6, height = 4, dpi = 300)
  
}

# using the animation method found here : https://stackoverflow.com/a/73376411/10710995
filenames <- paste0("book_", 1:114, ".png")

av::av_encode_video(filenames, framerate = 0.75,
                    output = "aarhus_bookclub.mp4")

#map with number of books per country

no_of_books_per_country <- goodreads_list %>% group_by(country) %>% summarise(no_of_books = n())

world_joined_all_countries <- left_join(world, no_of_books_per_country, by = c("region" = "country")) %>%
                              arrange(no_of_books)

unique_colors <- c("cyan", "blue", "green", "red", "purple", "orange", "pink")  

books_per_country <- ggplot(data = world_joined_all_countries, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = as.factor(no_of_books))) +
  scale_fill_manual(name = "# of Books", values = unique_colors, breaks = unique(na.omit(world_joined_all_countries$no_of_books))) +
  ggtitle("Books Read per Country") +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  plain

ggsave("books_per_country.png", plot = books_per_country, width = 6, height = 4, dpi = 300)
