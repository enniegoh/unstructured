# Web scraping


library(rvest)
library(tidyverse)

# Science fiction movies
movies_df_sciFi <- data.frame()

for(page_result in seq(from = 1, to = 101, by = 50)){
  movies <- paste0("https://www.imdb.com/search/title/?title_type=feature&genres=sci-fi&start=", page_result, "&ref_=adv_nxt") %>%
    read_html()
  
  movies_df_sciFi <- bind_rows(movies_df_sciFi, movies %>%
                                 html_elements(".lister-item-content") %>%
                                 map_dfr(~ tibble(
                                   title = .x %>%
                                     html_element(".lister-item-header a") %>%
                                     html_text2(),
                                   year = .x %>%
                                     html_element(".text-muted.unbold") %>%
                                     html_text2(),
                                   synopsis = .x %>%
                                     html_element(".ratings-bar+ .text-muted") %>%
                                     html_text2(),
                                   category = .x %>%
                                     html_element(".certificate") %>%
                                     html_text2(),
                                   duration = .x %>%
                                     html_element(".runtime") %>%
                                     html_text2(),
                                   rating = .x %>%
                                     html_element(".ratings-imdb-rating strong") %>%
                                     html_text2(),
                                   director = .x %>%
                                     html_element(".text-muted+ p a:nth-child(1)") %>%
                                     html_text2(),
                                   voter = .x %>%
                                     html_element(".sort-num_votes-visible span:nth-child(2)") %>%
                                     html_text2(),
                                   earning = .x %>%
                                     html_element(".ghost~ .text-muted+ span") %>%
                                     html_text2(),
                                 )))
  print(paste("Page: ", page_result))
}

head(movies_df_sciFi)


# Animated movies
movies_df_animation <- data.frame()

for(page_result in seq(from = 1, to = 101, by = 50)){
  movies <- paste0("https://www.imdb.com/search/title/?title_type=feature&genres=animation&start=", page_result, "&ref_=adv_nxt") %>%
    read_html()
  
  movies_df_animation <- bind_rows(movies_df_animation, movies %>%
                                     html_elements(".lister-item-content") %>%
                                     map_dfr(~ tibble(
                                       title = .x %>%
                                         html_element(".lister-item-header a") %>%
                                         html_text2(),
                                       year = .x %>%
                                         html_element(".text-muted.unbold") %>%
                                         html_text2(),
                                       synopsis = .x %>%
                                         html_element(".ratings-bar+ .text-muted") %>%
                                         html_text2(),
                                       category = .x %>%
                                         html_element(".certificate") %>%
                                         html_text2(),
                                       duration = .x %>%
                                         html_element(".runtime") %>%
                                         html_text2(),
                                       rating = .x %>%
                                         html_element(".ratings-imdb-rating strong") %>%
                                         html_text2(),
                                       director = .x %>%
                                         html_element(".text-muted+ p a:nth-child(1)") %>%
                                         html_text2(),
                                       voter = .x %>%
                                         html_element(".sort-num_votes-visible span:nth-child(2)") %>%
                                         html_text2(),
                                       earning = .x %>%
                                         html_element(".ghost~ .text-muted+ span") %>%
                                         html_text2(),
                                     )))
  print(paste("Page: ", page_result))
}

head(movies_df_animation)


# Data Frame
movies_df <- bind_rows(movies_df_sciFi, movies_df_animation) %>% 
  mutate(genre = ifelse(row_number() <= 150, "sci-Fi", "animation"))


movies_df$year <- gsub("[()]", "", movies_df$year)
movies_df$year <- gsub("I ", "", movies_df$year)
movies_df$year <- gsub("I", "", movies_df$year)
movies_df$year <- as.numeric(movies_df$year)

unique(movies_df$category)
movies_df$category[movies_df$category == "PG-13"] <- "P13" 
movies_df$category[movies_df$category == "PG"] <- "P13"
movies_df$category[movies_df$category == "18SG"] <- "18"
movies_df$category[movies_df$category == "18PL"] <- "18"
movies_df$category[movies_df$category == "R"] <- "18"
movies_df$category[movies_df$category == "G"] <- "U"
movies_df$category[movies_df$category == "(Banned)"] <- "Banned"

movies_df$duration <- gsub("min", "", movies_df$duration)
movies_df$duration <- gsub(" ", "", movies_df$duration)
movies_df$duration <- as.numeric(movies_df$duration)

movies_df$rating <- as.numeric(movies_df$rating)

movies_df$voter <- gsub(",", "", movies_df$voter)
movies_df$voter <- as.numeric(movies_df$voter)

movies_df$earning <- gsub("[$M]", "", movies_df$earning)
movies_df$earning <- as.numeric(movies_df$earning)

movies_df <- na.omit(movies_df)

# Export data frame
# write.csv(movies_df, "movies_df.csv", row.names = F)
# Import data frame because the data of web scraping changed over time
movies_df <- read.csv("movies_df.csv", header = T)
movies_df$category <- as.factor(movies_df$category)
movies_df$genre <- as.factor(movies_df$genre)
head(movies_df)
str(movies_df)

# Comparison
## 1. genre and year
summary(movies_df$genre)
library(ggplot2)
library(scales)
movies_df %>% group_by(year, genre) %>% 
  summarise(total_movies = n_distinct(paste(title, year))) %>% 
  ggplot(aes(x = as.factor(year), y = total_movies, fill = genre)) + 
  geom_col(position = position_dodge(preserve="single")) + 
  scale_y_continuous(breaks = pretty_breaks()) + 
  labs(title = "Number of Movie Released Over the Years", x = "Year", y = "Number of Movie")


## 2. genre and classifications
total_cat <- movies_df %>% group_by(category) %>% 
  summarise(total_movies = length(genre))
sciFi_freq_cat <- movies_df %>% filter(genre == "sci-Fi") %>% group_by(category) %>% 
  summarise(total_movies = length(genre))
animation_freq_cat <- movies_df %>% filter(genre == "animation") %>% group_by(category) %>% 
  summarise(total_movies = length(genre))
freq_cat <- bind_cols(total_cat, sciFi_freq_cat, animation_freq_cat)
names(freq_cat) <- c("All", "n", "Sci-Fi", "n", "Animation", "n")
freq_cat

movies_df %>% group_by(category, genre) %>% 
  summarise(total_movies = n_distinct(paste(title, year))) %>% 
  ggplot(aes(x = category, y = total_movies, fill = genre)) + 
  geom_col(position = "dodge") + 
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  labs(title = "Classifications of Movies", x = "Category", y = "Number of Movie")


## 3. genre and duration/runtime
sciFi_movies <- movies_df %>% filter(genre == "sci-Fi")
animation_movies <- movies_df %>% filter(genre == "animation")

ggplot() + geom_histogram(data = sciFi_movies, aes(x = duration, fill = "sci-Fi"), alpha = 0.5, bins = 20) +
  geom_histogram(data = animation_movies, aes(x = duration, fill = "animation"), alpha = 0.5, bins = 20) +
  labs(title = "Number of Movies Over Duration", x = "Duration (min)", y = "Number of Movies") + 
  scale_x_continuous(breaks = pretty_breaks(n = 10)) + 
  scale_fill_manual(values = c("sci-Fi" = "red", "animation" = "blue")) +
  scale_fill_manual(values = c("sci-Fi" = "red", "animation" = "blue")) +
  guides(fill = guide_legend(title = "Genre"), color = guide_legend(title = "Genre"))


## 4. genre and rating and number of voters over years
movies_df %>% 
  filter(genre %in% c("sci-Fi", "animation")) %>% 
  ggplot(aes(x = rating, y = as.factor(year), color = voter)) + 
  geom_point(aes(size = voter), show.legend = c("size" = F, "color" = T), alpha = 0.5) +
  scale_color_viridis_b(option = "C") +
  scale_size_continuous(range = c(1,5)) +
  labs(title = "Movie Ratings and Number of Voters Over the Years", x = "Ratings", y = "Year") +
  facet_wrap(~genre)


## 5. genre and rating and revenues over years
movies_df %>% 
  filter(genre %in% c("sci-Fi", "animation")) %>% 
  ggplot(aes(x = earning, y = as.factor(year), color = rating)) + 
  geom_point(aes(size = rating), show.legend = c("size" = F, "color" = T), alpha = 0.5) +
  scale_color_viridis_b(option = "C") +
  scale_size_continuous(range = c(1,5)) +
  labs(title = "Movie Ratings and Revenues Over the Years", x = "Earning (million dollars)", y = "Year") + 
  facet_wrap(~genre)
