# Load all necessary libraries
library(dplyr)
library(ggplot2)
library(readr)

# Load in data set containing all checkouts from the SPL from 2022-2023
checkouts <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv")

# which title from Creator Eric Carle has the least checkouts?
carle_checkouts <-  checkouts %>% 
  filter(Creator == "Carle, Eric") %>% 
  group_by(Title) %>% 
  summarise(sum_checkouts = sum(Checkouts))

carle_least_checkouts <- carle_checkouts %>%
  filter(sum_checkouts == min(sum_checkouts))

# which title from Creator Eric Carle has the most checkouts?
carle_most_checkouts <- carle_checkouts %>%
  filter(sum_checkouts == max(sum_checkouts))

# what is the average number of checkouts for Dr. Seuss and Eric Carle titles?
dr_seuss_avg_checkouts <- checkouts %>%
  filter(Creator == "Seuss, Dr.") %>%
  summarise(avg_checkouts = mean(Checkouts))

carle_avg_checkouts <- checkouts %>%
  filter(Creator == "Carle, Eric") %>%
  summarise(avg_checkouts = mean(Checkouts))

# how many observations are there for each author?
n_carle <- nrow(checkouts %>% filter(Creator == "Carle, Eric"))

n_seuss <- nrow(checkouts %>% filter(Creator == "Seuss, Dr."))

# what is the oldest title by Creator Eric Carle?
oldest_titles <- checkouts %>%
  filter(Creator %in% c("Seuss, Dr.", "Carle, Eric")) %>%
  group_by(Creator, Title) %>%
  slice_head(n = 1) %>%
  arrange(Creator, PublicationYear) %>%
  ungroup() %>%
  select(Creator, Title, PublicationYear)

oldest_carle_title <- oldest_titles %>%
  filter(Creator == "Carle, Eric") %>%
  arrange(PublicationYear) %>%
  slice_head(n = 1) %>%
  select(PublicationYear)

oldest_carle_year <- oldest_titles %>%
  filter(Creator == "Carle, Eric") %>%
  arrange(PublicationYear) %>%
  slice_head(n = 1) %>%
  select(PublicationYear)

# what is the oldest title by Creator Dr. Seuss?
newest_seuss_title <- checkouts %>%
  filter(Creator == "Seuss, Dr.") %>%
  arrange(PublicationYear) %>%
  slice_tail(n = 1) %>%
  select(Title)

newest_seuss_year <- checkouts %>%
  filter(Creator == "Seuss, Dr.") %>%
  arrange(PublicationYear) %>%
  slice_tail(n = 1) %>%
  select(PublicationYear)
