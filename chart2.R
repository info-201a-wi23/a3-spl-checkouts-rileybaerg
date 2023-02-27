# Load necessary packages
library(dplyr)
library(ggplot2)
library(stringr)

# Read in data

# choose titles to analyze from data set.
popular_titles <- c("The Very Hungry Caterpillar", "The Cat in the Hat")

# select the month, title, and number of checkouts for the chosen titles.
popular_checkouts <- checkouts %>%
  filter(str_detect(Title, regex("The Very Hungry Caterpillar", ignore_case = TRUE)) | str_detect(Title, regex("The Cat in the Hat", ignore_case = TRUE))) %>%
  mutate(Title = case_when(
    str_detect(Title, regex("The Very Hungry Caterpillar", ignore_case = TRUE)) ~ "The Very Hungry Caterpillar",
    str_detect(Title, regex("The Cat in the Hat", ignore_case = TRUE)) ~ "The Cat in the Hat"
  )) %>%
  select(CheckoutMonth, Title, Checkouts)

# create line graph with dots representing the number of checkouts a month for each of the popular titles.
popular_checkouts_per_month <- popular_checkouts %>%
  group_by(CheckoutMonth, Title) %>%
  summarise(total_checkouts = sum(Checkouts), .groups = "drop") %>% 
  ggplot(aes(x = CheckoutMonth, y = total_checkouts, color = Title)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(title = "Monthly Checkouts for The Very Hungry Caterpillar and The Cat in the Hat",
       x = "Month",
       y = "Total Checkouts",
       color = "Title") +
  theme_minimal()

# chart test: run after assigning graph object to variable.
popular_checkouts_per_month