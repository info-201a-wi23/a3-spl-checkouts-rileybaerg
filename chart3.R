# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read in data

# Filter data to include only Eric Carle and Dr. Seuss titles
carle_seuss_checkouts <- checkouts %>% 
  filter(Creator %in% c("Carle, Eric", "Seuss, Dr."))

# Count checkouts by year and creator
checkout_counts <- carle_seuss_checkouts %>%
  group_by(CheckoutMonth, Creator) %>%
  summarize(checkouts = n())

# Calculate percentage of checkouts by year and creator
checkout_percents <- checkout_counts %>%
  group_by(CheckoutMonth) %>%
  mutate(total_checkouts = sum(checkouts), percent = checkouts/total_checkouts)

# Create stacked bar chart
stacked_checkouts <- ggplot(checkout_percents, aes(x = CheckoutMonth, y = percent, fill = Creator)) +
  geom_col() +
  labs(title = "Percentage of Eric Carle and Dr. Seuss Checkouts by CheckoutMonth (22'-23')", x = "Month", y = "Percent of Checkouts") +
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Creator", nrow = 1))

# chart test: run after assigning graph object to variable.
stacked_checkouts
