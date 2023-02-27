# Load necessary packages
library(dplyr)
library(ggplot2)
library(stringr)

# Read in data

# Filter data to include only Eric Carle and Dr. Seuss titles
carle_seuss_checkouts <- checkouts %>%
  filter(Creator %in% c("Carle, Eric", "Seuss, Dr."))

# Convert PublicationYear to a single year
carle_seuss_checkouts$PublicationYear <- gsub("\\[|\\]", "", carle_seuss_checkouts$PublicationYear)
carle_seuss_checkouts$PublicationYear <- as.numeric(carle_seuss_checkouts$PublicationYear)

# Group by Creator and PublicationYear to get total checkouts per year for each creator
checkouts_by_author_year <- carle_seuss_checkouts %>%
  group_by(Creator, PublicationYear) %>%
  summarize(total_checkouts = sum(Checkouts), .groups = "drop")

# Create a line/point graph
publications_total_checkouts  <- ggplot(checkouts_by_author_year, aes(x = PublicationYear, y = total_checkouts, color = Creator)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Checkouts of Eric Carle and Dr. Seuss Books", x = "Publication Year", y = "Total Checkouts", color = "Author") +
  scale_color_manual(values = c("Carle, Eric" = "orange", "Seuss, Dr." = "blue")) +
  scale_x_continuous(breaks = seq(1930, 2030, 10))

# chart test: run after assigning graph object to variable.
publications_total_checkouts