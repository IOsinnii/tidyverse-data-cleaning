getwd()
here()
# Load necessary libraries
library(tidyverse)
library(here)
library(readr)
library(DescTools)

# Read the data
coverage <- read_csv('tidy_healthcare_coverage.csv')
spending <- read_csv('tidy_spending_clean_corrected.csv')



# View the structure of the datasets
glimpse(coverage)
glimpse(spending)

# Assuming both datasets have a common key for merging, e.g., 'State' or 'Year'
# Check for common columns
common_columns <- intersect(names(coverage), names(spending))
print(common_columns)


# Assuming both datasets have a common key for merging, e.g., 'State' or 'Year'
# Merge the datasets on the common key
merged_data <- left_join(coverage, spending, by = "Location") # Replace 'common_key' with the actual column name

# View the merged data
glimpse(merged_data)

# Analyze the relationship between healthcare coverage and healthcare spending
# Plotting a scatter plot to visualize the relationship
ggplot(coverage, 
       aes(x = Total_2013, y = Total_2014, color = Location)) + # Replace with actual column names
  geom_point() +
  labs(title = "Relationship between Total Healthcare Coverage in 2013 and 2014",
       x = "2013_total",
       y = "2014_total") +
  theme_minimal()

# Calculate the correlation between healthcare coverage and spending
correlation <- cor(merged_data$healthcare_coverage_column, merged_data$healthcare_spending_column, use = "complete.obs") # Replace with actual column names
print(paste("Correlation: ", correlation))


# Convert the data to long format for easier merging
coverage_long2 <- coverage %>%
  pivot_longer(cols = -starts_with("Location"), names_to = "Type_Year", values_to = "Coverage") %>%
  separate(Type_Year, into = c("Type", "Year"), sep = "_") %>%
  pivot_wider(names_from = Type, values_from = Coverage)

spending_long <- healthcare_spending %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), names_to = "Year_Type", values_to = "Spending") %>%
  separate(Year_Type, into = c("Year", "Type"), sep = "__") %>%
  filter(Type == "Total Health Spending") %>%
  select(-Type)

# Merge the datasets
merged_data <- coverage_long %>%
  inner_join(spending_long, by = c("Location", "Year"))

# Inspect the merged data
glimpse(merged_data)


