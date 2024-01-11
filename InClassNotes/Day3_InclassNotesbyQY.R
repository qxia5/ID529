library(ID529data)
library(dplyr)

# Explicitly rename variables in the dataset
library(ID529data)
data <- nhanes_id529
View(nhanes_id529)
rename(data,
       sbp = mean_BP,
       pov_ratio = poverty_ratio,
       race_eth = race_ethnicity
)


rename(data, 
       sbp = mean_BP,
       pov_ratio = poverty_ratio,
       race_eth = race_ethnicity,
) %>% 
  glimpse()

#Lecture2 Cleaning Text data
# Load tidyverse
library(tidyverse)

# Create a simple text string
string <- "Regex patterns are a useful tool!"

# Match the word 'useful'
str_view_all(string, pattern = "useful")
# Match 'useful', and anything that comes after
str_view_all(string, pattern = "useful.*[^\\!]")
# Match letter 'e' when not followed by x or space a
str_view_all(string, pattern = "e(?!x|\\sa)")
# Match exclamation point or spaces
str_view_all(string, pattern = "\\!|\\s")

string <- c("cat", "dog", "penguin", "kangaroo", "lion")

stringr::str_extract(
  c("abc123", "USA-1234", "MA-02446-A"),
  pattern = '^[A-Za-z]*')

# follow example with Jarvis for Data Clean ----------------------------------------------


#Data cleaning with data messy_data
install.packages("pacman")
library(pacman)
library(janitor)
p_load(tidyverse)
p_load(scales)

# Read in the messy data from csv
messy_data <- read_csv("messy_data.csv")

# Clean column names ------------------------------------------------------

# We can use the clean_names() function from the janitor package to clean up the column names
df <- messy_data |>
  janitor::clean_names()

# there appear to be some ID numbers that appear multiple times
table(df$id_number)

df <- df |> arrange(id_number)
View(df)

df <- df |>
  mutate(across(c(self_identified_gender, highest_education_completed),
                ~ ifelse(.x == "unknown", NA_character_, .x)),
         across(c(age_at_exam, hours_of_sleep_per_night, household_income_before_taxes),
                ~ ifelse(.x <0, NA_real_, .x)))


unique_df <- df |>
  # count the number of missing columns
  mutate(missing_count = rowSums(is.na(df), na.rm = FALSE)) |>
  group_by(id_number) |>
  # group each id_number's observations by the number of missing columns
  arrange(missing_count) |>
  # take the first row for each id_number, which is the row with the least missing variables
  slice(1) |>
  # drop the missing_count variable
  select(-missing_count) |>
  ungroup()

dftest <- df |>
  # count the number of missing columns
  mutate(missing_count = rowSums(is.na(df), na.rm = FALSE)) |>
  group_by(id_number) |>
  # group each id_number's observations by the number of missing columns
  arrange(missing_count) |>
  # take the first row for each id_number, which is the row with the least missing variables
  slice(1) |>
  # drop the missing_count variable
  select(-missing_count)

dim(dftest)


# About Function ----------------------------------------------------------

# Calculate the Length of the Hypotenuse for a Right Triangle with Base of
# Length x and Height of Length y
#
# Uses the Pythagorean theorem to calculate the length of the hypotenuse of a
# right triangle with the side-lengths given.
#   
# Reference: https://en.wikipedia.org/wiki/Pythagorean_theorem
# 
# arguments: 
#   x   length of the triangle base 
#   y   height of the triangle 
# 
# returns: 
#   the length of the hypotenuse of the triangle
calculate_hypotenuse_length <- function(x,y) {
  sqrt(x^2 + y^2)
}

calculate_hypotenuse_length(3,4)

#Arguments
x <- 1
argument2 <- 'spooky value!'

my_fun_function <- function(x, argument2) {
  print(paste0("the value of x inside this function: ", x))
  print(paste0("the value of tricky_argument inside this function: ", 
               argument2))
}

my_fun_function(1234, "i'm not spooky!")
