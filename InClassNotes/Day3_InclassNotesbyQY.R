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

library(pacman)
library(janitor)
p_load(tidyverse)
p_load(scales)

# Read in the messy data from csv
messy_data <- read_csv("data/messy_data.csv")

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

# Collapsing categories ---------------------------------------------------


# We would like to do some collapsing of these variables:
# 1. Create a combined race/ethnicity variable from race_self_identified and hispanic_ethnicity
# This new variable should combine Asian and Native Hawaiian and Pacific Islander individuals into
# the same category. 
# 2. Create a collapsed gender variable with categories cisgender female, cisgender male, and trans/non-binary.
# 3. Create a collapsed education variable with categories less than high school, high school graduate, and college graduate.

table(unique_df$race_self_identified, unique_df$hispanic_ethnicity, useNA="ifany")


unique_df <- unique_df |>
  mutate(raceth = factor(   #change to factor then you can use white as reference factor later
    case_when(
      race_self_identified == "White" &
        hispanic_ethnicity == "Non-Hispanic" ~ "Non-Hispanic White",
      race_self_identified == "Black" &
        hispanic_ethnicity == "Non-Hispanic" ~ "Non-Hispanic Black",
      hispanic_ethnicity == "Hispanic" ~ "Hispanic",
      is.na(race_self_identified) |
        is.na(hispanic_ethnicity) ~ NA_character_,
      TRUE ~ "underrepresented racial/ethnic group"
    ),
    levels = c(
      "Non-Hispanic White",
      "Non-Hispanic Black",
      "Hispanic",
      "underrepresented racial/ethnic group"
    )
  ))


unique_df <- unique_df |>
  mutate(
    # recode gender collapsed
    gender_collapsed = if_else(
      self_identified_gender %in% c(
        "transgender female",
        "transgender male",
        "non-binary or genderqueer"
      ),
      "trans/non-binary",
      self_identified_gender
    ),
    education_collapsed = case_when(
      highest_education_completed %in% c("8th Grade", "9-11th Grade") ~ "less than hs",
      highest_education_completed %in% c("High School", "Some College") ~ "hs grad",
      highest_education_completed == "College Grad" ~ "college grad"
    )
  )

# Create categorical variables from continuous variables ------------------

# We would like to create a dichotomous sleep variable where 0 = >=7 hours of sleep per night and 1= <7 hours of sleep per night
# We would also like to create a categorical income variable with categories <$35,000, $35000-$50000, $50000-$100000, and >=$100000
# We would also like to create an income quintile variable


unique_df <- unique_df |>
  mutate(insufficient_sleep = hours_of_sleep_per_night < 7)

table(unique_df$insufficient_sleep)

unique_df <- unique_df |>
  mutate(income_cat = cut(household_income_before_taxes, 
                          breaks = c(0, 25000, 50000, 75000, 100000, 1000000),
                          right = FALSE, include.lowest=TRUE, na.rm=T),
         income_quint = ntile(household_income_before_taxes, 5))


# Let's visualize how income_cat has split up the household_income_before_taxes variable
ggplot(unique_df, aes(x=income_cat, y=household_income_before_taxes)) + 
  geom_boxplot() +
  scale_y_continuous(labels=label_comma())

# Let's compare how income_quint has split up the household_income_before_taxes variable
ggplot(unique_df, aes(x=factor(income_quint), y=household_income_before_taxes)) + geom_boxplot()

# Use group_by and summarize ----------------------------------------------


# to see the minimum and maximum of the quintile categories created by ntile()
# we can use group_by and summarize
unique_df |>
  group_by(income_quint) |>
  summarise(mininc = min(household_income_before_taxes, na.rm=T),
            maxinc = max(household_income_before_taxes, na.rm=T))


# suppose that we want to calculate the proportion of subjects
# with insufficient sleep in each gender and racial/ethnic group

proportions_by_gender_raceth <- unique_df |>
  # group by gender, race/ethnicity, and insufficient sleep category
  group_by(gender_collapsed, raceth, insufficient_sleep) |>
  # count the number of subjects in each category
  # Note the use of .groups='drop' which removes the group_by after summarizing
  summarise(count_insufficient_sleep = n(), .groups='drop') |>
  # Now group by gender and race/ethnicity
  group_by(gender_collapsed, raceth) |>
  # Calculate the proportion
  mutate(proportion_insufficient_sleep = count_insufficient_sleep/sum(count_insufficient_sleep, na.rm=T)) |>
  # Remove the grouping
  ungroup()

head(proportions_by_gender_raceth)


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
