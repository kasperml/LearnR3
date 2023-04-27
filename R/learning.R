# load packages

library(tidyverse)
library(NHANES)

# looking at data
glimpse(NHANES)

# Selecting columns

select(NHANES, Age)
select(NHANES, Age, Weight, BMI)
select(NHANES, -HeadCirc)
select(NHANES, starts_with("BP"))
select(NHANES, ends_with("Day"))
select(NHANES, contains("Age"))

# Creating smaller NHANES dataset

NHANES_small <- select(NHANES, Age, Gender, BMI, Diabetes, PhysActive, BPSysAve, BPDiaAve, Education)

# Renaming columns

nhanes_small <- rename_with(NHANES_small, snakecase::to_snake_case)

# Renaming specific columns

nhanes_small <- rename(nhanes_small, sex = gender)

# Trying out the pipe

colnames(nhanes_small)

nhanes_small %>%
  colnames()

nhanes_small %>%
  select(phys_active) %>%
  rename(physically_active = phys_active)

nhanes_small %>%
  rename(
    bp_sys = bp_sys_ave,
    bp_dia = bp_dia_ave
  )

nhanes_small %>%
  select(
    bmi,
    contains("age")
  )


nhanes_small %>%
  select(starts_with("bp_")) %>%
  rename(bp_systolic = bp_sys_ave)

# Filtering

nhanes_small %>%
  filter(phys_active == "No")

nhanes_small %>%
  filter(bmi == 25)

nhanes_small %>%
  filter(bmi >= 25)

# Combining logical operators
nhanes_small %>%
  filter(bmi >= 25 & phys_active == "No")

nhanes_small %>%
  filter(bmi >= 25 | phys_active == "No")

# Arrange data

nhanes_small %>%
  arrange(desc(age))

nhanes_small %>%
  arrange(education, age)

# Transforming data

nhanes_small %>%
  mutate(
    age = age * 12,
    ln_bmi = log(bmi)
  )

nhanes_small %>%
  mutate(old = if_else(age >= 30, "Yes", "No"))


# 7.12 --------------------------------------------------------------------

# 1. BMI between 20 and 40 with diabetes
nhanes_small %>%
  # Format should follow: variable >= number or character
  filter(bmi >= 20 & bmi <= bmi & diabetes == "Yes")

# Pipe the data into mutate function and:
nhanes_modified <- nhanes_small %>% # Specifying dataset
  mutate(
    # 2. Calculate mean arterial pressure
    mean_arterial_pressure = ((2 * bp_dia_ave) + bp_sys_ave) / 3,
    # 3. Create young_child variable using a condition
    young_child = if_else(age < 6, "Yes", "No")
  )

nhanes_modified

# Creating summary statistics

nhanes_small %>%
  summarise(max_bmi = max(bmi))

nhanes_small %>%
  summarise(max_bmi = max(bmi, na.rm = TRUE),
            min_bmi = min(bmi, na.rm = TRUE))

nhanes_small %>%
    filter(!is.na(diabetes)) %>%
    group_by(diabetes) %>%
    summarise(max_bmi = max(bmi, na.rm = TRUE),
              min_bmi = min(bmi, na.rm = TRUE),
              mean_bmi=mean(bmi,na.rm=TRUE)) %>%
    ungroup()

# Saving data

readr::write_csv(nhanes_small,
                 here::here("data/nhanes_small.csv"))

