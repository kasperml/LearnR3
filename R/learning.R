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
