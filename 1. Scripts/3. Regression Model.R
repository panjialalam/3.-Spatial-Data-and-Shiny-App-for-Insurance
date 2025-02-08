###############################################################
## Title: Regression Model
## Author: Panji Al 'Alam
## Email: panjialalam@outlook.com
################################################################

library(tidyverse)
library(stargazer)

##--------------------------------------------------------------
## Section 1: Logistic Regression
##--------------------------------------------------------------
file_path <- "/Users/panjialalam/Documents/GitHub/3.-Spatial-Data-and-Shiny-App-for-Insurance/2. Data files/"
acs_2022 <- read_csv(paste0(file_path, "acs_2022.csv"))

# Change observation formats
insurance <- acs_2022 |>
  mutate(SEX = if_else(SEX == 2, "Female", "Male"),
         RACE = if_else(RACE == 1, "White", "No White"),
         MARST = case_when(MARST %in% c(1, 2) ~ "Married",
                           MARST %in% c(3, 4, 5, 6) ~ "Not Married"),
         HCOVPRIV = case_when(HCOVPRIV %in% 1 ~ 0,
                              HCOVPRIV %in% 2 ~ 1),
         EDUC = case_when(EDUC %in% 0 ~ 0,
                          EDUC %in% c(1, 2, 3, 4, 5, 6) ~ 1,
                          EDUC %in% c(7, 8, 9, 10, 11) ~ 2)) |>
  select(c(SEX, AGE, MARST, RACE, HCOVPRIV, EDUC, WKSWORK1, INCWAGE))

summary(insurance)

# Run the model
glm_insurance <- glm(HCOVPRIV ~ ., family = "binomial", data = insurance)
summary(glm_insurance)

stargazer(glm_insurance, type = "text")
