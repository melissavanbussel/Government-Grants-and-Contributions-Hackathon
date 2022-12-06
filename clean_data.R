# Load packages
library(tidyverse)

# Load CSV
table1 <- read.csv("Table.csv")

# Rename columns
table1 <- table1 %>%
  rename(org = Organization,
         type = Transfer.Payment...Type,
         name = Transfer.Payment...Name)

# Pivot data from wide to long
table1 <- table1 %>%
  pivot_longer(-c(org, type, name),
               names_to = "year",
               names_prefix = "X",
               values_to = "value")

# Tidy up values
table1 <- table1 %>%
  mutate(indicator = substr(year, 11, nchar(year))) %>%
  mutate(indicator = ifelse(indicator == "Total.budgetary.authority.available.for.use", "Total budgetary authority for available use", "Expenditures")) %>%
  mutate(year = substr(year, 1, 4)) %>%
  relocate(value, .after = indicator)

# Save results
write.csv(table1, "Table_clean.csv")
