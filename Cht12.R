pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder", 
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr", 
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR2", "MASS", "testthat", "leaps", "caret")

#  --------------------------------------
## CHT 12 based on tidyr package part of the core tidyverse.

table1

table2	 
table3	 
table4a	 
table4b	 

# Data sets are tidy if: each variable has it's own column and each observation
# has it's own row, and finally, each value has it's own cell.

table1 %>% 
  mutate(rate = cases / population * 10000)

table1 %>% 
  count(year)

table1 %>% 
  count(year, wt = cases)


ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))


# pivot_wider (spreading) and pivot_longer (gathering) - the two most important 
# functions in tidyr: pivot_longer() and pivot_wider()


# pivot_longer: E.g. a dataset where some of the column names are not names of
# variables, but values of a variable.

table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

# gather er en gammel version af pivot_longer 
table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")


table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")

# gather er en gammel version af pivot_longer 
table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")


# ny udgave
tidy4a <- table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
tidy4b <- table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")

left_join(tidy4a, tidy4b, by = c("country", "year"))


#pivot_wider (spreading) - used when an observation is scattered across multiple rows.

table2

# ny udgave
table2 %>% 
  pivot_wider(names_from = type, values_from = count)

# gammel udgave
spread(table2, key = type, value = count)


# Separating
# separate() pulls apart one column into multiple columns, by splitting 
# wherever a separator character appears.

table3

table3 %>% 
  separate(rate, into = c("cases", "population"))

table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")

table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/", convert = TRUE)


table3 %>% 
  separate(year, into = c("century", "year"), sep = 2) # position at split.



# Unite - combining multiple columns into a single one

table5
table5 %>% 
  unite(new, century, year)

table5 %>% 
  unite(new, century, year, sep = "")

table5 %>% 
  unite(new, century, year, sep = "") %>% 
   mutate(new = as.double(new))



#Missing values - NA and 2016 first quarter misisng

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks

# En eksplicit missing og en implicit missing
# Eksplicit: 2015 4. kvartal
# Implicit:2016 1. kvartal


stocks %>% 
  pivot_wider(names_from = year, values_from = return)

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(
    cols = c(`2015`,`2016`), names_to = "year", 
    values_to = "return"
  ) %>% 
  arrange(year, qtr)

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(
    cols = c(`2015`,`2016`), names_to = "year", 
    values_to = "return", values_drop_na = TRUE,
  ) %>% 
  arrange(year, qtr)


#complete takes a set of columns and finds all unique combinations.
stocks %>% 
  complete(year, qtr) 


treatment <- tribble(
  ~ person,           ~ treatment, ~response, 
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

treatment %>% 
  fill(person) #last observation carried forward





#  --------------------------------------
#Case Study

who
# View(who1)

who1 <- who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key",
    values_to = "cases",
    values_drop_na = TRUE
  )
who1

xxx <- who1 %>% 
  count(key) 


#stringr=package, str_replace is the function, key is the 
# variable and replace newrel with new_rel

who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2

who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")
who3

who3 %>% 
  count(new)

who4 <- who3 %>% 
  dplyr::select(-new, -iso2, -iso3)

who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1)
who5


who_finale <- who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = "key",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "type", "sexage"), sep = "_") %>% 
  dplyr::select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

#  --------------------------------------

#Exercises to do
# 12.2.1.2
# 12.3.3.1
# 12.3.3.2
# 12.3.3.3
# 12.3.3.4
# 12.4.3.1
# 12.4.3.2
# 12.5.1.1
# 12.5.1.2

