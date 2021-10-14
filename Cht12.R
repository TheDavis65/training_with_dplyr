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
# reducere 1999 og 2000 til year værdierne flyttes til cases

# den gamle metode neden for
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
treatment

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
xxx
view(xxx)
#stringr=package, str_replace is the function, key is the 
# variable and replace newrel with new_rel

who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2

who2-sletmig <- who5 %>% 
  mutate(key = stringr::str_replace(age, c("014"), "0-14"))
who2-sletmig



view(who2)

who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")
who3

who3 %>% 
  count(new)

table(who2$iso2)

table(who2$iso3)


age_pattrn <- c("014" = "0-14", "1524" = "15-24", "2534" = "24-34",
                "3544" = "35-44","4554" = "45-54", "5564" = "55-64", "65" = "65+")

who_sletmig <- who5 %>% 
  mutate(age = str_replace_all(age, pattern = age_pattrn))

who_sletmig

who_sletmig <- who5 %>% 
  mutate(age = str_replace_all(age, pattern = c('014' = "0-14", '1524' = "15-24")))

view(who_sletmig)


who4 <- who3 %>% 
  dplyr::select(-new, -iso2, -iso3)
who4

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
  separate(sexage, c("sex", "age"), sep = 1) %>% 
  mutate(age = str_replace_all(age, pattern = age_pattrn))

who_finale
view(who_finale)

who888 <- who5 %>% 
  mutate(age = stringr::str_replace(age, "014", "0-14"))
who888

#  --------------------------------------


#12.2.1.2
t2_cases <- filter(table2, type == "cases") %>%
  rename(cases = count) %>%
  arrange(country, year)
t2_population <- filter(table2, type == "population") %>%
  rename(population = count) %>%
  arrange(country, year)

t2_cases_per_cap <- tibble(
  year = t2_cases$year,
  country = t2_cases$country,
  cases = t2_cases$cases,
  population = t2_population$population
) %>%
  mutate(cases_per_cap = (cases / population) * 10000) %>%
  dplyr::select(country, year, cases_per_cap)

bind_rows(table2, t2_cases_per_cap) %>%
  arrange(country, year, type, count)


table4c <-
  tibble(
    country = table4a$country,
    `1999` = table4a[["1999"]] / table4b[["1999"]] * 10000,
    `2000` = table4a[["2000"]] / table4b[["2000"]] * 10000
  )
table4c
table1 %>% 
  mutate(rate = (cases / population) * 10000)
# 12.2.3.1

table2 %>%
  filter(type == "cases") %>%
  ggplot(aes(year, count)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country)) +
  scale_x_continuous(breaks = unique(table2$year)) +
  ylab("cases")



# 12.3.1
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")
glimpse(stocks)


# 12.3.3.2

table4a %>% 
  pivot_longer(c(1999, 2000), names_to = "year", values_to = "cases")

table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
table4a %>% 
  pivot_longer(c("1999", "2000"), names_to = "year", values_to = "cases")


# 12.3.3.3
people <- tribble(
  ~name, ~key, ~value,
  #-----------------|--------|------
  "Phillip Woods",  "age", 45,
  "Phillip Woods", "height", 186,
  "Phillip Woods", "age", 50,
  "Jessica Cordero", "age", 37,
  "Jessica Cordero", "height", 156
)
glimpse(people)

pivot_wider(people, names_from="name", values_from = "value")
#> Rows: 5
#> Columns: 3
#> $ name  <chr> "Phillip Woods", "Phillip Woods", "Phillip Woods", "Jessica Cor…
#> $ key   <chr> "age", "height", "age", "age", "height"
#> $ value <dbl> 45, 186, 50, 37, 156
people2 <- people %>%
  group_by(name, key) %>%
  mutate(obs = row_number())
people2

pivot_wider(people2, names_from="name", values_from = "value")

people %>%
  distinct(name, key, .keep_all = TRUE) %>%
  pivot_wider(names_from="name", values_from = "value")


# 12.3.3.4

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes", NA, 10,
  "no", 20, 12
)

preg_tidy <- preg %>%
  pivot_longer(c(male, female), names_to = "sex", values_to = "count")
preg_tidy

preg_tidy2 <- preg %>%
  pivot_longer(c(male, female), names_to = "sex", values_to = "count", values_drop_na = TRUE)
preg_tidy2


# 12.4.3.1
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"))
#> Warning: Expected 3 pieces. Additional pieces discarded in 1 rows [2].
#> # A tibble: 3 x 3
#>   one   two   three
#>   <chr> <chr> <chr>
#> 1 a     b     c    
#> 2 d     e     f    
#> 3 h     i     j

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"))
#> Warning: Expected 3 pieces. Missing pieces filled with `NA` in 1 rows [2].
#> # A tibble: 3 x 3
#>   one   two   three
#>   <chr> <chr> <chr>
#> 1 a     b     c    
#> 2 d     e     <NA> 
#> 3 f     g     i

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"))
#> Warning: Expected 3 pieces. Additional pieces discarded in 1 rows [2].
#> # A tibble: 3 x 3
#>   one   two   three
#>   <chr> <chr> <chr>
#> 1 a     b     c    
#> 2 d     e     f    
#> 3 h     i     j
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "drop")
#> # A tibble: 3 x 3
#>   one   two   three
#>   <chr> <chr> <chr>
#> 1 a     b     c    
#> 2 d     e     f    
#> 3 h     i     j
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "merge")
#> # A tibble: 3 x 3
#>   one   two   three
#>   <chr> <chr> <chr>
#> 1 a     b     c    
#> 2 d     e     f,g  
#> 3 h     i     j

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"))
#> Warning: Expected 3 pieces. Missing pieces filled with `NA` in 1 rows [2].
#> # A tibble: 3 x 3
#>   one   two   three
#>   <chr> <chr> <chr>
#> 1 a     b     c    
#> 2 d     e     <NA> 
#> 3 f     g     i

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"), fill = "right")
#> # A tibble: 3 x 3
#>   one   two   three
#>   <chr> <chr> <chr>
#> 1 a     b     c    
#> 2 d     e     <NA> 
#> 3 f     g     i
tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"), fill = "left")
#> # A tibble: 3 x 3
#>   one   two   three
#>   <chr> <chr> <chr>
#> 1 a     b     c    
#> 2 <NA>  d     e    
#> 3 f     g     i


# 12.5.1.1

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return,
              values_fill = 0)
#> # A tibble: 4 x 3
#>     qtr `2015` `2016`
#>   <dbl>  <dbl>  <dbl>
#> 1     1   1.88   0   
#> 2     2   0.59   0.92
#> 3     3   0.35   0.17
#> 4     4  NA      2.66

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return,
              values_fill = 0)
#> # A tibble: 4 x 3
#>     qtr `2015` `2016`
#>   <dbl>  <dbl>  <dbl>
#> 1     1   1.88   0   
#> 2     2   0.59   0.92
#> 3     3   0.35   0.17
#> 4     4  NA      2.66
stocks %>% 
  complete(year, qtr, fill=list(return=0))
#> # A tibble: 8 x 3
#>    year   qtr return
#>   <dbl> <dbl>  <dbl>
#> 1  2015     1   1.88
#> 2  2015     2   0.59
#> 3  2015     3   0.35
#> 4  2015     4   0   
#> 5  2016     1   0   
#> 6  2016     2   0.92
#> # … with 2 more rows

# 12.5.1.2

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks %>% 
  fill()





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

