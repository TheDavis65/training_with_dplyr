#  Installed packages ---------------------------------------------------


# install.packages("pacman")

pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder", 
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr", 
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR2", "MASS","dplyr")

# tidyverse indeholder pakkerne dplyr, ggplot2, forcats, tibble, readr, 
# tidyr, purrr


#  --------------------------------------
# CHT 4 - Workflow Baisis

1 / 200 * 30
(59 + 73 + 2) / 3
sin(pi / 2)

#You can create new objects with <-:
x <- 3 * 4
#All R statements where you create objects, assignment statements, have the 
#same form:  object_name <- value
#When reading that code say “object name gets value” in your head.

# You can inspect an object by typing its name:

x


#Type the beginning of the word, press TAB, add characters until you have 
#a unique prefix:

this_is_my_new_variable_which_is_very_long <- 1
this_is_my_new_variable_which_is_very_long

#TYPOS MATTER AND CASE MATTERS.

y <- seq(1, 10, length.out = 5)
y

(y <- seq(1, 10, length.out = 5))


#  --------------------------------------
#CHECK ALT + SHIFT + K
#  --------------------------------------

#CHT 5 DATA TRANSFORMATION - Focus on the dplyr

#Be aware of the conflict. Some functions of the BASE is overwritten by the dplyr
#In order to use the overwritten functions use their full name like
#stats::filter() and stats::lag()

?flights
nycflights13::flights
tail(flights)
head(flights)
view(flights)
#More on tibbles later on. Similar to dataframes

#  --------------------------------------
#Type of variables:

#int stands for integers.
#dbl stands for doubles, or real numbers.
#chr stands for character vectors, or strings.
#dttm stands for date-times (a date + a time).
#lgl stands for logical, vectors that contain only TRUE or FALSE.
#fctr stands for factors, which R uses to represent categorical variables with 
#fixed possible values.
#date stands for dates.
#  --------------------------------------
#dplyr for data manipulation:
#  filter() Pick observations by their values.
#  arrange() Reorder the rows.
#  select() Pick variables by their names.
#  mutate() Create new variables with functions of existing variables.
#  summarise() Collapse many variables down to a single summary.

# These can be used in conjunction with group_by() 
#  --------------------------------------
##

# You can use these operators to effectively filtering your data
# >, >=, <, <=, !=, ==

sqrt(2)^2==2
1/49*49==1

near(sqrt(2)^2, 2)

near(1/49*49, 1)

?near

##
#  --------------------------------------
##FILTER



flights
jan1 <- filter(flights, month == 1, day == 1)
jan1
view(jan1)
jan1_2 <- filter(flights, month == 1, between(day, 2, 3))
jan1_2
head(jan1_2)
tail(jan1_2)
month11_12 <- filter(flights, month == 11 | month == 12)
month11_12
nov_dec <- filter(flights, month %in% c(11, 12))
nov_dec

#De Morgan’s law: 
# !(x & y) is the same as !x | !y, and 
# !(x | y) is the same as !x & !y.

delay01 <- filter(flights, !(arr_delay > 120 | dep_delay > 120))
delay02 <- filter(flights, arr_delay <= 120, dep_delay <= 120)
delay01
delay02

delay03 <- filter(flights, !(arr_delay > 120 & dep_delay > 120))
delay04 <- filter(flights, arr_delay <= 120 | dep_delay <= 120)
delay03
delay04


#Missing: most operations involving an unknown value will also be unknown.
NA > 5
10 == NA
NA + 10
NA / 2
NA == NA

#filter() only includes rows where the condition is TRUE; it excludes 
#both FALSE and NA values. If you want to preserve missing values, ask 
#for them explicitly:

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)
df
#  --------------------------------------
##ARRANGE

flights_01 <- arrange(flights, year, month, day) 
flights_01

flights_02 <- arrange(flights, desc(year), desc(month), desc(day))
flights_02

flights_03 <- arrange(flights, desc(year, month, day))
flights_03
#Missing values are always sorted at the end of the file:

df <- tibble(x=c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))
arrange(df, desc(is.na(x)))

#  --------------------------------------
##SELECT
#select() allows you to rapidly zoom in on a useful subset using 
#operations based on the names of the variables.
flights
dplyr::select(flights, year, month, day)
dplyr::select(flights, year:day)
dplyr::select(flights, year, arr_time, everything())
dplyr::select(flights, tailnum:time_hour)
dplyr::select(flights, -(year:day))
dplyr::select(flights, c(17:1))

#Functions that can be used within select():
#starts_with("abc")
dplyr::select(flights, starts_with("d"))
#ends_with("xyz")
dplyr::select(flights, ends_with("e"))
#contains("ijk")
dplyr::select(flights, contains("arr"))
#matches("(.)\\1"): select variables that match a regular expression. This one
#matches may variable that contain repeated characters. 
dplyr::select(flights, matches("(.)\\1"))

#num_range("x", 1:3)  would select x1, x2 and x3: 
##

#rename rename variables and keeps the variables not mentioned
flights

rename(flights, tail_num = tailnum)
today()



dplyr::select(flights, day, month, day) #keeps the first variable called day
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
dplyr::select(flights, one_of(vars))
?one_of

#  --------------------------------------
##mutate()
#Add new variables 
flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
mutate(flights_sml, gain = arr_delay - dep_delay, speed = distance / air_time * 60)

#Note that you can refer to columns that you’ve just created:

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)
view(mutate)
#If you only want to keep the new variables, use transmute():
transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)
view(transmute)
#Arithmetic operators are also useful in conjunction with the aggregate 
#functions. E.g. x / sum(x) , and y - mean(y).
#Modular arithmetic: %/% (integer division) and %% (remainder), 
#where x == y * (x %/% y) + (x %% y)

transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

#Other functions: log(), log2(), log10()
?log #etc
x <- 1:10
x
log(x)
x01 <- lag(x)
x02 <- lead(x)
x03 <- lag(x, 2)
x04 <- lead(x, 5)
x01
x02
x03
x04
#  --------------------------------------
#Cumulative and rolling aggregates:
#cumsum(), cumprod(), cummin(), cummax(), cummean() etc. Check RcppRoll package for 
#more rolling aggregates.
cumsum(x)
cumprod(x)
cummin(x)
cummax(x)
cummean(x)

#  --------------------------------------
#Logical comparisons, <, <=, >, >=, !=
x >= 6
#  --------------------------------------

#Ranking
y <- c(1, 2, 2, 3, 4)
y01 <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))
min_rank(y01)
min_rank(desc(y01))
#  --------------------------------------
#Other ranking functions: row_number(), dense_rank(), percent_rank(), 
#cume_dist(), ntile()
#Check it out now or in Dec.
#  --------------------------------------
##
#Grouped summaries with summarise()
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

#  --------------------------------------
##PIPE

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay
delay <- filter(delay, count > 20, dest != "HNL")

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

#We had to give each intermediate data frame a name. 
#Instead we are going to use pipe, %>%, which is pronounced "then"


delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

# distinct(flights, dest) --> 105

ggplot(data = delays, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/4) +
  geom_smooth(se = FALSE)


flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))


#Getting rid of the missing values:
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

#reused later on:
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

#  --------------------------------------
#Counts

#Whenever you do any aggregation, it is always a good idea to include either a 
#count (n()), or a count of non-missing values (sum(!is.na(x))). 


# brug af table
t <- table(flights$dest)
t


delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )
ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)




delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

#integrating ggplot2 into dplyr flows:
delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)
#  --------------------------------------
#RStudio tip: a useful keyboard shortcut is Cmd/Ctrl + Shift + P. 
#This resends the previously sent chunk from the editor to the console. 
#  --------------------------------------
#Lahman package
class(Batting)
#  --------------------------------------
# Convert to a tibble so it prints nicely

batting <- as_tibble(Lahman::Batting)
class(batting)
?as_tibble

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)


##Useful summary functions
#Location: mean(x), median(x) 
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) #subset: average positive delay
  )

#Spread: sd(x), IQR(x), mad(x) 
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))


#Rank: min(x), quantile(x, 0.25), max(x)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time),
    quan_025 = quantile(dep_time, 0.25)
  )

#Position: first(x), nth(x, 2), last(x)
?nth(x, 2)

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    nth = nth(dep_time, 117) 
  )


not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  select(r, everything()) %>% 
  filter(r %in% range(r))


not_cancelled %>% 
  count(dest)
not_cancelled %>% 
  count(tailnum)
not_cancelled %>% #weight variable wt = distance 
  count(tailnum, wt = distance)

#Counts and proportions of logical values: sum(x > 10), mean(y == 0). 
#When used with numeric functions, TRUE is converted to 1 and FALSE to 0
#Sum is the no of TRUE and mean is the proportion of TRUE. TRUE = 1 and FALSE = 0.

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500)) #COUNTS

#Go to the beginning of the following code "not_cancelled...." and press Ctrl+Return

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60)) #PROPORTION


#Grouping by multiple variables
flights
?group_by
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights))) #JJJ

#Ungrouping
daily %>% 
  ungroup() %>%             # no longer grouped by date
  summarise(flights = n())  # all flights #Similar to #JJJ

#Grouped mutates and filters
flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 100)

#Find all groups bigger than a threshold:
popular_dests <- flights %>% 
  group_by(dest) %>% 
  mutate(n = n())  %>% 
  select(n, everything()) %>% 
  filter(n > 10000)
popular_dests

#  --------------------------------------
#Standardise to compute per group metrics:
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)


#HINT: One of the most important keyboard shortcuts: Cmd/Ctrl + Enter. 
#This executes the current R expression in the console.
#It will also move the cursor to the next statement.


this_is_a_really_long_name <- 2.5


# Exercises 
# 5.2.4. (1.-4.)
# 5.3.1. (1.-4.)
# 5.4.1. (1.-4.)
# 5.5.2. (1)

# De her klarer I selv:)
# 5.5.2. (2.-6.)
# 5.6.7. (1.-6.)
# 5.6.8. (1.-8.)

