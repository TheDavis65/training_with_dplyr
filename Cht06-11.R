#  Installed packages ---------------------------------------------------
# install.packages("pacman")

pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder", 
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr", 
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR2", "MASS")

# tidyverse indeholder pakkerne dplyr, ggplot2, forcats, tibble, readr, 
# tidyr, purrr



##
#  --------------------------------------
#CHT 7
#Variation
#A variable is categorical if it can only take one of a small set of values. 
#In R, categorical variables are usually saved as factors or character vectors.
#To examine the distribution of a categorical variable, use a bar chart:

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

ggplot(diamonds) +
  geom_bar(aes(cut))

diamonds %>% 
  count(cut)


#A variable is continuous if it can take any of an infinite set of ordered 
#values. Numbers and date-times are two examples of continuous variables. 
#To examine the distribution of a continuous variable, use a histogram:

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
#Different binwidths can reveal different patterns.

#You can compute this by hand by combining dplyr::count() and ggplot2::cut_width():

diamonds %>% 
  count(cut_width(carat, 0.5))

##
smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)


#Overlay multiple histograms in the same plot, use geom_freqpoly() instead of 
#geom_histogram(). The same calculation as geom_histogram().

ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

#Smaller binwidth
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

?faithful
#Clusters
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)

#Zooming in on unusual values
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)


ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  dplyr::select(price, x, y, z) %>%
  arrange(y)
unusual



#Missing values - Outliers: Drop them or make them into missing values.
diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

#ifelse() has three arguments. The first argument test should be a logical vector. 
#The result will contain the value of the second argument, yes, when test is TRUE, 
#and the value of the third argument, no, when it is false.
#Check the dplyr::case_when()

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()

#na.rm = TRUE suppresses the warning.
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)

##
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

#It’s common to want to explore the distribution of a continuous 
#variable broken down by a categorical variable.

ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(diamonds) +
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

#BOx-plot
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()
?reorder


#Two categorical variables
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

#Another approach - compute the count with dplyr and then visualise with
#geom_tile() and fill with aesthetic:
diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

table(diamonds$color, diamonds$cut)

#Two continuous variables
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

#cut_width
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

#cut_number
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))
?cut_number

#coord_cartesian
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) 

#Zooming in on the main graph ignoring some outlier observations
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

#Two clusters:
ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))

mod <- lm(log(price) ~ log(carat), data = diamonds)
summary(mod)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

?add_residuals
diamonds2


ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))


ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = price))

ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))


ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

ggplot(faithful, aes(eruptions)) + 
  geom_freqpoly(binwidth = 0.25)


diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile() 

#  --------------------------------------


######
#CHT 10 Tibbles

#Tibbles are data frames, but they tweak some older behaviours to make life a 
#little easier. Use the tibble package which is part of the core tidyverse.



iris # Data 
class(iris) #Type: dataframe
xxx <- as_tibble(iris) #Type: tibble. Coerce a data frame to a tibble.
vignette("tibble")

#Creating tibbles
tb01 <- tibble(
  x = 1:5, 
  y = 1, 
  z = x ^ 2 + y
)
tb01


tb02 <- tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)
tb02

ls()    

#Another way to create a tibble is with tribble(), short for transposed tibble.

tribble(
  ~x, ~y, ~z,
  "a", 2, 3.6,
  "b", 1, 8.5
)


tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

?lubridate

nycflights13::flights %>% 
  print(n = 10)

nycflights13::flights %>% 
  print(n = 10, width = Inf)

nycflights13::flights %>% 
  View()

#Subsetting
df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
df

# Extract by name
df$x
df[["x"]]

# Extract by position
df[[1]]

#To use these in a pipe, you’ll need to use the special placeholder .:
df %>% .$x
df %>% .[["x"]]


#Interaction with older code: If you encounter one of these functions, 
#use as.data.frame() to turn a tibble back to a data.frame:

class(as.data.frame(df))



#CHT 11 Data import
#load flat files in R with the readr package - part of tidyverse:

#read_csv() reads comma delimited files.
#read_csv2() reads semicolon separated files. 
#read_tsv() reads tab delimited files.
#read_delim() reads in files with any delimiter.

#read_fwf() reads fixed width files. You can specify fields either by their 
#widths with fwf_widths() or their position with fwf_positions(). 
#read_table() reads a common variation of fixed width files where columns are 
#separated by white space.
#read_log() reads Apache style log files.

heights <- read_csv("heights.csv")
heights

heights <- dplyr::select(heights, height, everything())
heights


#Read a comma separated datalines
read_csv("a,b,c
1,2,3
4,5,6")

#Skip lines
read_csv("The first line of metadata 
      The second line of metadata
            x,y,z
            1,2,3", skip = 2)

#Skip comments
read_csv("@ A comment I want to skip
 x,y,z
 1,2,3", comment = "@")

#Skip comments
read_csv("# A comment I want to skip
 x,y,z
         1,2,3", comment = "#")

#No column names
read_csv("1,2,3
         4,5,6", col_names = FALSE)

#Shortcut for adding a new line
read_csv("1,2,3\n4,5,6", col_names = FALSE)

#Column names
read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))

#NA
read_csv("a,b,c\n1,2,.", na = ".")


#  --------------------------------------
#parse
#Display the structure of an object

?parse_integer()
str(parse_logical(c("TRUE", "FALSE", "NA")))
parse_logical(c("TRUE", "FALSE", "NA"))

str(parse_integer(c("1", "2", "3")))
str(parse_date(c("2010-01-01", "1979-10-14")))
parse_integer(c("1", "231", ".", "456"), na = ".")
x <- parse_integer(c("123", "345", "abc", "123.45"))
x
problems_x <- problems(x)

#parse_logical() and parse_integer() parse logicals and integers respectively. 
#parse_double() is a strict numeric parser, and parse_number() is a flexible numeric parser. 
#parse_character() 
#parse_factor() create factors.
#parse_datetime(), parse_date(), and parse_time() 

parse_double("1.23")
parse_double("1,23", locale = locale(decimal_mark = ","))
parse_number("$100")
parse_number("20%")
parse_number("Prisen er $123.45")
parse_number("$123,456,789")
parse_number("123.456.789", locale = locale(grouping_mark = "."))
parse_number("123'456'789", locale = locale(grouping_mark = "'"))

x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
x1 #Not really a problem on my computer
x2
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))

guess_encoding(charToRaw(x1))
parse_character(x1, locale = locale(encoding = "ISO-8859-1"))
parse_character(x1, locale = locale(encoding = "ISO-8859-9"))

guess_encoding(charToRaw(x2))
parse_character(x2, locale = locale(encoding = "KOI8-R"))


#R uses factors to represent categorical variables that have a 
#known set of possible values. 

fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)

#parse_datetime() expects an ISO8601 date-time. 
#ISO8601 is an international standard in which the components 
#of a date are organised from biggest to smallest: 
#year, month, day, hour, minute, second.

parse_datetime("2010-10-01T2010")
parse_datetime("20101010")
#  https://en.wikipedia.org/wiki/ISO_8601
parse_date("2010-10-01")

parse_time(c("01:10 pm", "00:34 am"))
parse_time("20:10:01")


#Year
# %Y (4 digits).
# %y (2 digits); 00-69 -> 2000-2069, 70-99 -> 1970-1999.
#Month
# %m (2 digits).
# %b (abbreviated name, like “Jan”).
# %B (full name, “January”).
#Day
# %d (2 digits).
# %e (optional leading space).
#Time
# %H 0-23 hour.
# %I 0-12, must be used with %p.
# %p AM/PM indicator.
# %M minutes.
# %S integer seconds.
# %OS real seconds.
# %Z Time zone (as name, e.g. America/Chicago).
# %z (as offset from UTC, e.g. +0800).
#Non-digits
# %. skips one non-digit character.
# %* skips any number of non-digits.


parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")

parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))


parse_date("January 1, 2010", "%B %d, %Y")
parse_date("2015-Mar-07", "%Y-%b-%d")
parse_date("06-Jun-2017", "%d-%b-%Y")
parse_date(c("August 19 (2015)", "July 1 (2015)"), "%B %d (%Y)")

ex_date_in <- c("August 19 (2015)", "July 1 (2015)")
ex_data_out <- parse_date(ex_date_in, "%B %d (%Y)")
ex_data_out

parse_date("12/30/14", "%m/%d/%y")

parse_time("1705", "%H%M")
parse_time("11:15:10.12 PM", "%I:%M:%OS %p")


guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TRUE", "FALSE"))
guess_parser(c("1", "5", "9"))
guess_parser(c("12,352,561"))
str(parse_guess("2010-12-24"))


#  --------------------------------------
#Missing and problems:
challenge <- read_csv(readr_example("challenge.csv"))
problems(challenge)
tail(challenge)
head(challenge)

challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_integer(),
    y = col_character()
  )
)

challenge
View(challenge)
tail(challenge)

challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_character()
  )
)

challenge

View(challenge)
tail(challenge)


challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)

challenge
View(challenge)
tail(challenge)


#Important - Every parse_xyz() function has a corresponding col_xyz() function.
#parse_xyz() is used when the data is in a character vector in R already.
#col_xyz() is used in order to tell readr how to load data. 
#Always supply col_types. 
#stop_for_problems(): that will throw an error and stop your script if there 
#are any parsing problems

#How many observations should the guess be based on? The default is 1000.
challenge2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)
#The first 1000 observations are NA but the 1001th is a date.
challenge2

#Sometimes it’s easier to diagnose problems if you just read in all the 
#columns as character vectors:
challenge2 <- read_csv(readr_example("challenge.csv"), 
                       col_types = cols(.default = col_character())
)
challenge2


df <- tribble(
  ~x,  ~y,
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
)
df

type_convert(df)


#Writing to a file

# write_csv() and write_tsv() Both functions increase the chances of the 
# output file being read back in correctly by: Always encoding strings in UTF-8.
# And saving dates and date-times in ISO8601 format so they are easily
# parsed elsewhere.
# write_excel_csv() #export a csv file to Excel

write_csv(challenge, "challenge.csv")

#Note that the type information is lost when you save to csv:


challenge
write_csv(challenge, "challenge-2.csv")
read_csv("challenge-2.csv")

#Problem that the type information is lost. However, there are two solutions:

write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")
#R’s custom binary format: RDS.
#Cannot be used outside R - but supports list-columns.

write_feather(challenge, "challenge.feather")
read_feather("challenge.feather")
# The feather package implements a fast binary file format that can be shared
# across programming languages.

# haven reads SPSS, Stata, and SAS files.
# readxl reads excel files (both .xls and .xlsx).
# DBI, along with a database specific backend 
# (e.g. RMySQL, RSQLite, RPostgreSQL etc) allows you to run SQL queries 
#	 against a database and return a data frame.
# For hierarchical data: use jsonlite (by Jeroen Ooms) for json, 
# and xml2 for XML. Jenny Bryan has some excellent worked examples 
# at https://jennybc.github.io/purrr-tutorial/.

#For other file types, try the R data import/export manual and the rio package.

#  --------------------------------------


# Øvelser

# 
# 7.4.1.1.
# 7.4.1.2.
# 
# 7.5.1.1.1.
# 7.5.1.1.5.
# 
# 7.5.3.1.1.
# 7.5.3.1.2.
# 
# 10.5.1.
# 10.5.2.
# 10.5.3.
# 
# 11.2.2.1.
# 11.2.2.2.
# 11.2.2.3.
# 11.2.2.4.
# 11.3.5.5.
# 
# 11.3.5.7.



# Ekstra øvelser
# 6.3.1.
# 6.3.2
# 7.3.4.1.
# 7.3.4.2.
# 7.3.4.3.
# 7.3.4.4.
# 7.5.1.1.2.
# 7.5.1.1.3.
# 7.5.1.1.4.
# 7.5.1.1.6.
# 7.5.2.1.1.
# 7.5.2.1.2.
# 7.5.2.1.3.
# 7.5.3.1.3.
# 7.5.3.1.4.
# 7.5.3.1.5.
# 10.5.4.
# 10.5.5.
# 10.5.6.
# 11.2.2.5.
# 11.3.5.1.
# 11.3.5.2.
# 11.3.5.3.
# 11.3.5.4.
# 11.3.5.6.