################ Data Import #####################

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
               ggvis,httr, lubridate, plotly, rio, rmarkdown, shiny,
               stringr, tidyr, caret, lars, tidyverse, psych, dygraphs,
               vioplot, gapminder, nycflights13, gapminder, Lahman, ISLR2,
               hms, feather, haven, readxl, DBI, jsonlite, xml2 )


library(dplyr)
library(nycflights13)
library(tidyverse)
library(feather)

heights <- read_csv("data/heights.csv")
head(heights)
heights

read_csv("a,b,c
         1,2,3
         4,5,6")

read_csv("The first line of metadata
         The second line of metedata
         x,y,z
         1,2,3", skip = 2)

read_csv("# A comment t0 skip
         x,y,z
         1,2,3", comment = "#")

read_csv("1,2,3\n4,5,6", col_names = FALSE)


read_csv("1,2,3\n4,5,6", col_names = c("x","y","z"))

read_csv("a,b,c\n1,2,.", na = ".")

### Exce 11.2.2

# 1
read_delim(file, delim = "|")

# 2
# col_names
# id
# na
# n_max


# 3
# Read a fixed width file.
# fwf_widths()

# 4
read_csv("x,y\n1,'a,b'", quote = "'")

# 5
read_csv("a,b\n1,2,3\n4,5,6") # mangler col
read_csv("a,b,c\n1,2\n1,2,3,4") # for lidt data i række 1 og formeget i række 2 col og rækker stemmer ikke overens
read_csv("a,b\n\"1") # dur ikke
read_csv("a,b\n1,2\nna,b") # den virker hvis der skal stå na hvis der kun skal vises n fjernes et n  
read_csv2("a;b\n1;3") # der skal bruges read_csv2

## 11.3


str(parse_logical(c("TRUE","FALSE","NA")))

str(parse_integer(c("1","2","3")))

str(parse_date(c("2010-01-01","1979-10-14")))

parse_integer(c("1","123",".","456"), na = ".")

x <- parse_integer(c("123", "345", "abc", "123.45"))
x
problems(x)


# 11 3.1 numbers

parse_double("1.23")

readr::parse_double("1.23", locale = locale(decimal_mark =","))

parse_number("$100")

parse_number("20%")

parse_number("det koster $120.45")

# bruges i USA
parse_number("$123,456,789")

# bruges i europa
parse_number("123.456.789", locale = locale(grouping_mark = "."))

# bruges i sweitze
parse_number("123'456'789", locale = locale(grouping_mark = "'"))

### 11.3.2 strings

charToRaw("Hadley")
#> [1] 48 61 64 6c 65 79
x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"

x1
#> [1] "El Ni\xf1o was particularly bad this year"
x2
#> [1] "\x82\xb1\x82\xf1\x82\u0242\xbf\x82\xcd"

parse_character(x1, locale = locale(encoding = "Latin1"))
#> [1] "El Niño was particularly bad this year"
parse_character(x2, locale = locale(encoding = "Shift-JIS"))
#> [1] "こんにちは"

guess_encoding(charToRaw(x1))
#> # A tibble: 2 x 2
#>   encoding   confidence
#>   <chr>           <dbl>
#> 1 ISO-8859-1       0.46
#> 2 ISO-8859-9       0.23
guess_encoding(charToRaw(x2))
#> # A tibble: 1 x 2
#>   encoding confidence
#>   <chr>         <dbl>
#> 1 KOI8-R         0.42

#### 11.3.3 Factors

fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)
fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)
#> Warning: 1 parsing failure.
#> row col           expected   actual
#>   3  -- value in level set bananana
#> [1] apple  banana <NA>  
#> attr(,"problems")
#> # A tibble: 1 x 4
#>     row   col expected           actual  
#>   <int> <int> <chr>              <chr>   
#> 1     3    NA value in level set bananana
#> Levels: apple banana


### 11.3.4 Dates date-time, and times

parse_datetime("2010-10-01T2010")

parse_datetime("20101010")

parse_date("2010-10-01")

library(hms)
### fejler
parse_time("01:10 am")

parse_time("20:10:01")
library(hms)
parse_time("01:10 am")
#> 01:10:00
parse_time("20:10:01")
#> 20:10:01

parse_date("01/02/15", "%m/%d/%y")
#> [1] "2015-01-02"
parse_date("01/02/15", "%d/%m/%y")
#> [1] "2015-02-01"
parse_date("01/02/15", "%y/%m/%d")
#> [1] "2001-02-15"

parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))
#> [1] "2015-01-01"

### Excr 11.3.5

# 1
## locale = locale("fr")

# 2
parse_double("1,23", locale = locale(decimal_mark ="."))
parse_number("123'456'789", locale = locale(grouping_mark = "'"))
## der sker nogget forskelligt

d1 <- parse_date("january 1, 2010")
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"


### 11.4
guess_parser("2010-10-01")
#> [1] "date"
guess_parser("15:01")
#> [1] "time"
guess_parser(c("TRUE", "FALSE"))
#> [1] "logical"
guess_parser(c("1", "5", "9"))
#> [1] "double"
guess_parser(c("12,352,561"))
#> [1] "number"

str(parse_guess("2010-10-10"))
#>  Date[1:1], format: "2010-10-10"

### eks
challenge <- read_csv(readr_example("challenge.csv"))

 

#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   x = col_double(),
#>   y = col_logical()
#> )
#> Warning: 1000 parsing failures.
#>  row col           expected     actual                                                           file
#> 1001   y 1/0/T/F/TRUE/FALSE 2015-01-16 '/Users/runner/work/_temp/Library/readr/extdata/challenge.csv'
#> 1002   y 1/0/T/F/TRUE/FALSE 2018-05-18 '/Users/runner/work/_temp/Library/readr/extdata/challenge.csv'
#> 1003   y 1/0/T/F/TRUE/FALSE 2015-09-05 '/Users/runner/work/_temp/Library/readr/extdata/challenge.csv'
#> 1004   y 1/0/T/F/TRUE/FALSE 2012-11-28 '/Users/runner/work/_temp/Library/readr/extdata/challenge.csv'
#> 1005   y 1/0/T/F/TRUE/FALSE 2020-01-13 '/Users/runner/work/_temp/Library/readr/extdata/challenge.csv'
#> .... ... .................. .......... ..............................................................
#> See problems(...) for more details.

problems(challenge)

view(challenge)
head(challenge)
tail(challenge)

### fix challenge problem

challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_logical()
  )
)
problems(challenge)
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)
problems(challenge)
view(challenge)
head(challenge)
tail(challenge)


challenge2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)
challenge2

challenge2 <- read_csv(readr_example("challenge.csv"),
                       col_types = cols(.default = col_character()))

challenge2
view(challenge2)
head(challenge2)
tail(challenge2)

df <- tribble(
  ~x,  ~y,
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
)

df
type_convert(df)
df


### 11.5 Writing to a file
write_csv(challenge, "challenge.csv")

challenge

### en anden måde at læse og skrive filen på så den beholder variabel formatet
write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")

#The feather package implements a fast binary file format that can be shared across programming languages:
write_feather(challenge, "challenge.feather")
read_feather("challenge.feather")



# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)