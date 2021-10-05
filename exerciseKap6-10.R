######## Exercise Kap 6 - 10 ####################

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
               ggvis,httr, lubridate, plotly, rio, rmarkdown, shiny,
               stringr, tidyr, caret, lars, tidyverse, psych, dygraphs,
               vioplot, gapminder, nycflights13, gapminder, Lahman, ISLR2 )



### 6.1
library(dplyr)
library(nycflights13)
library(tidyverse)

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

### 7.3.1

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>% 
  count(cut)
## histoogram
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# count

diamonds %>% 
  count(cut_width(carat, 0.5))
diamonds %>% 
  dplyr::count(ggplot2::cut_width(carat, 0.5))

### mindre binwith histogram geom_histogram

smaller <- diamonds %>% 
  filter(carat < 3)
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

### med linjer i stedet for geom_freqpoly
ggplot(data = smaller,mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

### 7.3.2
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

### eruptions
ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_histogram(binwidth = 0.25)

### 7.3.3 Unusual values

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y),binwidth = 0.5)

### at zoome med coord_cartesian

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0,50))

### vi filtrere de usædvanlige værdier væk
unusal <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>% 
  arrange(y)
unusal


### 7.3.4 Exercises

###

#### 7.4 Missing values
### brug ikke
diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))
diamonds2

### men brug mutate() i stedet for
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))
diamonds2
#> Warning: Removed 9 rows containing missing values (geom_point).

diamonds2 <- diamonds
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point()

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)


### using is.na()

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour +sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(mapping = aes(colour = cancelled),binwidth = 1/4)

### 7.4.1 Exercises

missing <- flights %>% 
  filter(!is.na(missing))
missing
sum(is.na(missing))
mean(missing$dep_delay)
missing2 <- flights
missing2



##### 7.5 Covariation

###7.5 Covariation

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))

### density
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

### boxplot

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

### mpg dataset

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()


### 7.5.1.1 Exercises

###


### 7.5.2 Two categorical variables 
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>% 
  count(color, cut)

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

### 7.5.2.1 Exercises ####

####
#### 7.5.3 Two continuous variables
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

### med gennemsigtighed alpha()
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price), alpha = 1/100)


### geom_bin2d() and geom_hex()

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

###
ggplot(data = smaller,mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

### cut_number()

ggplot(data = smaller,mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

### 7.5.3.1 Exercises
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

#####

####### 7.6 Patterns and models
ggplot(data = faithful) +
  geom_point(mapping = aes(x = eruptions, y = waiting))

library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))


ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))

### 7.7 ggplot2 calls

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

ggplot(faithful, aes(eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()

#### 10.2 Creating tibbles

as_tibble(iris)

#> # A tibble: 5 x 3
tibble(
  x = 1:5,
  y = 1,
  z = x ^ 2 + y
)
#> # A tibble: 1 x 3
tb <- tibble(
    `:)` = "smile",
    ` ` = "space",
    `2000` = "number"
)
tb

#> # A tibble: 2 x 3

tibble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)
tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)
### 10.3.1 Printing
tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

nycflights13::flights %>% 
  print(n = 10, width = Inf)


### 10.3.2 Subsetting

df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
# Extract by name
df$x
# Extract by position
df[[1]]

df %>% .$x
df %>% .[["x"]]

### 10.4 Interacting with older code
class(as.data.frame(tb))
#> [1] "data.frame"
df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

## 10.5 Exercises

####
