pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder", 
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr", 
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR", "MASS", "testthat", "leaps", "caret",
               "RSQLite", "class", "babynames", "nasaweather",
               "fueleconomy", "viridis")


##CHT 13

# nycflights13
?flights
airlines
airports
planes
weather

#  --------------------------------------
#A primary key uniquely identifies an observation in its own table.
#A foreign key uniquely identifies an observation in another table. 
#A variable can be both a primary key and a foreign key.

#Is n greater than one?

planes %>% 
  count(tailnum) %>% 
  filter(n > 1)
#No -> Each observation is identified by a key variable.

#Is n greater than one?
weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)
#????YES -> no primary keys

#Is n greater than one?
flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)
#Yes -> no primary keys

#Is n greater than one?
flights %>% 
  count(year, month, day, tailnum, sort = TRUE) %>% 
  filter(n > 1)
#Yes -> no primary keys

#If a table lacks a primary key, add one with mutate() and row_number().
#This is called a surrogate key.


flights %>%
  mutate(idnumber = row_number()) %>%
  dplyr::select(idnumber, everything()) %>%
  count(idnumber) %>%
  filter(n > 1)
#  --------------------------------------


#A primary key and the corresponding foreign key in another table form
#a relation.
#  --------------------------------------


#Mutating joins

flights2 <- flights %>%
  dplyr::select(year:day, hour, origin, dest, tailnum, carrier)
flights2

airlines

flights2 %>%
  dplyr::select(-origin, -dest) %>% #
  left_join(airlines, by = "carrier")


# "The old way to do it" which takes close reading to 
# figure out the overall intent:
flights2 %>%
  dplyr::select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])
#construct name to be equal to name in airlines and based on a match 
#between carrier in flights2 and carrier in airlines. 



x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

x
y

#  --------------------------------------
#Inner join
x %>% 
  inner_join(y, by = "key")
#  --------------------------------------


#Outer joins - left, right, and full

x %>% 
  left_join(y, by = "key")

x %>% 
  right_join(y, by = "key")

x %>% 
  full_join(y, by = "key")

#Duplicate key: normally due to an error. The result is a cartesian product.
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)

left_join(x, y, by = "key")

#To join by different variables on x and y use a named vector. For 
# example, "by = c("a" = "b")" will match x.a to y.b.


?left_join()


x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
left_join(x, y, by = "key")


#the default by=NULL uses all variables that appear in 
#both tables (natural join)

flights2 %>% 
  left_join(weather)

#Which is the same as the more explicit way:

flights2 %>% 
  left_join(weather, by = c("year", "month", "day", "hour", "origin"))


flights2 %>% 
  left_join(planes, by = "tailnum")
#Check the result. year.x and year.y. "year" appears in both data sets.
#However, "year" is not a key variable.
#year variables are disambiguated in the output with a suffix.

# View(flights2)
# View(airports)
?airports
?flights

flights2 %>% 
  left_join(airports, c("dest" = "faa"))

flights2 %>% 
  left_join(airports, c("origin" = "faa"))



##
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()
##
#  --------------------------------------


#Filtering joins

#semi_join(x, y) keeps all observations in x that have a match in y.
#anti_join(x, y) drops all observations in x that have a match in y.

top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
top_dest

flights %>% 
  filter(dest %in% top_dest$dest)

#A better way is to use semi-joins. 

flights %>% 
  semi_join(top_dest)

#implicit: by = "dest"


flights %>% 
  semi_join(top_dest) # This can be extended to multiple variables. 

#explicit: by = "dest"
flights %>% 
  semi_join(top_dest, by = c("dest")) # This can be extended to multiple variables. 


# Excluding observations
xxx <-flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)
xxx

#Set operations:
#intersect(x, y): return only observations in both x and y.
#union(x, y): return unique observations in x and y.
#setdiff(x, y): return observations in x, but not in y.

df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1
)
df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2
)

intersect(df1, df2)#finder fælles værdier
union(df1, df2)# hvad de har sammen
union_all(df1, df2)# alle værdier

setdiff(df1, df2)#forskællen  med df1 som udgangspunkt
setdiff(df2, df1)#forskællen  med df2 som udgangspunkt

