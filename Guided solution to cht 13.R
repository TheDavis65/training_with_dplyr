pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder", 
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr", 
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR", "MASS", "testthat", "leaps", "carat",
               "RSQLite", "class", "babynames", "nasaweather",
               "fueleconomy", "viridis","ISLR2")


# 13.2.1.1
# Imagine you wanted to draw (approximately) the route each plane flies 
# from its origin to its destination. What variables would you need? 
# What tables would you need to combine?

flights
flights$origin
flights$dest

airports
airports$lat
airports$lon

names(flights)
names(airports)

flights_latlon <- flights %>%
  dplyr::select(origi, dest) %>% 
  inner_join(dplyr::select(airports, origin = faa, origin_lat = lat, origin_lon = lon),
             by = "origin"
  ) %>%
  inner_join(dplyr::select(airports, dest = faa, dest_lat = lat, dest_lon = lon),
             by = "dest"
  )

View(flights_latlon)

flights_latlon %>%
  slice(1:100) %>%
  ggplot(aes(
    x = origin_lon, xend = dest_lon,
    y = origin_lat, yend = dest_lat
  )) +
  borders("state") +
  geom_segment(arrow = arrow(length = unit(0.25, "cm"))) +
  coord_quickmap() +
  labs(y = "Latitude", x = "Longitude")

weather
# 13.2.1.2
# I forgot to draw the relationship between weather and 
# airports. What is the relationship and how should it 
# appear in the diagram?

# airports$faa er foreign key i weather$origin


# 13.2.1.3
# Weather only contains information for the origin (NYC) 
# airports. If it contained weather records for all airports
# in the USA, what additional relation would it define with
# flights

# weather : year month day hour origin --> flights : year month day hour dest    


# 13.2.1.4
# We know that some days of the year are “special”, and 
# fewer people than usual fly on them. How might you 
# represent that data as a data frame? What would be the 
# primary keys of that table? How would it connect to the 
# existing tables?


special_days <- tribble(
  ~year, ~month, ~day, ~holiday,
  2013, 01, 01, "New Years Day",
  2013, 07, 04, "Independence Day",
  2013, 11, 29, "Thanksgiving Day",
  2013, 12, 25, "Christmas Day"
)
special_days
# Primary key (year, month, day)
# Disse kolonner kan bruges til at joine special_days med
# andre tabeller


# 13.3.1.1
# Add a surrogate key to flights.

flights %>%
  arrange(year, month, day, sched_dep_time, carrier, flight) %>%
  mutate(flight_id = row_number()) %>%
  glimpse()


# 13.3.1.2
# Identify the keys in the following datasets:
# Lahman::Batting,
# babynames::babynames
# nasaweather::atmos
# fueleconomy::vehicles
# ggplot2::diamonds
# (You might need to install some packages and read some 
#   documentation.)


?Batting
Lahman::Batting %>%
  count(playerID, yearID, stint) %>%
  filter(n > 1) %>%
  nrow()
?Batting


?babynames
dim(babynames)
names(babynames)

babynames::babynames %>%
  count(year, sex, name) %>%
  filter(n > 1) %>%
  nrow()

view(babynames)

?nasaweather
?atmos
nasaweather::atmos %>%
  count(lat, long, year, month) %>%
  filter(n > 1) %>%
  nrow()

?fueleconomy
?vehicles
dim(vehicles)
View(vehicles)
fueleconomy::vehicles %>%
  count(id) %>%
  filter(n > 1) %>%
  nrow()


?diamonds
#Ingen 

names(ggplot2::diamonds) 

ggplot2::diamonds %>%
  distinct() %>%
  nrow()

nrow(ggplot2::diamonds)
# Surrogate:
diamonds <- mutate(ggplot2::diamonds, id = row_number())

diamonds
# 13.4.6.1
# Compute the average delay by destination, then join on 
# the airports data frame so you can show the spatial 
# distribution of delays.


avg_dest_delays <- flights %>%
   group_by(dest) %>%
  # arrival delay NA's are cancelled flights
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c("dest" = "faa"))

avg_dest_delays %>%
  ggplot(aes(lon, lat, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()


# 13.4.6.2
# Add the location of the origin and destination (i.e. 
# the lat and lon) to flights.


airport_locations <- airports %>%
  dplyr::select(faa, lat, lon)


flights %>%
  dplyr::select(year:day, hour, origin, dest) %>%
  left_join(
    airport_locations,
    by = c("origin" = "faa")
  ) %>%
  left_join(
    airport_locations,
    by = c("dest" = "faa"),
    suffix = c("_origin", "_dest")
  )


# 13.4.6.3
# Is there a relationship between the age of a plane and 
# its delays?


plane_cohorts <- inner_join(flights,
                            dplyr::select(planes, tailnum, plane_year = year),
                            by = "tailnum"
) %>%
  mutate(age = year - plane_year) %>%
  filter(!is.na(age)) %>%
  mutate(age = if_else(age > 25, 25L, age)) %>%
  group_by(age) %>%
  summarise(
    dep_delay_mean = mean(dep_delay, na.rm = TRUE),
    arr_delay_mean = mean(arr_delay, na.rm = TRUE)
     )
view(plane_cohorts)
plane_cohorts
ggplot(plane_cohorts, aes(x = age, y = dep_delay_mean)) +
  geom_point() +
  scale_x_continuous("Age of plane (years)", breaks = seq(0, 30, by = 10)) +
  scale_y_continuous("Mean Departure Delay (minutes)")


ggplot(plane_cohorts, aes(x = age, y = arr_delay_mean)) +
  geom_point() +
  scale_x_continuous("Age of Plane (years)", breaks = seq(0, 30, by = 10)) +
  scale_y_continuous("Mean Arrival Delay (minutes)")


# 13.4.6.4
# What weather conditions make it more likely to see a delay?
?weather
flight_weather <-
  flights %>%
  inner_join(weather, by = c(
    "origin" = "origin",
    "year" = "year",
    "month" = "month",
    "day" = "day",
    "hour" = "hour"
  ))

flight_weather
# Næsten enhver mængde nedbør er forbundet med en 
# forsinkelse. Der er dog ikke en stærk tendens...

flight_weather %>%
  group_by(precip) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = precip, y = delay)) +
  geom_line() + geom_point()



# Der ser ud til at være en stærkere sammenhæng mellem 
# sigtbarhed og forsinkelse. Forsinkelser er højere, når 
# sigtbarheden er mindre end 3 km.



table(flight_weather$visib)

flight_weather %>%
  dplyr::select(visib, dep_delay) %>% 
  ungroup() %>%
  mutate(visib_cat = cut_interval(visib, n = 10)) %>%
  group_by(visib_cat) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = visib_cat, y = dep_delay)) +
  geom_point()

view(flight_weather)
# 13.4.6.5
# What happened on June 13 2013? Display the spatial pattern 
# of delays, and then use Google to cross-reference with 
# the weather.


#Der var en stor række storme i det sydøstlige USA

flights %>%
  filter(year == 2013, month == 6, day == 13) %>%
  group_by(dest) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  ggplot(aes(y = lat, x = lon, size = delay, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap() +
  scale_colour_viridis()

?scale_colour_viridis

?planes

# 13.5.2
# Filter flights to only show flights with planes that 
# have flown at least 100 flights.

planes_gte100 <- flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  count() %>%
  filter(n >= 100)


flights %>%
  semi_join(planes_gte100, by = "tailnum")


# 13.5.3

# Combine fueleconomy::vehicles and fueleconomy::common 
# to find only the records for the most common models.

fueleconomy::vehicles %>%
  semi_join(fueleconomy::common, by = c("make", "model"))

#bemærk at model-variablen kan ikke alene udgøre by-statement
fueleconomy::vehicles %>%
  distinct(model, make) %>%
  group_by(model) %>%
  filter(n() > 1) %>%
  arrange(model)

fueleconomy::common %>%
  distinct(model, make) %>%
  group_by(model) %>%
  filter(n() > 1) %>%
  arrange(model)




# 13.5.5

anti_join(flights, airports, by = c("dest" = "faa")) %>% 
  # dplyr::select(dest, everything()) %>% 
  distinct(dest)

anti_join(airports, flights, by = c("faa" = "dest"))


flights %>% 
  distinct(dest)

airports %>% 
  distinct(faa)




