pacman::p_load("pacman", "tidyverse", "magrittr", "nycflights13", 
               "testthat","RSQLite")

if (!dir.exists("ourdata")) dir.create("ourdata")


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")


ex_001 <- data.frame(
  cpr = c("210588-9991", "200488-9993", "190388-9992", "180288-9994"), 
   name = c("Emil Pedersen", "Rasmus Jensen", "Camilla Nielsen", "Mia Hansen"),
    edu = c(11, 11, 14, 21), 
     stringsAsFactors = FALSE
                      )
ex_001

devtools::install_github("r-dbi/odbc")
dbWriteTable(connection_ourdata, "db_ex_001", ex_001, overwrite = TRUE)

dbListTables(connection_ourdata)

dbDisconnect(connection_ourdata)



connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
dbExistsTable(connection_ourdata, "db_ex_001")
dbListTables(connection_ourdata)
dbListFields(connection_ourdata, "db_ex_001")
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
dbWriteTable(connection_ourdata, "diamonds", diamonds, row.names = FALSE)
dbWriteTable(connection_ourdata, "flights", flights, row.names = FALSE)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")

dbWriteTable(connection_ourdata, "diamonds", diamonds, row.names = FALSE, overwrite = TRUE)
dbWriteTable(connection_ourdata, "airports", airports, row.names = FALSE, overwrite = TRUE)
dbWriteTable(connection_ourdata, "planes", planes, row.names = FALSE, overwrite = TRUE)
dbWriteTable(connection_ourdata, "flights", flights, row.names = FALSE, overwrite = TRUE)
dbWriteTable(connection_ourdata, "weather", weather, row.names = FALSE, overwrite = TRUE)
dbWriteTable(connection_ourdata, "airlines", airlines, row.names = FALSE, overwrite = TRUE)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
dbExistsTable(connection_ourdata, "diamonds")
dbExistsTable(connection_ourdata, "airports")
dbExistsTable(connection_ourdata, "planes")
dbExistsTable(connection_ourdata, "flights")
dbExistsTable(connection_ourdata, "weather")
dbExistsTable(connection_ourdata, "airlines")


dbListTables(connection_ourdata)

dbListFields(connection_ourdata, "diamonds")
dbListFields(connection_ourdata, "airports")
dbListFields(connection_ourdata, "planes")
dbListFields(connection_ourdata, "flights")
dbListFields(connection_ourdata, "weather")
dbListFields(connection_ourdata, "airlines")

dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db <- dbReadTable(connection_ourdata, "diamonds")
dbDisconnect(connection_ourdata)

class(diamonds)
class(diamonds_in_our_db)
head(diamonds_in_our_db, 3)
head(diamonds, 3)

identical(diamonds_in_our_db, diamonds)
compare(diamonds_in_our_db, diamonds, max_diffs = 11)


typeof(diamonds$cut)
class(diamonds$cut)
str(diamonds$cut)

typeof(diamonds_in_our_db$cut)
class(diamonds_in_our_db$cut)
str(diamonds_in_our_db$cut)


typeof(diamonds$color)
class(diamonds$color)
str(diamonds$color)

typeof(diamonds_in_our_db$color)
class(diamonds_in_our_db$color)
str(diamonds_in_our_db$color)


typeof(diamonds$clarity)
class(diamonds$clarity)
str(diamonds$clarity)

typeof(diamonds_in_our_db$clarity)
class(diamonds_in_our_db$clarity)
str(diamonds_in_our_db$clarity)



connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")

diamonds_in_our_db_001 <- dbGetQuery(connection_ourdata, 
                           "select * from diamonds"
                                     )
head(diamonds_in_our_db_001, 3)

dbDisconnect(connection_ourdata)





connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")

diamonds_in_our_db_002 <- dbGetQuery(connection_ourdata, 
                           "select carat, price from diamonds"
                                     )
head(diamonds_in_our_db_002, 3)

dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_003 <- dbGetQuery(connection_ourdata, 
                           "select distinct clarity from diamonds"
                                     )
head(diamonds_in_our_db_003, Inf)
diamonds_in_our_db_003
dbDisconnect(connection_ourdata)



connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_004 <- dbGetQuery(connection_ourdata, 
                           "select distinct clarity from diamonds"
                                     )[[1]]

diamonds_in_our_db_004
head(diamonds_in_our_db_004, 10)
dbDisconnect(connection_ourdata)



connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_005 <- dbGetQuery(connection_ourdata, 
                    "select carat as Carat, price as 'Price in dollars' from diamonds"
                                     )
head(diamonds_in_our_db_005, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_006 <- dbGetQuery(connection_ourdata, 
                           "select x*y*z as size from diamonds"
                                     )
head(diamonds_in_our_db_006, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_007 <- dbGetQuery(connection_ourdata, 
                           "select x*y*z as size from diamonds,
                            price / size as value_density from diamonds"
                                     )
head(diamonds_in_our_db_007, 3)
dbDisconnect(connection_ourdata)



connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_007 <- dbGetQuery(connection_ourdata, 
                           "select *, price / size as value_density from
                            (select carat, price, x * y * z as size from diamonds)"
                                     )
head(diamonds_in_our_db_007, 3)
dbDisconnect(connection_ourdata)




connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_008 <- dbGetQuery(connection_ourdata, 
                           "select carat, cut, price from diamonds where cut = 'Premium'"
                                     )
head(diamonds_in_our_db_008, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_009 <- dbGetQuery(connection_ourdata, 
                           "select carat, price from diamonds where price > 18800"
                                     )
head(diamonds_in_our_db_009)

dbDisconnect(connection_ourdata)



connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_010 <- dbGetQuery(connection_ourdata,
                           "select carat, cut, color from diamonds 
                            where cut = 'Good' and color = 'E'"
                                     )
head(diamonds_in_our_db_010, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_011 <- dbGetQuery(connection_ourdata,
                           "select carat, cut, color from diamonds 
                            where cut = 'Good' and (color = 'E' or color = 'I')"
                                     )
head(diamonds_in_our_db_011, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_012 <- dbGetQuery(connection_ourdata,
                           "select carat, cut, color from diamonds 
                            where cut != 'Good' and color != 'E'"
                                     )
head(diamonds_in_our_db_012)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_013 <- dbGetQuery(connection_ourdata,
                          "select carat, cut, color, price from diamonds
                            where cut in ('Ideal','Premium')"
                                     )
head(diamonds_in_our_db_013, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_014 <- dbGetQuery(connection_ourdata,
                           "select carat, cut, color, price from diamonds
                            where price between 450 and 500"
                                     )
head(diamonds_in_our_db_014, 3)
tail(diamonds_in_our_db_014, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_015 <- dbGetQuery(connection_ourdata,
                           "select carat, cut, color, price from diamonds
                            where cut like '%Good'"
                                     )
head(diamonds_in_our_db_015, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_016 <- dbGetQuery(connection_ourdata,
                           "select carat, cut, color, price from diamonds
                            where cut like '_ood'"
                                     )
head(diamonds_in_our_db_016, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_017 <- dbGetQuery(connection_ourdata,
                           "select carat, cut, color, price from diamonds
                            order by price desc"
                                     )
head(diamonds_in_our_db_017, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_018 <- dbGetQuery(connection_ourdata,
                           "select carat, cut, color, price from diamonds
                            order by price, carat desc"
                                     )
head(diamonds_in_our_db_018, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_019 <- dbGetQuery(connection_ourdata,
                           "select carat, price, x * y * z as size from diamonds
                            order by carat / size desc"
                                     )
head(diamonds_in_our_db_019, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_020 <- dbGetQuery(connection_ourdata,
                           "select carat, price, cut, clarity from diamonds
                            where cut = 'Ideal' and clarity not like '%S%' 
                             order by price"
                                     )
head(diamonds_in_our_db_020, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_021 <- dbGetQuery(connection_ourdata,
                           "select carat, price, cut, clarity from diamonds
                            where cut = 'Ideal' and clarity not like '%S%' 
                             order by price 
                              limit 3"
                                     )
head(diamonds_in_our_db_021, Inf)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_021 <- dbGetQuery(connection_ourdata,
                           "select count(*) as distinct_cut
                            from (select distinct cut from diamonds)"
                                     )
head(diamonds_in_our_db_021, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_022 <- dbGetQuery(connection_ourdata,
                           "select color, count(*) as number from diamonds
                            group by color"
                                     )
head(diamonds_in_our_db_022, Inf)


diamonds_in_our_db_023 <- dbGetQuery(connection_ourdata,
                           "select count(color) from diamonds
                            group by color"
                                     )
head(diamonds_in_our_db_023, Inf)
dbDisconnect(connection_ourdata)



connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_024 <- dbGetQuery(connection_ourdata,
                          "select clarity, avg(price) as avg_price 
                           from diamonds
                            group by clarity 
                             order by avg_price desc"
                                    )
head(diamonds_in_our_db_024, Inf)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_025 <- dbGetQuery(connection_ourdata,
                          "select clarity, 
                            min(price) as min_price, 
                             max(price) as max_price,
                              avg(price) as avg_price, 
                               sum(price) as sum_price
                                from diamonds
                                 group by clarity 
                                  order by avg_price desc"
                                    )
head(diamonds_in_our_db_025, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_026 <- dbGetQuery(connection_ourdata,
                           "select cut, color, avg(price) as avg_price
                             from diamonds
                              group by cut, color 
                               order by avg_price desc"
                                    )
head(diamonds_in_our_db_026, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
diamonds_in_our_db_027 <- dbGetQuery(connection_ourdata,
                           "select clarity, avg(price) as avg_price 
                            from diamonds
                             group by clarity 
                              having avg_price > 3500
                               order by avg_price desc"
                                    )
head(diamonds_in_our_db_027, Inf)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
flights_sql01 <- dbGetQuery(connection_ourdata,
                  "select flights.year, flights.month, flights.day, flights.hour, 
                   flights.dep_time, flights.sched_dep_time, flights.dep_delay, 
                    flights.arr_time, flights.sched_arr_time, flights.arr_delay, 
                     flights.carrier, flights.flight, flights.tailnum, flights.origin,
                      flights.dest, flights.air_time, flights.distance, flights.minute, 
                       flights.time_hour, planes.year, planes.type, planes.manufacturer, 
                        planes.model, planes.engines, planes.tailnum, planes.seats, 
                         planes.speed, planes.engine
                          from flights
                           inner join planes on flights.tailnum = planes.tailnum"
                                    )
head(flights_sql01, 3)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
flights_sql02 <- dbGetQuery(connection_ourdata,
                  "select flights.*, planes.*
                   from flights
                    inner join planes 
                     on flights.tailnum = planes.tailnum"
                                    )
head(flights_sql02, 3)
dbDisconnect(connection_ourdata)



connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
flights_sql03 <- dbGetQuery(connection_ourdata,
                  "select flights.*, planes.*
                   from flights
                    left join planes 
                     on flights.tailnum = planes.tailnum"
                                    )
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
flights_sql04 <- dbGetQuery(connection_ourdata,
                  "select s.year, s.month, s.day, s.dep_delay, t.seats, t.model
                   from flights as s
                    left join 
                     planes as t
                      on s.tailnum=t.tailnum
                       where upper(engine)='TURBO-FAN'"
                                    )
head(flights_sql04, 3)
dbDisconnect(connection_ourdata)



flights2 <- flights %>%
  dplyr::select(year:day, hour, origin, dest, tailnum, carrier)
head(flights2, 3)

flights2 <- flights %>%
  dplyr::select(year, month, day, hour, origin, dest, carrier, tailnum)
head(flights2, 3)

flights3 <- flights2 %>% 
  left_join(planes, by = "tailnum")
head(flights3, 3)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
flights_sql05 <- dbGetQuery(connection_ourdata,
                  "select f.year, f.month, f.day, f.hour, f.origin, f.dest, f.carrier, f.tailnum, 
                   p.*
                    from flights as f
                     left join planes as p 
                      on f.tailnum = p.tailnum"
                           )
head(flights_sql05, 3)
dbDisconnect(connection_ourdata)



connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
flights_sql06 <- dbGetQuery(connection_ourdata,
                   "select a.year, a.day, a.hour, a.origin,
                     a.dest, a.carrier, a.tailnum, b.*
                      from flights as a
                       full outer join planes as b
                        on a.tailnum = b.tailnum"
                           )
head(flights_sql06, 3)
dbDisconnect(connection_ourdata)




ex_foj_01 <- tibble(
  a=1:5,
  b=letters[1:5]
)
ex_foj_02 <- tibble(
  a=3:7,
  c=LETTERS[1:5]
)
ex_foj_01
ex_foj_02

connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")

dbWriteTable(connection_ourdata, "ex_foj_01", ex_foj_01, row.names = FALSE, 
             overwrite = TRUE)
dbWriteTable(connection_ourdata, "ex_foj_02", ex_foj_02, row.names = FALSE, 
             overwrite = TRUE)


### test dette set full outer join

uniondata1  <- dbGetQuery(connection_ourdata, 
                         "select a from ex_foj_01
                          union
                           select a from ex_foj_02")

uniondata2  <- dbGetQuery(connection_ourdata, 
                         "select a from ex_foj_01
                          union all
                           select a from ex_foj_02")

dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
flights_sql07 <- dbGetQuery(connection_ourdata,
                   "select x1.a, x1.b, x2.a, x2.c
                     from ex_foj_01 as x1
                      left join ex_foj_02 as x2
                       on x1.a = x2.a
                        union all 
                         select xx1.a, xx2.b, xx2.a, xx1.c
                          from ex_foj_02 as xx1
                           left join ex_foj_01 as xx2
                            on xx1.a = xx2.a
                             where xx2.a is null"
                           )
head(flights_sql07[, -3], Inf)
dbDisconnect(connection_ourdata)








connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
Canteen_data_inRformat <- readRDS(file="data20171001.Rda")
dbWriteTable(connection_ourdata, "Canteen_data_inRformat", Canteen_data_inRformat, 
             row.names = FALSE, overwrite = TRUE
             )

Extra001 <- Canteen_data_inRformat %>% 
  filter(!trimws(Department) %in% c(NA, "-", "")) %>% 
  dplyr::select(-Segment, -Units01) %>% 
  mutate(id=row_number())
Extra002 <- Extra001 %>% 
  filter(!(!Subsidy %in% c(" ", NA) & Price01 < 0)) %>%  
  filter(Price01 > 0) 
Extra003 <- Extra001 %>% 
  filter(!Subsidy %in% c(" ")) %>% 
  mutate(id_extra = as.integer(id - 1)) %>% 
  dplyr::select(-Date01, -Timofday03, -(Department:Product), -id) %>% 
  rename(Initials_extra = Initials, Sub_extra = Subsidy, Price_extra = Price01)

dbWriteTable(connection_ourdata, "Extra001", Extra001, row.names = FALSE, 
             overwrite = TRUE)
dbWriteTable(connection_ourdata, "Extra002", Extra002, row.names = FALSE, 
             overwrite = TRUE)
dbWriteTable(connection_ourdata, "Extra003", Extra003, row.names = FALSE, 
             overwrite = TRUE)

dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
dbListTables(connection_ourdata)
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
dbListFields(connection_ourdata, "Extra001")
dbListFields(connection_ourdata, "Extra002")
dbListFields(connection_ourdata, "Extra003")
dbDisconnect(connection_ourdata)


connection_ourdata <- dbConnect(SQLite(), "ourdata/r_db_ex.sqlite")
db_Extra001 <- dbReadTable(connection_ourdata, "Extra001")
db_Extra002 <- dbReadTable(connection_ourdata, "Extra002")
db_Extra003 <- dbReadTable(connection_ourdata, "Extra003")
dbDisconnect(connection_ourdata)
