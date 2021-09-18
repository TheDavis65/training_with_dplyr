pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
               ggvis,httr, lubridate, plotly, rio, rmarkdown, shiny,
               stringr, tidyr, caret, lars, tidyverse, psych, dygraphs,
               vioplot, gapminder, nycflights13, gapminder, Lahman )


###################### dplyr #################################################
x <- "fantastic"
x

library(nycflights13)
library(tidyverse)

nycflights13?
flights
view(flights)
head(flights)


jan1 <-  filter(flights, month == 1, day == 1)
view(jan1)

dec25 <- filter(flights, month == 12, day == 25)
view(dec25)
head(dec25)

sqrt(2) ^ 2 == 2
#> [1] FALSE
1 / 49 * 49 == 1
#> [1] FALSE
near(sqrt(2) ^ 2,  2)
#> [1] TRUE
near(1 / 49 * 49, 1)
#> [1] TRUE

## Vil ikke virke
nov_dec <- filter(flights, month == 1 | month == 12)
view(nov_dec)
head(nov_dec)
## Men dette vil virke
nov_dec <- filter(flights, month %in% c(11,12))
view(nov_dec)
head(nov_dec)


delay1 <- filter(flights, !(arr_delay > 120 | dep_delay > 120))
head(delay1)
view(delay1)

delay2 <- filter(flights, arr_delay <= 120, dep_delay <= 120)
head(delay2)
view(delay2)



#### Opgave 5.2.4


### er forsinket mere end 2 timer  
delayed <-filter(flights, arr_delay >= 120)
view(delayed)
### Svar 10200 fly er forsinket

### hvormange fløj til Houston HOU eller IAH
IAH_HOU <- filter(flights, dest %in% c("IAH","HOU"))
view(IAH_HOU)
### Svar 9313 fly fløj til Houston

### alle fly der var i luften for american airline og delta

aa_ua_delta <- filter(flights, carrier %in% c("UA","DL","AA"))
view(aa_ua_delta)
### 139504 afgange var med AA UA eller Delta

##¤ hvor mange fly Havde afgang i juli, august eller September
jul_aug_sep <- filter(flights, month %in% c(7,8,9))
view(jul_aug_sep)
### 86326 afgik i juli, august og september

### var mere end 2 timer forsinket men afgik til tiden
arrLateMoreThan2HoursButOnTimeAtDeparture <- filter(flights, arr_delay >= 120, dep_delay <= 0)
view(arrLateMoreThan2HoursButOnTimeAtDeparture)
### svar 29

### var mindst 1 time forsinket men var i luften i mere end 30 minutter
delay1H30minInAir <- filter(flights, arr_delay >= 60, minute >= 30)
view(delay1H30minInAir)
### 2159 var forsinket mere end en time og havde mere end 30 minutters flyvetid

### afgenge mellem midnat og 6:00 inklusiv 6:00
betweenMidnigtAnd6 <- filter(flights, hour >= 0, hour <= 6)
view(betweenMidnigtAnd6)
### der var 2013 afgange

### between()
betweenMidnigtAnd6Between <- filter(flights, between(hour, 0, 6))      
view(betweenMidnigtAnd6Between) 

### dep_time mangler
missingDepTime <- filter(flights, dep_time == 0)
view(missingDepTime)
### hmmm 0

## 5.3 arrange()
arr1 <- arrange(flights, year, month, day)
view(arr1)

## Decending order
arr2 <- arrange(flights, desc(dep_delay))
view(arr2)

df <- tibble(x = c(5, 2, NA))
arrange(df, x)
#> # A tibble: 3 x 1
#>       x
#>   <dbl>
#> 1     2
#> 2     5
#> 3    NA
arrange(df, desc(x))
#> # A tibble: 3 x 1
#>       x
#>   <dbl>
#> 1     5
#> 2     2
#> 3    NA

#### 5.3.1

realFlights <- arrange(flights, x = is.na())
view(realFlights)

testFlight <-is.na(flights)
view(testFlight)
head(testFlight)
sum(is.na(testFlight))### 0
#### arrange()
arr3 <- arrange(flights, desc(dep_delay))
view(arr3)
arr4 <- arrange(flights, dep_delay)
view(arr4)

### hurtigeste fly
arr5 <- arrange(flights, air_time)
view(arr5)

### efter længeste distance
byFarthestTrip <- arrange(flights, desc(distance))
view(byFarthestTrip)

### efter korteste distance
byShortestDistance <- arrange(flights, distance)
view(byShortestDistance)

#### select()
### vælg kolonner ved navn
sel1 <- select(flights, year ,month ,day)
view(sel1)

### vælg kolonner i mellem år og dag
sel2 <- select(flights, year:day)
view(sel2)

### fravælg år til dag
sel3 <- select(flights, -(year:day))
view(sel3)

### rename()  VIRKER IKKE
rename(flights, tail_num = tailnum)
view(rename1)

### EVERYTHING()
evry1 <- select(flights, time_hour, air_time, everything())
view(evry1)

#### 5.4.1
## dep_time, dep_delay, arr_time, and arr_delay from flights.
sel4 <- select(flights, dep_time, dep_delay, arr_time, arr_delay)
view(sel4)



vars <- c("year", "month", "day", "dep_delay", "arr_delay")
sel5 <- select(flights, vars)
sel5


select(flights, contains("TIME"))
select(flights, contains("time"))

###### Mutate ################ 
#### et subset af flights
flight_small <- select(flights,
                       year:day,
                       ends_with("delay"),
                       distance,
                       air_time
                       )
view(flight_small)
flight_smallWithNewCol <- mutate(flight_small,
                                 gain = dep_delay - arr_delay,
                                 speed = distance / air_time * 60
                                 )

view(flight_smallWithNewCol)
addNewColMutate <- mutate(flight_smallWithNewCol, 
                          gain = dep_delay - arr_delay,
                          hours = air_time / 60,
                          gain_per_hour = gain / hours
                          )
view(addNewColMutate)

## transmute()
transmuted <- transmute(flights,
                        gain = dep_delay - arr_delay,
                        hours = air_time / 60,
                        gain_per_hour = gain / hours
                        )
view(transmuted)
#### transmute to INT
transmuted1 <- transmute(flights,
                        dep_time,
                        hours = dep_time %/% 100,
                        minute = dep_time %% 100
)
view(transmuted1)

### Lead() og lag()
(x <- 1:10)
lag(x)
lead(x)

cumsum(x)
cummean(x)


y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
#> [1]  1  2  2 NA  4  5
min_rank(desc(y))
#> [1]  5  3  3 NA  2  1
row_number(y)
#> [1]  1  2  3 NA  4  5
dense_rank(y)
#> [1]  1  2  2 NA  3  4
percent_rank(y)
#> [1] 0.00 0.25 0.25   NA 0.75 1.00
cume_dist(y)
#> [1] 0.2 0.6 0.6  NA 0.8 1.0

### 5.5.2
minutesFromMidnight <- transmute(flights,
                   dep_time,
                   hour = dep_time %/% 60,
                   minute = dep_time %% 60,
                   sched_dep_time,
                   shour = sched_dep_time %/% 60,
                   sminute = sched_dep_time %% 60,
                   diff = dep_time - sched_dep_time 
                    
)
view(minutesFromMidnight)

compair <- transmute(flights,
                                air_time,
                                hour = air_time %/% 60,
                                minute = air_time %% 60,
                                dep_time = as.double(dep_time),
                                dep_hour = dep_time %/% 60,
                                dep_minute = dep_time %% 60,
                                arr_time,
                                arr_hour = arr_time %/% 60,
                                arr_minute = arr_time %% 60,
                                diff_arr_time_dep_time = arr_time - dep_time,
                                diff_air_time_dep_time = air_time - dep_time
                                
                                 
)
view(compair)
head(compair)

head(flights)

comp2 <- transmute(flights,
                 dep_time,
                 dtHour = dep_time %/% 60,
                 dtMinute = dep_time %% 60,
                 sched_dep_time,
                 sdtHour = sched_dep_time %/% 60,
                 stdMinute = sched_dep_time %% 60,
                 dep_delay = as.integer(dep_delay),
                 ddHour = dep_delay %/% 60,
                 ddMinute = dep_delay %% 60
             
                 
                 
                   
                     )

view(comp2)
head(comp2)

comp3 <- transmute(flights,
                   dep_time,
                   sched_dep_time,
                   dep_delay = as.integer(dep_delay)
)

view(comp3)
head(comp3)


rank_delay <- filter(flights,min_rank(dep_delay, ))
head(rank_delay)


#### Summaries()
summer <- summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
view(summer)

### by group()
by_day <- group_by(flights, year, month, day)
summ <- summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))
view(summ)




flights
head(flights)
view(flights)
typeof(dep_time)
dep_time


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
