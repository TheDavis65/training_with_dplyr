pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder",
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr",
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR2", "MASS", "testthat", "leaps", "caret",
               "RSQLite", "class", "babynames", "nasaweather",
               "fueleconomy", "viridis")


#  --------------------------------------

##CHT 15 - FACTORS

#In R, factors are used to work with categorical variables.
#They are also useful when you want to display character vectors in
#a non-alphabetical order. 

x1 <- c("Dec", "Apr", "Jan", "Mar")
x1
x2 <- c("Dec", "Apr", "Jam", "Mar")
x2
sort(x1)
str(x1)
#Creating a list of levels
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

y1 <- factor(x1, levels = month_levels)
y1
sort(y1)

#NA
y2 <- factor(x2, levels = month_levels)
y2
sort(y2)

#Warning in connection with typos (and other things):
#readr::parse_factor():
y2 <- parse_factor(x2, levels = month_levels)
y2

# Alphabetical order. 
factor(x1)

#The levels match the order of the first appearance
#Either:
f1 <- factor(x1, levels = unique(x1))
f1
#Or:
f2 <- x1 %>% factor() %>% fct_inorder()
f2


#Accessing the set of valid levels directly:
levels(f2)

#GSS - forcats::gss_cat
forcats::gss_cat
?gss_cat
#View(gss_cat)

gss_cat %>%
  count(race)
levels(gss_cat$race)

#Bar chart
ggplot(gss_cat, aes(race)) +
  geom_bar()

#Avoid dropping levels that don't have any values 
ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)


#Modifying the factor order:

relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
relig_summary
glimpse(relig_summary)

ggplot(relig_summary, aes(tvhours, relig)) + geom_point()

#Sorting the factor levels by tvhours
ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()
#f=relig, x=tvhours

#A separate mutate() step is recommended when transformations 
#get more complicated.
relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()


rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(rincome_summary, aes(x = age, y = fct_reorder(rincome, age))) + 
  geom_point()
?fct_reorder


#Pulling “Not applicable” to the front:
levels(rincome_summary$rincome)
ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()
?fct_relevel


by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)

levels(gss_cat$marital)

#fct_reorder2() reorders the factor by the y values associated 
#with the largest x values. This makes the plot easier to read 
#because the line colours line up with the legend.
ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")

levels(gss_cat$marital)


gss_cat %>%
  ggplot(aes(marital)) +
  geom_bar()


#fct_infreq() to order levels in increasing frequency
gss_cat %>%
  mutate(marital = marital %>% fct_infreq()) %>%
  ggplot(aes(marital)) +
  geom_bar()

#fct_rev() reverse order
gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar()


#Modifying order:
gss_cat %>% count(partyid)

#The levels are terse and inconsistent. Let’s tweak them to be 
#longer and use a parallel construction.
#fct_recode() will leave levels that aren’t explicitly mentioned 
#as is, and will warn you if you accidentally refer to a level that 
#doesn’t exist.

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
  )) %>%
  count(partyid)

#Combining groups: "Other" used in several groups
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat",
                              "Other"                 = "No answer",
                              "Other"                 = "Don't know",
                              "Other"                 = "Other party"
  )) %>%
  count(partyid)

#fct_collapse() is a useful variant of fct_recode()
#If you want to collapse a lot of levels

levels(gss_cat$partyid)

gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)


#Lump together small groups:

gss_cat %>%
  group_by(relig) %>%
  count()

gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)
#Probably not that usefull

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n = Inf) #Infinity - try say 5 instead





#  --------------------------------------
##CHT 16 - Dates
#  --------------------------------------
today()
now()

#Central European Summer Time - CEST (Daylight Saving Time)
#Coordinated Universal Time (UTC) 
#From Strings

ymd("2017-01-31")
mdy("January 31st, 2017")
mdy("January 31st, 2017", locale = ("US"))
dmy("31-Jan-2017")
ymd(20170131)
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")
ymd(20170131, tz = "UTC") #tz=timezone

#From individual components
flights %>% 
  dplyr::select(year, month, day, hour, minute)

flights %>% 
  dplyr::select(year, month, day, hour, minute) %>% 
   mutate(departure = make_datetime(year, month, day, hour, minute)) %>% 
    dplyr::select(departure, everything())
# make_datetime()
# View(flights)
flights



make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
} #time is just the fourth "place" in the function input.


flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  dplyr::select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt


#Visualise the distribution of departure times across the year.
flights_dt %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% #Only Jan 1
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes


#When you use date-times in this setting 1 means 1 sec.
#Other types:
today()
as_datetime(today())
as_date(now())
as_datetime(60 * 60 * 10)
as_date(365 * 10 + 2)

as_date(365 * 22 + 182)
as_datetime(60 * 60 * 24 * (365 * 22 + 182) + 20 * 60 * 60 + 15 * 60)
 

#Date-time components 
#Pull out individual parts of the date 

datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
month(datetime)
mday(datetime) #day of the month
yday(datetime) #day of the year
wday(datetime) #day of the week - hvilken ugedag er det? Lørdag? Eller måske fredag?
hour(datetime)
minute(datetime)
second(datetime)
month(datetime, label = TRUE) #With abbreviated labels
month(datetime, label = TRUE, abbr = FALSE) 
wday(datetime, label = TRUE, abbr = FALSE)

flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(minute, avg_delay)) +
  geom_line()


sched_dep <- flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n())

ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()

ggplot(sched_dep, aes(minute, n)) +
  geom_line()

#Rounding - round the date to a nearby unit of time
#floor_date() - Round down 
#round_date() - Round up
#ceiling_date() - Round to
?floor_date()
#This, for example, allows us to plot the number of flights per week:

flights_dt %>% 
  count(week = floor_date(dep_time, "month")) %>% 
  ggplot(aes(week, n)) +
  geom_line()


#Setting components - updating
(datetime <- ymd_hms("2016-07-08 12:34:56"))
year(datetime) <- 2020
datetime 
month(datetime) <- 01
datetime
hour(datetime) <- hour(datetime) + 1
datetime

ymd("2015-02-01") %>% 
  update(mday = 30)

ymd("2015-02-01") %>% 
  update(hour = 400)

ymd("2015-02-01") %>% 
  update(mday = 30) %>% 
  update(hour = -40000)


flight03 <- flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>%  #collapses to Jan 1 but time of the day hasn't changed
  dplyr::select(dep_time, everything()) %>%
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 600)

flight03



#  --------------------------------------
#Durations
#Durations measure the exact amount of time that 
#occurs between two instants. This can create unexpected
#results in relation to clock times if a leap second, leap year, 
#or change in daylight savings time (DST) occurs in the interval. 
#Durations represents an exact number of seconds. 
#60 seconds in a minute, 60 minutes in an hour, 24 hours in day, 
#7 days in a week, 365 days in a year....
#  --------------------------------------
EM <- today() - ymd(19920626)
EM
MILL <- today() - ydm(20000101)
MILL
h_age <- today() - ymd(19791014)
h_age

as.duration(h_age) #as.duration uses seconds
dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)
2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)
tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)
one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
one_pm
one_pm + ddays(1) #1 day AND 1 HOUR - AND the tz has chanced
#Daylight Saving Time (DST)
#Eastern Daylight Time (EDT)
#Eastern Standard Time (EST) 

#In order to solve this problem - lubridate provides periods:

#  --------------------------------------
#Periods
#Periods measure the change in clock time that occurs between 
#two instants. Periods provide robust predictions of clock time in 
#the presence of leap seconds, leap years, and changes in DST.
#Works with “human” times, like days and months
#  --------------------------------------
one_pm
one_pm + ddays(1)
one_pm + days(1)
?days
seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1:20)
10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)

ymd("2016-01-01") + dyears(1) # 365 days (Wrong?) : leap year
ymd("2017-01-01") + dyears(1) # 365 days
ymd("2017-01-01") - ymd("2016-01-01") # 366 days due to leap year
ymd("2016-01-01") + years(1) # 366 days (Right?) : leap year
ymd("2016-01-01") + 12*months(1)

# Daylight Savings Time
one_pm
one_pm + ddays(1)
one_pm + days(1)
one_pm + 48*months(1)

#arr_time before dep_time due to overnight flights. Add one day: days(1)
flights_dt %>% 
  filter(arr_time < dep_time) 

flights_dt_01 <- flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time,
    overnight01 = days(overnight * 1),
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  ) %>%
  dplyr::select(
    origin, dest, arr_time, dep_time, overnight, overnight01,
    sched_arr_time, everything()
  ) %>% 
  arrange(desc(overnight))
flights_dt_01

flights_dt_01 %>% 
  filter(arr_time < dep_time) 

# Interval
dyears(1) / ddays(365)  #Duration: 1 year always = 365!

years(1) / days(1)  #More information needed - either 365 or 366. We only
#get an estimate.
# dyears --> duration
# years --> period
# ddays --> duration
# days --> period

#  --------------------------------------
#Interval
#Intervals are timespans that begin at a specific instant and end 
#at a specific instant. Intervals retain complete information about
#a timespan. They provide the only reliable way to convert between 
#periods and durations. 
#If you want a more accurate measurement, you’ll have to use an interval.
#  --------------------------------------
next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)
#To find out how many periods fall into an interval, you need to use
#integer division:
(today() %--% next_year) %/% days(1)



  #  --------------------------------------
#Timezones

Sys.timezone()

length(OlsonNames())
head(OlsonNames())
(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))

x1 - x2
x1 - x3
x4 <- c(x1, x2, x3)
x4

x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a
x4a - x4

x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b
x4b - x4


