############### Exercises ######################## 
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
               ggvis,httr, lubridate, plotly, rio, rmarkdown, shiny,
               stringr, tidyr, caret, lars, tidyverse, psych, dygraphs,
               vioplot, gapminder, nycflights13, gapminder, Lahman, ISLR2,
               hms, feather, haven, readxl, DBI, jsonlite, xml2, lvplot 
)




### 
library(dplyr)
library(nycflights13)
library(tidyverse)

### 7.4.1

diamonds <- diamonds

head(diamonds)
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))
diamonds2

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point()

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)
# man får denne advarsel, man kan fjerne advarsel med na.rm = TRUE men de 9 rækker forsvinder stadig
#> Warning: Removed 9 rows containing missing values (geom_point).


### histogram

diamonds3 <- diamonds

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))
diamonds2

### med NA verdier
ggplot(data = diamonds3, mapping = aes(x = y)) +
  geom_histogram()

### uden NA verdier
ggplot(data = diamonds2, mapping = aes(x = y)) +
  geom_histogram()
### uden fejlmeddelser "
ggplot(data = diamonds2, mapping = aes(x = y)) +
  geom_histogram(na.rm = TRUE)
### konklution den viser ikke alt når der er tomme verdier 

### 7.4.2

### fjerner NA verdier før gennensnit eller sum bliver udregnet

# 7.5.1.1.1.
## Use what you’ve learned to improve the visualisation of the departure times of cancelled vs. non-cancelled flights.

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

### med boxplot
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot() +
  geom_boxplot(mapping = aes(y = sched_dep_time, x = cancelled))

par(mfrow = c(2, 1))
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = sched_dep_time)) 

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = cancelled)) 


# 7.5.1.1.5.
#Hvad er de generelle forhold mellem hver variabel og prisen på diamanterne? 
#Jeg vil overveje variablerne: karat, klarhed, farve og snit. Jeg ignorerer diamantens dimensioner, da karat måler størrelse og inkorporerer dermed de fleste oplysninger i disse variabler.
#Da både pris og karat er kontinuerlige variabler, bruger jeg et spredningsdiagram til at visualisere deres forhold.

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()

### men man kan også bruge et boxplot
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))
ggplot(data = diamonds2, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)), orientation = "x")


#Variablerne farve og klarhed er ordnet kategoriske variabler. Kapitlet foreslår at
#visualisere en kategorisk og kontinuerlig variabel ved hjælp af frekvenspolygoner eller 
#boxplots. I dette tilfælde vil jeg bruge et boksplot, da det bedre vil vise et forhold 
#mellem variablerne.

#Der er et svagt negativt forhold mellem farve og pris. Diamantfarvens skala går fra D 
#(bedst) til J (værst). I øjeblikket er niveauerne af diamanter $ farve i den forkerte 
#rækkefølge. Inden jeg plotter, vil jeg vende rækkefølgen på farveniveauerne, så de vil 
#være i stigende kvalitetsrækkefølge på x-aksen. Farvesøjlen er et eksempel på en 
#faktorvariabel, som er dækket i kapitlet "Faktorer"

diamonds %>%
  mutate(color = fct_rev(color)) %>%
  ggplot(aes(x = color, y = price)) +
  geom_boxplot()

ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = clarity, y = price))

ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_boxplot()
# 7.5.1.1.5.3

ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_boxplot() +
  coord_flip()

# 7.5.1.1.5.4

diamondsddd <- as.data.frame(diamonds)

ggplot(diamondsddd, aes(x = diamondsddd$cut, y = diamondsddd$price)) +
  geom_lv()
  

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