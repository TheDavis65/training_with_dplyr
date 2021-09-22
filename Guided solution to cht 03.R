# Opgaver:
# 3.2.4. (1.-5.)
# 1
ggplot(data = mpg)
# 2
nrow(mpg)
ncol(mpg)
glimpse(mpg)
# 3
?mpg
# "f"	front-wheel drive
# "r"	rear-wheel drive
# "4"	four-wheel drive
# 4
ggplot(mpg, aes(x = cyl, y = hwy)) +
  geom_point()
# 5
ggplot(mpg, aes(x = class, y = drv)) +
  geom_point()

count(mpg, drv, class)
 
ggplot(mpg, aes(x = class, y = drv)) +
  geom_count()

mpg %>%
  count(class, drv) %>%
  ggplot(aes(x = class, y = drv)) +
  geom_tile(mapping = aes(fill = n))

mpg %>%
  count(class, drv) %>%
  complete(class, drv, fill = list(n = 0)) %>%
  ggplot(aes(x = class, y = drv)) +
  geom_tile(mapping = aes(fill = n))


# 3.3.1. (1.-6.)
# 1
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = "blue"))

# The argument colour = "blue" is included within the mapping
# argument, and as such, it is treated as an aesthetic, which 
# is a mapping between a variable and a value. In the expression, 
# colour = "blue", "blue" is interpreted as a categorical 
# variable which only takes a single value "blue".

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), colour = "blue")

#2
mpg
summary(mpg)
glimpse(mpg)

#3
ggplot(mpg, aes(x = displ, y = hwy, colour = cty)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, size = cty)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, shape = cty)) +
  geom_point()

#4
ggplot(mpg, aes(x = displ, y = hwy, colour = hwy, size = displ)) +
  geom_point()


#5
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape = 21, colour = "black", 
             fill = "red", size = 2, stroke = 2)

#6
ggplot(mpg, aes(x = displ, y = hwy, colour = year < 2000)) +
  geom_point()


#3.5.1. (1.-6.)
#1
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(. ~ cty)

#2
ggplot(data = mpg) +
  geom_point(mapping = aes(x = hwy, y = cty)) +
  facet_grid(drv ~ cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = drv, y = cyl))

table(mpg$drv, mpg$cyl)


#3
# The symbol . ignores that dimension when faceting. For 
# example, drv ~ . facet by values of drv on the y-axis.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

#4
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~class, nrow = 2)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class), alpha = 0.5)

#5
?facet_wrap
# The arguments nrow (ncol) determines the number of rows (columns) 
# to use when laying out the facets. It is necessary since facet_wrap() 
# only facets on one variable.
# 
# The nrow and ncol arguments are unnecessary for facet_grid() since 
# the number of unique values of the variables specified in the 
# function determines the number of rows and columns.

#6
# There will be more space for columns if the plot is laid out 
# horizontally (landscape).



#3.6.1. (1.-5.)
#1
# line chart: geom_line()
# boxplot: geom_boxplot()
# histogram: geom_histogram()
# area chart: ?geom_area()

#2

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv)) +
  geom_point() +
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)

#3
ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, colour = drv),
    show.legend = FALSE
  )

#4
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)
#It adds standard error bands to the lines.
# By default se = TRUE
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv)) +
  geom_point() +
  geom_smooth()

#5
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

#3.8.1. (1.-2.)
#1
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point()
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point(position = "jitter")

#2
# width controls the amount of horizontal displacement, and
# height controls the amount of vertical displacement.
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point(position = position_jitter())

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_jitter(width = 0)
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_jitter(width = 20)
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_jitter(height = 0)
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_jitter(height = 15)
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_jitter(height = 0, width = 0)
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_jitter()
