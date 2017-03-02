#Molly Jenkins 
#BIOL669_04 ggplot2 chs 3&4 examples/activities script 

#setwd("C:/git/coursework/")

library(ggplot2)
library(directlabels)
library(broom)


df <- data.frame(
  x = c(3, 1, 5),
  y = c(2, 4, 6),
  label = c("a","b","c")
)
p <- ggplot(df, aes(x, y, label = label)) +
  labs(x = NULL, y = NULL) + # Hide axis label
  theme(plot.title = element_text(size = 12)) # Shrink plot title
p + geom_point() + ggtitle("point")
p + geom_text() + ggtitle("text")
p + geom_bar(stat = "identity") + ggtitle("bar")
p + geom_tile() + ggtitle("raster")
p + geom_line() + ggtitle("line") #connects but out of data sequence
p + geom_area() + ggtitle("area")
p + geom_path() + ggtitle("path") #connects in order of data sequence
p + geom_polygon() + ggtitle("polygon")

# What geoms would you use to draw each of the following named plots?
# 1. Scatterplot -> geom_point
# 2. Line chart -> geom_line or geom_path
# 3. Histogram -> geom_bar but with modifier to have bars on y axis
# 4. Bar chart -> geom_bar
# 5. Pie chart -> why would I want this
# 2. What's the difference between geom_path() and geom_polygon()? What's
# the difference between geom_path() and geom_line()? geom path is a line type and polygon is a polygon filled type; 
#path is sequential and line is just visually sequential 
# 3. What low-level geoms are used to draw geom_smooth()? What about 
# geom_boxplot() and geom_violin()? 


#labelling

df <- data.frame(x = 1, y = 3:1, face = c("plain", "bold", "italic"))
ggplot(df, aes(x, y)) +
  geom_text(aes(label = face, fontface = face))

df <- data.frame(
  x = c(1, 1, 2, 2, 1.5),
  y = c(1, 2, 1, 2, 1.5),
  text = c(
    "bottom-left", "bottom-right",
    "top-left", "top-right", "center"
  )
)
ggplot(df, aes(x, y)) +
  geom_text(aes(label = text))
ggplot(df, aes(x, y)) +
  geom_text(aes(label = text), vjust = "inward", hjust = "inward")

df <- data.frame(trt = c("a", "b", "c"), resp = c(1.2, 3.4, 2.5))
ggplot(df, aes(resp, trt)) +
  geom_point() +
  geom_text(aes(label = paste0("(", resp, ")")), nudge_y = -0.25) +
  xlim(1, 3.6)

ggplot(mpg, aes(displ, hwy)) +
  geom_text(aes(label = model)) +
  xlim(1, 8)
ggplot(mpg, aes(displ, hwy)) +
  geom_text(aes(label = model), check_overlap = TRUE) +
  xlim(1, 8)


label <- data.frame(
  waiting = c(55, 80),
  eruptions = c(2, 4.3),
  label = c("peak one", "peak two")
)
ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_tile(aes(fill = density)) +
  geom_label(data = label, aes(label = label))

ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point()
ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point(show.legend = FALSE) +
  directlabels::geom_dl(aes(label = class), method = "smart.grid")



ggplot(economics, aes(date, unemploy)) +
  geom_line()
presidential <- subset(presidential, start > economics$date[1])
ggplot(economics) +
  geom_rect(
    aes(xmin = start, xmax = end, fill = party),
    ymin = -Inf, ymax = Inf, alpha = 0.2,
    data = presidential
  ) +
  geom_vline(
    aes(xintercept = as.numeric(start)),
    data = presidential,
    colour = "grey50", alpha = 0.5
  ) +
  geom_text(aes(x = start, y = 2500, label = name),
            data = presidential,
            size = 3, vjust = 0, hjust = 0, nudge_x = 50
  ) +
  geom_line(aes(date, unemploy)) +
  scale_fill_manual(values = c("blue", "red"))


yrng <- range(economics$unemploy)
xrng <- range(economics$date)
caption <- paste(strwrap("Unemployment rates in the US have
                         varied a lot over the years", 40), collapse = "\n")
ggplot(economics, aes(date, unemploy)) +
  geom_line() +
  annotate("text", x = xrng[1], y = yrng[2], label = caption,
           hjust = 0, vjust = 1, size = 4
  )



ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d() +
  facet_wrap(~cut, nrow = 1)
mod_coef <- coef(lm(log10(price) ~ log10(carat), data = diamonds))
ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d() +
  geom_abline(intercept = mod_coef[1], slope = mod_coef[2],
              colour = "white", size = 1) +
  facet_wrap(~cut, nrow = 1)



#group aes
data(Oxboys, package = "nlme")
head(Oxboys)

ggplot(Oxboys, aes(age, height, group = subject)) +
  geom_point() +
  geom_line()

#if leave out grouping var get sawtooth error 
#if want to group by multi vars use analog to group_by, which is the interaction function: 

aes(group =
      interaction(school_id, student_id))


ggplot(mpg, aes(class)) +
  geom_bar()
ggplot(mpg, aes(class, fill = drv)) +
  geom_bar() #stacked bar comes from fill = diff vars

# 
# 3.5.5 Exercises
# 1. Draw a boxplot of hwy for each value of cyl, without turning cyl into a
# factor. What extra aesthetic do you need to set?
# 2. Modify the following plot so that you get one boxplot per integer value
# value of displ.
# ggplot(mpg, aes(displ, cty)) +
#   geom_boxplot()
# 3. When illustrating the difference between mapping continuous and discrete
# colours to a line, the discrete example needed aes(group = 1). Why? What
# happens if that is omitted? What's the difference between aes(group = 1)
# and aes(group = 2)? Why?
# 4. How many bars are in each of the following plots?
# ggplot(mpg, aes(drv)) +
#   geom_bar()
# ggplot(mpg, aes(drv, fill = hwy, group = hwy)) +
#   geom_bar()
# library(dplyr)
# mpg2 <- mpg %>% arrange(hwy) %>% mutate(id = seq_along(hwy))
# ggplot(mpg2, aes(drv, fill = hwy, group = id)) +
#   geom_bar()
# (Hint: try adding an outline around each bar with colour = "white")
# 56 3 Toolbox
# 5. Install the babynames package. It contains data about the popularity of
# babynames in the US. Run the following code and fix the resulting graph.
# Why does this graph make me unhappy?
# library(babynames)
# hadley <- dplyr::filter(babynames, name == "Hadley")
# ggplot(hadley, aes(year, n)) +
#   geom_line()

# 
# 3.11.1 Exercises
# 1. What binwidth tells you the most interesting story about the distribution
# of carat?
# 2. Draw a histogram of price. What interesting patterns do you see?
# 3. How does the distribution of price vary with clarity?
# 4. Overlay a frequency polygon and density plot of depth. What computed
# variable do you need to map y to make the two plots comparable? (You
#                                                                  can either modify geom_freqpoly() or geom_density().)


# 
# 4.5 Exercises
# 1. One of the best ways to get a handle on how the grammar works is to
# apply it to the analysis of existing graphics. For each of the graphics listed
# below, write down the components of the graphic. Don't worry if you don't
# know what the corresponding functions in ggplot2 are called (or if they
#                                                              even exist!), instead focussing on recording the key elements of a plot so
# you could communicate it to someone else.
# 1. "Napoleon's march" by Charles John Minard: http://www.datavis.ca/
#   gallery/re-minard.php
# 2. "Where the Heat and the Thunder Hit Their Shots", by Jeremy White,
# Joe Ward, and Matthew Ericson at The New York Times. http://nyti.
# ms/1duzTvY
# 90 4 Mastering the grammar
# 3. "London Cycle Hire Journeys", by James Cheshire. http://bit.ly/
#   1S2cyRy
# 4. The Pew Research Center's favorite data visualizations of 2014: http:
#   //pewrsr.ch/1KZSSN6
# 5. "The Tony's Have Never Been so Dominated by Women", by Joanna
# Kao at FiveThirtyEight: http://53eig.ht/1cJRCyG.
# 6. "In Climbing Income Ladder, Location Matters" by the Mike Bostock,
# Shan Carter, Amanda Cox, Matthew Ericson, Josh Keller, Alicia Parlapiano,
# Kevin Quealy and Josh Williams at the New York Times:
#   http://nyti.ms/1S2dJQT
# 7. "Dissecting a Trailer: The Parts of the Film That Make the Cut", by
# Shan Carter, Amanda Cox, and Mike Bostock at the New York Times:
#   http://nyti.ms/1KTJQOE


####Fletcher's code for 03/02####
library(ggplot2)
mpg

# 5.3.1 Exercises
library(dplyr) 
class <- mpg %>% 
  group_by(class) %>%
  summarise(n = n(), hwy = mean(hwy))

class

ggplot(mpg, aes(class, hwy)) + 
  geom_point()

ggplot(mapping = aes(class, hwy)) +
  geom_point(data = mpg, position = position_jitter(width = 0.1, height = 0.1)) + 
  geom_point(data = class, color = "red", size = 5) + 
  geom_text(data = class, aes(y = 10, label = paste("n =", n)))

# 5.4.3 exercises
# 1. simplify the following plot specifications
ggplot(mpg) + geom_point(aes(mpg$displ, mpg$hwy))
# simplification
ggplot(mpg, aes(displ, hwy)) + geom_point()

ggplot() +
  geom_point(mapping = aes(y = hwy, x = cty), data = mpg) + geom_smooth(data = mpg, mapping = aes(cty, hwy))
# simplification
ggplot(mpg, aes(cty,hwy)) + geom_point() + geom_smooth()

ggplot(diamonds, aes(carat, price)) + geom_point(aes(log(brainwt), log(bodywt)), data = msleep)
# simplification
# this one doesn't make sense to me


# 2. what does the following code do? 
# Does it work? Does it make sense? Why/why not?
ggplot(mpg) + geom_point(aes(class, cty)) + geom_boxplot(aes(trans, hwy))


# 5.6.2 Exercise
# use the appropriate geoms to mimic the geom_smooth() display

mod <- loess(hwy ~ displ, data = mpg)
smoothed <- data.frame(displ = seq(1.6, 7, length = 50)) 
pred <- predict(mod, newdata = smoothed, se = TRUE) 
smoothed$hwy <- pred$fit
smoothed$hwy_lwr <- pred$fit - 1.96 * pred$se.fit 
smoothed$hwy_upr <- pred$fit + 1.96 * pred$se.fit

# my figure
ggplot(smoothed, aes(displ, hwy)) + 
  geom_ribbon(aes(ymin = hwy_lwr, ymax = hwy_upr), fill = "grey50") + 
  geom_line(color = "blue")

# use geom_count() to create a plot that shows 
# the proportion of cars that have each combination 
# of drv and trans

?stat_sum()
ggplot(mpg, aes(drv, trans)) + 
  geom_count(aes(size = ..prop.., group = 1))



###Ex. from ch 5 
library(dplyr) 
library(ggplot2)
mpg = mpg
class = mpg %>% 
  group_by(class) %>% 
  summarise(n = n(), 
            hwy = mean(hwy)) 
mpg$fclass = as.factor(mpg$class)
ggplot(data = mpg, mapping = aes(class, mean(hwy))) +
  geom_point(color = "red", size = 5) + 
  geom_text(data = mpg, aes(y = 10, label = paste("n =", tally(fclass))))



ggplot(mpg, aes(displ, hwy)) +geom_point()
  
ggplot(mpg, aes(hwy, cty)) + geom_point() +
  geom_smooth()

ggplot(diamonds, aes(carat, price)) +
  geom_point(aes(log(brainwt), log(bodywt)), data = msleep)


ggplot(mpg) + geom_boxplot(aes(trans, hwy)) 
ggplot(mpg) + geom_boxplot(aes(class, hwy)) 


mod <- loess(hwy ~ displ, data = mpg)
smoothed <- data.frame(displ = seq(1.6, 7, length = 50))
pred <- predict(mod, newdata = smoothed, se = TRUE)
smoothed$hwy <- pred$fit
smoothed$hwy_lwr <- pred$fit - 1.96 * pred$se.fit
smoothed$hwy_upr <- pred$fit + 1.96 * pred$se.fit

ggplot(smoothed, aes(displ, hwy)) +geom_ribbon(aes(ymin=hwy_lwr, ymax=hwy_upr, alpha = 0.4))+ geom_line(color = "navy")
#mimicking geom_smooth()


#from ggplot mimick graph from beginning, workaround:
geom_text(stat=count, y = min(mpg$hwy)) #want min of this vector


