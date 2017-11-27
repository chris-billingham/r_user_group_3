library(tidyverse)

# let's load some data
housing <- read.csv("data_sets/landdata-states.csv")
head(housing[1:5])

# this is a histogram just using base-r graphics
hist(housing$Home.Value)

# same thing using ggplot2
ggplot(housing, aes(x = Home.Value)) +
  geom_histogram()

# all well and good, let's make it a coloured scatterplot
# and tidy up the look of the thing
plot(Home.Value ~ Date,
     data=subset(housing, State == "MA"))
points(Home.Value ~ Date, col="red",
       data=subset(housing, State == "TX"))
legend(1975, 400000,
       c("MA", "TX"), title="State",
       col=c("black", "red"),
       pch=c(1, 1))

# that was a palaver, let's do it in ggplot (and use dplyr whilst we're there)
housing %>% 
  filter(State == "MA" | State == "TX") %>%
  ggplot(aes(x=Date,
             y=Home.Value,
             color=State)) +
  geom_point()

## Aesthetic Mapping
## Geometic Objects (`geom')

help.search("geom_", package = "ggplot2")

# let's get some data, filter it down
hp2001Q1 <- housing %>% 
  filter(Date == 2001.25)

# cool let's plot this
ggplot(hp2001Q1,
       aes(y = Structure.Cost, x = Land.Value)) +
  geom_point()

# hmm looks pretty sparse at high values of x. let's log it
ggplot(hp2001Q1,
       aes(y = Structure.Cost, x = log(Land.Value))) +
  geom_point()

# cool so you can log directly into it, 
# note we didn't actually change the data just the aesthetic

# let's layer some new stuff on.
# knock up a regression model
hp2001Q1$pred_sc <- predict(lm(Structure.Cost ~ log(Land.Value), data = hp2001Q1))

# we can load the ggplot command into a variable to call later
plot_1 <- ggplot(hp2001Q1, aes(x = log(Land.Value), y = Structure.Cost))

# note if i run this (without any geoms) it just shows the axes
plot_1

# let's do it with the point but we've coloured the points
# by setting the colour aesthetic in the geom_point to Home.Value
plot_1 + geom_point(aes(color = Home.Value))

# right now let's do the same thing but add the regression line as well
plot_1 + 
  geom_point(aes(color = Home.Value)) +
  geom_line(aes(y = pred_sc))

# a bit of a crude line of fit however, let's do something better looking
# let's look at geom_smooth()
plot_1 +
  geom_point(aes(color = Home.Value)) +
  geom_smooth()

# sometimes we want to label things up instead
plot_1 + 
  geom_point() + 
  geom_text(aes(label=State), size = 3)

# that's a bit of a mess
# here we can supplement ggplot with NEW geom's
library(ggrepel)
plot_1 + 
  geom_point() + 
  geom_text_repel(aes(label=State), size = 3)

# we can also continue to modify the representation by
# using different shapes due to region
plot_1 +
  geom_point(aes(color=Home.Value, shape = region))

# let's use some what we've learned so far on that economist data
dat <- read.csv("data_sets/EconomistData.csv")
head(dat)

ggplot(dat, aes(x = CPI, y = HDI, size = HDI.Rank)) + 
  geom_point()

# statistical transformations
# default histogram
plot_2 <- ggplot(housing, aes(x = Home.Value))
plot_2 + 
  geom_histogram()

# hmm what's it tell me off? 
# we can probably choose a better bucket for the histogram
plot_2 + 
  geom_histogram(stat = "bin", binwidth=4000)

# sometimes default statistical transformation isn't what you want
# this happens a lot with pre-summarised data
# let's make some
housing_sum <- housing %>% 
  group_by(State) %>% 
  summarise(Home.Value = mean(Home.Value))

# now plot it
ggplot(housing_sum, aes(x=State, y=Home.Value)) + 
  geom_bar()

# ah
# so this failed because we took binned and summarised data and asked
# ggplot to do this again for us. the answer was no. 
# geom_bar() defaults to stat = stat_count
ggplot(housing_sum, aes(x=State, y=Home.Value)) + 
  geom_bar(stat="identity")

# much better. stat = "identity" just says "use the number you find"

# scales
# aesthetic mapping only determines what variable should be
# mapped to an aesthetic. it doesn't say how it should happen.
# so aes(shape = x) just says use x for shape, it doesn't say
# what shape. similarly aes(colour = x) just says use x for colour
# not what colour. scales allow you to change that

# let's make a dot plot of state by HPI
# not the use of the theme to put some words on
plot_3 <- ggplot(housing,
             aes(x = State,
                 y = Home.Price.Index)) + 
  theme(legend.position="top",
        axis.text=element_text(size = 6))

plot_3
# note, no geom, no data
# let's do some layering
plot_4 <- (plot_3 + geom_point(aes(color = Date),
                       alpha = 0.5,
                       size = 1.5,
                       position = position_jitter(width = 0.25, height = 0)))

plot_4
# jiggly
# now were going to amend the breaks for the x-axis and the colour
plot_4 + scale_x_discrete(name="State Abbreviation") +
  scale_color_continuous(name="",
                         breaks = c(1976, 1994, 2013),
                         labels = c("'76", "'94", "'13"))


# finally we're going to change the colour scale so
# low is blue and high is red
plot_4 +
  scale_x_discrete(name="State Abbreviation") +
  scale_color_continuous(name="",
                         breaks = c(1976, 1994, 2013),
                         labels = c("'76", "'94", "'13"),
                         low = "blue", high = "red")

# colourful but probably a bit bright
# the scales package can help here, with the function muted()
library(scales)
plot_4 +
  scale_color_continuous(name="",
                         breaks = c(1976, 1994, 2013),
                         labels = c("'76", "'94", "'13"),
                         low = muted("blue"), high = muted("red"))

# not nearly as garish
# ggplot has loads of different colour scales you can experiment with
# here we're going to use scale_color_gradient2 to add a middle colour
plot_4 +
  scale_color_gradient2(name="",
                        breaks = c(1976, 1994, 2013),
                        labels = c("'76", "'94", "'13"),
                        low = muted("blue"),
                        high = muted("red"),
                        mid = "gray60",
                        midpoint = 1994)

# faceting
# you know in excel when you make a chart and want to do that same chart
# but for loads of different filters (i.e. trading_title_codes)
# and how that's a pain in the backside
# let faceting guide you.

# here's the basic date/value plot with all states
plot_5 <- ggplot(housing, aes(x = Date, y = Home.Value))
plot_5 + 
  geom_line(aes(color = State))  

# a bit busy
# let's use facet_wrap
plot_5 + 
  geom_line() +
  facet_wrap(~ State, ncol = 10)

# note, they all use the same x and y axes
# you can let them use their own with scales = "free"
# however be careful with it
plot_5 + 
  geom_line() +
  facet_wrap(~ State, ncol = 10, scales = "free")

# themes
# themes allow you to set all non-plot elements at once
# such as axis labels, background colours and so on
plot_5 + 
  geom_line() +
  facet_wrap(~ State, ncol = 10) +
  theme_linedraw()

plot_5 + 
  geom_line() +
  facet_wrap(~ State, ncol = 10) +
  theme_minimal()

# you can also make your own. this can take a while but does look good
theme_new <- theme_bw() +
  theme(plot.background = element_rect(size = 1, color = "blue", fill = "black"),
        text=element_text(size = 12, color = "ivory"),
        axis.text.y = element_text(colour = "purple"),
        axis.text.x = element_text(colour = "red"),
        panel.background = element_rect(fill = "pink"),
        strip.background = element_rect(fill = muted("orange")))

plot_5 + 
  geom_line() +
  facet_wrap(~ State, ncol = 10) +
  theme_new

# hideous
# finally a word on data formatting when used with ggplot
# if you have two variables in your data frame you want to plot
# on the same chart with different colours

housing_byyear <- housing %>%
  group_by(Date) %>%
  summarise(Home.Value = mean(Home.Value), Land.Value = mean(Land.Value))

# this looks right but is wrong
# well it's wrong, from a certain point of view
ggplot(housing_byyear,
       aes(x=Date)) +
  geom_line(aes(y=Home.Value), color="red") +
  geom_line(aes(y=Land.Value), color="blue")

# you've used wide (or tidy) data, however if you want to efficiently
# use all ggplot's functionality it's best to use long data
library(tidyr)
home_land_byyear <- gather(housing_byyear,
                           value = "value",
                           key = "type",
                           Home.Value, Land.Value)

head(home_land_byyear)

# which gives you this, much more succint
ggplot(home_land_byyear,
       aes(x=Date,
           y=value,
           color=type)) +
  geom_line()

