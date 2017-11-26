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


