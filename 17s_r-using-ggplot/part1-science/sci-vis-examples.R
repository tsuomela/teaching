####################################
# R script for science vis demos

# 0 - loading libraries

library(ggplot2)
library(dplyr)
library(GGally)
library(tidyverse)
library(hexbin)
library(viridis)
library(ggfortify)

# 1 - starting with iris basics

# basic x, y scatterplot
ggplot(iris, aes(x = Petal.Length, y = Sepal.Length)) +
  geom_point()

# scatterplot with colors for the different species
fig1 <- ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, color = Species)) +
  geom_point()

# scatterplot, faceted into 3 sections based on species
fig1 <- ggplot(iris, aes(x = Petal.Length, y = Sepal.Length))
fig1 + facet_wrap(~ iris$Species) + geom_point()

# 2 - msleep data

# scatterplot
ggplot(msleep, aes(x = sleep_total, y = sleep_rem, color = vore)) +
  geom_point()

# adding a stat layer
ggplot(msleep, aes(x = sleep_total, y = sleep_rem, color = vore)) +
  geom_point() +
  stat_smooth()

# changing the stat method to lm, instead of defualt loess
ggplot(msleep, aes(x = sleep_total, y = sleep_rem, color = vore)) +
  geom_point() +
  stat_smooth(method = "lm", se = F)

# 3 - ecology data

# examples from the ecology dataset found in the Data Carpentry ecology workshop
# http://www.datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html


# get data from figshare

surveys <- read.csv('data/portal_data_joined.csv')

# basic summaries and structurehead(surveys)
dim(surveys)
nrow(surveys)
ncol(surveys)
names(surveys)
# levels(sex)

# data manipulation to setup the graphs
# clean data for graphing
surveys_complete <- surveys %>%
  filter(species_id != "",         # remove missing species_id
         !is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         sex != "")                # remove missing sex
## Extract the most common species_id
species_counts <- surveys_complete %>%
  group_by(species_id) %>%
  tally %>%
  filter(n >= 50)
## Only keep the most common species
surveys_complete <- surveys_complete %>%
  filter(species_id %in% species_counts$species_id)


# building graphs

# basic scatterplot
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) +
  geom_point()

# putting the plot into an object so it can be referenced
surveys_plot <- ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length))

# using hexbin
surveys_plot +
  geom_hex()

# with alpha adjustment
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1)

# with colors for species
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, aes(color=species_id))

####################################
## boxplots

ggplot(data = surveys_complete, aes(x = species_id, y = hindfoot_length)) +
  geom_boxplot()

ggplot(data = surveys_complete, aes(x = species_id, y = hindfoot_length)) +
  geom_violin()

ggplot(data = surveys_complete, aes(x = species_id, y = hindfoot_length)) +
  geom_boxplot() +
  scale_y_log10()

ggplot(data = surveys_complete, aes(x = species_id, y = hindfoot_length)) +
  geom_jitter(alpha = 0.3, color = "tomato") +
  geom_boxplot(alpha = 0.5)

## working on summary line graphs showing changes over time
# more data setup to summarize for timelines

yearly_sex_counts <- surveys_complete %>%
  group_by(year, species_id, sex) %>%
  tally

yearly_sex_weight <- surveys_complete %>%
  group_by(year, sex, species_id) %>%
  summarize(avg_weight = mean(weight))

ggplot(data = yearly_sex_weight, aes(x=year, y=avg_weight, color = species_id, group = species_id)) +
  geom_line() +
  facet_grid(sex ~ .)

# cleaning up presentation of graph, modifying labels

ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex, group = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = 'Observed species in time',
       x = 'Year of observation',
       y = 'Number of species') +
  theme_bw()

# more improvements to the stylistic presentation
# refining the angle, justification, text size

ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex, group = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = 'Observed species in time',
       x = 'Year of observation',
       y = 'Number of species') +
  theme_bw() +
  theme(axis.text.x = element_text(colour="grey20", size=12, angle=90, hjust=.5, vjust=.5),
        axis.text.y = element_text(colour="grey20", size=12),
        text=element_text(size=16, family="Arial"))

# 4 - changing up some options for summary boxplots
# material taken from DataCamp

# position definitions
posn.d <- position_dodge(width = 0.1)
posn.jd <- position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2)
posn.j <- position_jitter(width = 0.2)

# The base ggplot command, you don't have to change this
wt.cyl.am <- ggplot(mtcars, aes(x = cyl,y = wt, col = am, fill = am, group = am))

# wt.cyl.am, posn.d, posn.jd and posn.j are available

# Plot 1: Jittered, dodged scatter plot with transparent points
wt.cyl.am +
  geom_point(position = posn.jd, alpha = 0.6)

# Plot 2: Mean and SD - the easy way
wt.cyl.am +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), position = posn.d)


# Plot 3: Mean and 95% CI - the easy way
wt.cyl.am +
  stat_summary(fun.data = mean_cl_normal, position = posn.d)


# Plot 4: Mean and SD - with T-tipped error bars - fill in ___
wt.cyl.am +
  stat_summary(geom = "point", fun.y = mean, 
               position = posn.d) +
  stat_summary(geom = "errorbar", fun.data = mean_sdl, 
               position = posn.d, fun.args = list(mult = 1), width = 0.1)

# 5 - density and other unique plots

# Add viridis color scale for the old faithful eruptions data
ggplot(faithful, aes(x = waiting, y = eruptions)) +
  scale_y_continuous(limits = c(1, 5.5), expand = c(0,0)) +
  scale_x_continuous(limits = c(40, 100), expand = c(0,0)) +
  coord_fixed(60/4.5) +
  stat_density_2d(geom = "tile", aes(fill = ..density..), h=c(5,.5), contour = FALSE) +
  scale_fill_viridis() +
  ## add bits to create axis labels - tes
  labs(
    title = "Eruptions of Old Faithful by time between eruptions",
    x = "Wating time between eruptions (minutes)",
    y = "Number of eruptions (count)"
  )

# ggpairs - for pair plots

## pair and correlation plots
library(GGally)

# using the iris data set

# pairs from base graphics
pairs(iris[1:4])

# chart.Correlation from PerformanceAnalytics
library(PerformanceAnalytics)
chart.Correlation(iris[1:4])

# ggpairs
ggpairs(iris[1:4])  # default, b&w
iris.p <- ggpairs(iris, mapping = aes(color = Species), columns = 1:4) # using color aesthetic for species
iris.p

# using the tips data
data(tips, package = "reshape2")

str(tips)
pm <- ggpairs(tips, columns = c("total_bill", "time", "tip"), columnLabels = c("Total Bill", "Time of Day", "Tip"))
pm

# autoplot for lm diagnostic graphs
# for autoplotting lm diagnostic graphs
# Create linear model: res
res <- lm(Volume ~ Girth, data = trees)

# Plot res
plot(res) # this plots out 4 diagnostic graphs

# from ggfortify and use autoplot()
autoplot(res, ncol = 2)

# hexagonal 2d density plot
d <- ggplot(diamonds, aes(carat, price))
d + geom_hex()

