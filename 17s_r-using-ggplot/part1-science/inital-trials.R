# R ggplot examples

# using the iris (base) ; economics (ggplot) ; msleep (ggplot) data sets
# iris = scatterplot and facets
# economics = line plots, time series and facets
# msleep = scatter, stat(smooth)

# using the iris data set

# basic x, y scatterplot
ggplot(iris, aes = (x = Petal.Length, y = Sepal.Length)) +
  geom_point()

# scatterplot with colors for the different species
fig1 <- ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, color = Species)) +
  geom_point()

# scatterplot, faceted into 3 sections based on species
fig1 <- ggplot(iris, aes(x = Petal.Length, y = Sepal.Length))
fig1 + facet_wrap(~ iris$Species) + geom_point()


# Some examples using the economics data sets from the ggplot package
# note that there are two datasets : economics and economics_long

# Review the structure of the two datasets
str(economics)
str(economics_long)

# first line plot showing all variables involved in the long data set
fig2 <- ggplot(economics_long, aes(x = date, y = value, color = variable))
fig2 + geom_line()

# line plot showing median duration of unemployment
fig3 <- ggplot(economics, aes(date, uempmed))
fig3 + geom_line()

# same plot but adding points to the geoms so each month is given a point on the line
fig3 + geom_line() + geom_point()

# adding a horizontal line that shows the median of uempmed
fig3 + geom_line() + geom_point() + geom_hline(yintercept = median(economics$uempmed))

# trying to configure various facet options

# this first attempt facets on the different variables in the dataset but creates a 2x3 set of graphs
fig2 + facet_wrap(~ economics_long$variable) + geom_line()

# this attempt stacks graphs but does not change scales, so all graphs have the same vertical scales, making some lines appear to be zero
fig2 + facet_grid(economics_long$variable ~ .) + geom_line()

# this attempt successfully stacks the line graphs
fig2 + facet_grid(economics_long$variable ~ ., scales = "free") + geom_line()

# subsetting the economics data and plotting two graphs vertically
df4 <- subset(economics_long, variable == "uempmed" | variable == "unemploy")
fig4 <- ggplot(df4, aes(x = date, y = value))

# in b/w and then in 2 colors
fig4 + facet_grid(df4$variable ~ ., scales = "free") + geom_line()
fig4 + facet_grid(df4$variable ~ ., scales = "free") + geom_line(aes(color = df4$variable))

# using the msleep data

ggplot(msleep, aes(x = sleep_total, y = sleep_rem, color = vore)) +
  geom_point()
ggplot(msleep, aes(x = sleep_total, y = sleep_rem, color = vore)) +
  geom_point() +
  stat_smooth()
ggplot(msleep, aes(x = sleep_total, y = sleep_rem, color = vore)) +
  geom_point() +
  stat_smooth(method = "lm", se = F)

