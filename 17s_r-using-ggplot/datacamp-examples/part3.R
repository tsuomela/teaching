# DataCamp ggplot class 3

###############################################
# chapter 1

### scatter plots

library(ggplot2movies)
set.seed(123)
movies_small <- movies[sample(nrow(movies), 1000), ]
movies_small$rating <- factor(round(movies_small$rating))

# Explore movies_small with str()
str(movies_small)

# Build a scatter plot with mean and 95% CI
ggplot(movies_small, aes(x = rating, y = votes)) +
  geom_point() +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "crossbar",
               width = 0.2,
               col = "red") +
  scale_y_log10()


### box plots
# Add a boxplot geom
d <- ggplot(movies_small, aes(x = rating, y = votes)) +
  geom_point() +
  geom_boxplot() +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "crossbar",
               width = 0.2,
               col = "red")

# Untransformed plot
d

# Transform the scale
d + scale_y_log10()

# Transform the coordinates
d + coord_trans(y = "log10")

### density plots

test_data <- rnorm(200, 3, 0.25)

# Calculating density: d
d <- density(test_data$norm)

# Use which.max() to calculate mode
mode <- d$x[which.max(d$y)]

# Finish the ggplot call
ggplot(test_data, aes(x = norm)) +
  geom_rug() +
  geom_density()+
  geom_vline(xintercept = mode, col = "red")


# Arguments you'll need later on
fun_args <- list(mean = mean(test_data$norm), sd = sd(test_data$norm))

# Finish the ggplot
ggplot(test_data, aes(x = norm)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(col = "red") +
  stat_function(fun = dnorm, args = fun_args, col = "blue")

# for the different densities and adjustments
small_data <- data.frame(x = c(-3.5, 0.0, 0.5, 6.0))

# Get the bandwith
get_bw <- density(small_data$x)$bw

# Basic plotting object
p <- ggplot(small_data, aes(x = x)) +
  geom_rug() +
  coord_cartesian(ylim = c(0,0.5))

# Create three plots
p + geom_density()
p + geom_density(adjust = 0.25)
p + geom_density(bw = 0.25 * get_bw)

# Create two plots
p + geom_density(kernel = "r")
p + geom_density(kernel = "e")

## 2d density plots
# using the old faithful data

# Load in the viridis package
library(viridis)

# Add viridis color scale
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


####################################
# chapter 2
# plots for specific data types
# - pair plots and correlations
# - ternary
# - diagnostic
# - autoplot
####################################

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

####################################
## for ternary plots and stacked bar charts using soil data from Africa
## the africa dataset is located in the GSIF package https://cran.r-project.org/package=GSIF

## stacked bar charts

str(africa)

# Sample the dataset
africa_sample <- africa[sample(1:nrow(africa), size = 50), ]

# Add an ID column from the row.names
africa_sample$ID <- row.names(africa_sample)

# Gather africa_sample
library(tidyr)
africa_sample_tidy <- gather(africa_sample, key, value, -ID)
head(africa_sample_tidy)

# Finish the ggplot command
ggplot(africa_sample_tidy, aes(x = factor(ID), y = value, fill = key)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Location", y = "Composition", fill = "Component") +
  theme_minimal()

# Plot 1
ggtern(africa, aes(x = Sand, y = Silt, z = Clay)) +
  geom_density_tern()

# Plot 2
ggtern(africa, aes(x = Sand, y = Silt, z = Clay)) +
  stat_density_tern(geom = "polygon", aes(fill = ..level.., alpha = ..level..)) +
  guides(fill = guide_legend(show = F))

####################################
## for analyzing diagnostic plots

# linear modeling diagnostics
# - residuals v. fitted
# - normal Q-Q
# - scale-location
# - residuals v. leverage
# online vignette https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_lm.html

# for autoplotting lm diagnostic graphs
# Create linear model: res
res <- lm(Volume ~ Girth, data = trees)

# Plot res
plot(res) # this plots out 4 diagnostic graphs

# Import ggfortify and use autoplot()
library(ggfortify)
autoplot(res, ncol = 2)

# kmeans clustering

# Perform clustering
iris_k <- kmeans(iris[-5], 3)

# Autoplot: color according to cluster
autoplot(iris_k, data = iris, frame = TRUE)

# Autoplot: color according to species
autoplot(iris_k, data = iris, frame = TRUE, col = "Species")

### case studies

