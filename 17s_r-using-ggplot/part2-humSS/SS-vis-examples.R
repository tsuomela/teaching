####################################
# R script for social sciences and humanities vis demos
# 04/13/17
####################################

# 0 - loading libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(gapminder)
library(igraph)
library(geomnet)

#####################################
# social science examples
####################################

# 1 - gapminder data, using the r package

glimpse(gapminder)


# simple line plot for 5 countries
h_countries <- c("Egypt", "Haiti", "Romania", "Thailand", "Venezuela")
h_dat <- droplevels(subset(gapminder, country %in% h_countries))
h_dat$country <- with(h_dat, reorder(country, lifeExp, max))
ggplot(h_dat, aes(x = year, y = lifeExp)) +
  geom_line(aes(color = country)) +
  scale_colour_manual(values = country_colors) +
  guides(color = guide_legend(reverse = TRUE))

# spaghetti plot for lots of countries
ggplot(subset(gapminder, continent != "Oceania"),
       aes(x = year, y = lifeExp, group = country, color = country)) +
  geom_line(lwd = 1, show_guide = FALSE) + facet_wrap(~ continent) +
  scale_color_manual(values = country_colors) +
  theme_bw() + theme(strip.text = element_text(size = rel(1.1)))

# bubble plot for lots of countries
gap_bit <- subset(gapminder, year == 2007 & continent != "Oceania")
gap_bit <- gap_bit[with(gap_bit, order(continent, -1 * pop)), ]
ggplot(gap_bit, aes(x = gdpPercap, y = lifeExp, size = pop)) +
  scale_x_log10(limits = c(150, 115000)) + ylim(c(16, 96)) +
  geom_point(pch = 21, color = 'grey20', show.legend = F) +
  scale_size_area(max_size = 40) +
  facet_wrap(~ continent) + coord_fixed(ratio = 1/43) +
  aes(fill = country) + scale_fill_manual(values = country_colors) +
  theme_bw() + theme(strip.text = element_text(size = rel(1.1)))

# 2 - network analysis, example from Kieran Healy
# https://kieranhealy.org/blog/archives/2013/06/09/using-metadata-to-find-paul-revere/


data <- as.matrix(read.csv("../../github-repo/revere/data/PaulRevereAppD.csv",row.names=1))

person.net <- data %*% t(data)
group.net <- t(data) %*% data

diag(group.net) <- NA
diag(person.net) <- NA

person.g <- graph.adjacency(person.net,mode="undirected",
                            weighted=NULL, diag=FALSE)

group.g <- graph.adjacency(group.net, weighted=TRUE,
                           mode="undirected", diag=FALSE)

revere.g <- fortify(group.g)
revere.p <- fortify(person.g)

person2.net <- data %*% t(data)
diag(person2.net) <- NA
person2.g <- graph.adjacency(person2.net, weighted=TRUE,
                           mode="undirected", diag=FALSE)

revere.p.2 <- fortify(person2.g)

# from the geomnet github page

ggplot(data = revere.g,
  aes(from_id = from, to_id = to, linewidth = weight)) +
  geom_net(layout.alg = "fruchtermanreingold",
           labelon = TRUE, size = 1, labelcolour = 'black',
           ecolour = "grey70", repel = T) +
  theme_net()

ggplot(data = revere.p.2,
  aes(from_id = from, to_id = to, size = weight)) +
  geom_net(layout.alg = "fruchtermanreingold",
           labelon = revere.p.2$weight == 4,
           #aes(label = ifelse(..weight.. > 3, ..from_id.., NA)),
           linewidth = 0.01) +
  theme_net()


# 3 - timelines, using economic data

# Some examples using the economics data sets from the ggplot package
# note that there are two datasets : economics and economics_long
# variables
# pce = personal consumption expenditures
# pop = total population, in thousands
# psavert = personal savings rate
# uempmed = median duration of unemployment, in weeks
# unemploy = number of unemployed, in thousands

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
