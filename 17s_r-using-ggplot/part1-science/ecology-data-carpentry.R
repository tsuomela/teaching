# examples from the ecology dataset found in the Data Carpentry ecology workshop
# http://www.datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html

# ideas for looking at this data
# 1. divide months seasons
# 2. 

# get data from figshare
download.file("https://ndownloader.figshare.com/files/2292169", "data/portal_data_joined.csv")
surveys <- read.csv('data/portal_data_joined.csv')

# basic summaries and structurehead(surveys)
dim(surveys)
nrow(surveys)
ncol(surveys)
names(surveys)
levels(sex)

# load packages
library(dplyr)
library(ggplot2)
library(hexbin)

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

yearly_sex_counts <- surveys_complete %>%
  group_by(year, species_id, sex) %>%
  tally

yearly_sex_weight <- surveys_complete %>%
  group_by(year, sex, species_id) %>%
  summarize(avg_weight = mean(weight))

ggplot(data = yearly_sex_weight, aes(x=year, y=avg_weight, color = species_id, group = species_id)) +
  geom_line() +
  facet_grid(sex ~ .)

# cleaning up

ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex, group = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = 'Observed species in time',
       x = 'Year of observation',
       y = 'Number of species') +
  theme_bw()

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
