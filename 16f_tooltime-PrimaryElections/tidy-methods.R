# Tidy methods for the data work on primary project

# Data cleaning tasks
# Converting column names and county names to lower case.
names(pa.primaries) <- tolower(names(pa.primaries))
pa.primaries$county <- tolower(pa.primaries$county)

# This will give me all the counties, and the votes for each candidate as a separate observation.  So candidate could be turned into a factor.
library("tidyr")
pa.primaries %>%
  gather(candidate, votes, -1) # using -1 to ignore first column of data frame which contains the county or subdivision name
ls()

