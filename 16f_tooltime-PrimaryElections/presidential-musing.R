# Loading and checking data
pres.data <- read.csv("~/Repositories/github-repo/election_2016_data/data/presidential_general_election_2016_by_county.csv", header = TRUE)
pa.data <- filter(pres.data, state == "Pennsylvania")
head(pa.data)
names(pa.data)
str(pa.data)

# Summary functions for votes by candidate and geo_name (county)
summarise(group_by(pa.data, name), sum(votes))
summarise(group_by(pa.data, geo_name), sum(votes))

# Manipulating data, subsetting to drop variables, and keep fewer
pa.data <- subset(pa.data, select = c(fips, geo_name, name, votes))

# Spreading to put variables into columns
wide.pa <- spread(pa.data, name, votes)
pa.data <- subset(pa.data, select = c(fips, geo_name, name, votes))
wide.pa$TrumpMargin <- wide.pa$`D. Trump` - wide.pa$`H. Clinton`
boxplot(wide.pa$TrumpMargin)


# working with ACS
csa.local <- geo.make(csa = "526")
acs.lookup(2012, keyword = "race")
acs.lookup(2012, table.number = "B02001")
demo.local <- acs.fetch(2012, geography = csa.local, table.number = "B02001")
plot(demo.local)
api.key.install("917b9aa1ed404ad793cdef1530effee83b3aee8c")
