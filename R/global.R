# Read data globally
electionData <- fread("./inst/app/www/resources/Data/Elections_Data_Cleaned_V0.csv")
populationData <- fread("./inst/app/www/resources/Data/Population_Data_2020.csv")

# Pre-calculate state FIPS and vote percentages
electionData[, `:=`(
  state_fips = substr(FIPS, 1, nchar(FIPS) - 3),
  vote_percentage = (votes_for / votes_total) * 100
)]

# Join population
electionData[populationData, on = 'FIPS', Rural := i.Rural]

#Sort boundaries with new shapefiles
state_shapes <- readRDS("inst/app/www/states_simple.rds")
state_shapes <- state_shapes[order(state_shapes$state), ]

county_shapes <- readRDS("inst/app/www/counties_simple.rds")
county_shapes <- county_shapes[order(county_shapes$FIPS), ]