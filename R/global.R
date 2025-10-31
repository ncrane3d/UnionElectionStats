#Datatable preparation
electionData <- fread("resources/Data/Elections_Data_Cleaned_V0.csv")
populationData <- fread("resources/Data/Population_Data_2020.csv")
electionData[populationData, on = 'FIPS', Rural := i.Rural][]
electionData[, vote_percentage := (votes_for / votes_total) * 100]

#Gather and trim shapefiles
stateBoundaries <- sf::read_sf("./inst/app/www/states.json")
countyBoundaries <- sf::read_sf("./inst/app/www/counties.json")