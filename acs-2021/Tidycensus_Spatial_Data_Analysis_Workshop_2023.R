##Kyle Walker Workshop
##Spatial Data Analysis
##Annotations by Liz (2/15/23)

###############################################################
# Part 1: Working with "spatial" American Community Survey data
###############################################################

#Note: Make sure latest versions of R and R Studio are installed 
#Make sure packages are installed:

#install.packages(c("tidycensus", "tidyverse"))

#install.packages(c("mapview", "mapedit", "mapboxapi", 
                   "leafsync", "spdep", "segregation",
                   "ggiraph"))


library(tidycensus)

#Recommended to use this option:
options(tigris_use_cache = TRUE)

# You only have to do this part once (for example, I already have a key, so I get an error message if I try to rerun my key)
# census_api_key("YOUR KEY GOES HERE", install = TRUE)
# Note: The API key below is Liz's key. Each user should get their own key. :)
#census_api_key("21e547b1b7e07d5e3a8b7a71c22b4d86a889a660", install = TRUE)

texas_income <- get_acs(
  geography = "county",
  variables = "B19013_001",  #the variable for income
  state = "TX",
  year = 2021, #Remember that it defaults to 5-year ACS
  geometry = TRUE
)

#Peek at the data
#Note that it includes the estimate, margin of error, and geometry
texas_income

plot(texas_income["estimate"])
#Note in the map that there are counties with missing data.
#In this case, these are very small counties, so Census is keeping out data for confidentiality reasons

#See slides on "simple features" in R ("Looking under the hood" slide)
#See slide notes on sf package in R
#sf package foundational to tidycensus

#Interactive viewing tool
library(mapview)

#I cannot get mapview to run.
#zcol is used to create a shaded map automatically
mapview(texas_income, zcol = "estimate")

#to view all the variables in the ACS (browse data)
vars <- load_variables(2021, "acs5")

View(vars)

king_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "WA",
  county = "King",
  geometry = TRUE
)

king_income

mapview(king_income, zcol = "estimate")

orange_race <- get_acs(
  geography = "tract",
  variables = c(
    Hispanic = "DP05_0071P",
    White = "DP05_0077P",
    Black = "DP05_0078P",
    Asian = "DP05_0080P"
  ),
  state = "CA",
  county = "Orange",
  geometry = TRUE
)

orange_race_wide <- get_acs(
  geography = "tract",
  variables = c(
    Hispanic = "DP05_0071P",
    White = "DP05_0077P",
    Black = "DP05_0078P",
    Asian = "DP05_0080P"
  ),
  state = "CA",
  county = "Orange",
  geometry = TRUE,
  output = "wide" #<<
)


library(tigris)
library(sf)
sf_use_s2(FALSE)

#Erase water function can take out superfluous water.
king_erase <- erase_water(king_income, 
                          area_threshold = 0.9, 
                          year = 2021)

mapview(king_erase, zcol = "estimate")

##########################
# Part 2: Mapping ACS data
##########################

##Many packages in R being developed for cartographic visualization
##This workshop will use ggplot2
##Another package: tmap  (See chapter 6 of Kyle's book.)

library(tidyverse)

#Revisit object orange_race (percent of each racial/ethnic group for each tract)
orange_race

#Filter out just the values for Hispanic for each tract
orange_hispanic <- filter(orange_race, variable == "Hispanic")

#percent Hispanic (plus MOE) for OC census tracts
orange_hispanic

#Choropleth map
ggplot(orange_hispanic, aes(fill = estimate)) + 
  geom_sf()   #geom_sf function is drawing polygons

#Now, some customization
#Note: Color brewer palettes and viridis palettes are built into ggplot2
#Viridis palettes are colorblind safe
#He is using rocket palette from viridis palettes
ggplot(orange_hispanic, aes(fill = estimate)) + 
  geom_sf() + 
  theme_void() +   #theme_void strips out graticule, etc.
  scale_fill_viridis_c(option = "rocket") +     #Rocket is color palette
  labs(title = "Percent Hispanic by Census tract",
       subtitle = "Orange County, California",
       fill = "ACS estimate",
       caption = "2017-2021 ACS | tidycensus R package")

#Change the number of breaks in the color scale (in this case, not using continuous color palette, but using breaks/"binning" data)
#Binned choropleth
ggplot(orange_hispanic, aes(fill = estimate)) + 
  geom_sf() + 
  theme_void() + 
  scale_fill_viridis_b(option = "rocket", n.breaks = 6) + #ggplot2 identifies "pretty"/intuitive breaks; can specify breaks or calculate separtely and bring in
  labs(title = "Percent Hispanic by Census tract",
       subtitle = "Orange County, California",
       fill = "ACS estimate",
       caption = "2017-2021 ACS | tidycensus R package")

#Groupwise data approach/groupwise visualization (this is why KW loves long format)
#Using faceted plot here
#Now, we can see estimate for each of the 4 racial/ethnic groups for each tract in OC (pretty cool)
#Doesn't work quite as well if some groups aren't represented. 
#For example, here, the percent Black is pretty much 0 everywhere; may want to just subset the major groups
ggplot(orange_race, aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  theme_void() + 
  scale_fill_viridis_c(option = "rocket") + 
  facet_wrap(~variable) + #<<
  labs(title = "Race / ethnicity by Census tract",
       subtitle = "Orange County, California",
       fill = "ACS estimate (%)",
       caption = "2017-2021 ACS | tidycensus R package")

#Choropleths not the best for count data
#Looking at a count data example
#Graduated symbol map
orange_race_counts <- get_acs(
  geography = "tract",
  variables = c(
    Hispanic = "DP05_0071",
    White = "DP05_0077",
    Black = "DP05_0078",
    Asian = "DP05_0080"
  ),
  state = "CA",
  county = "Orange",
  geometry = TRUE
)

#Estimates are now counts instead of percentages
orange_race_counts

library(sf)

orange_black <- filter(
  orange_race_counts, 
  variable == "Black"
)

#st_centroid is function from sf package
#Converts the shape of a geographic unit (tract) to a point in the center of the unit (tract)
centroids <- st_centroid(orange_black)

ggplot() + 
  geom_sf(data = orange_black, color = "black", fill = "lightgrey") + #drawing polygons (circles)
  geom_sf(data = centroids, aes(size = estimate),      #Sizing according to estimate value
          alpha = 0.7, color = "navy") + 
  theme_void() + 
  labs(title = "Black population by Census tract",
       subtitle = "2017-2021 ACS, Orange County, California",
       size = "ACS estimate") + 
  scale_size_area(max_size = 6) #<< Can play around with this one; ggplot tries to make sizes proportional

#Dot density map
#At some point here he is shuffling the order of the dots (not sure exactly where)
orange_race_dots <- as_dot_density(
  orange_race_counts,
  value = "estimate",
  values_per_dot = 200,
  group = "variable"  
)

#Plot
ggplot() + 
  geom_sf(data = orange_black, color = "lightgrey", fill = "white") + 
  geom_sf(data = orange_race_dots, aes(color = variable), size = 0.01) + #<<
  scale_color_brewer(palette = "Set1") + 
  guides(color = guide_legend(override.aes = list(size = 3))) + #<<
  theme_void() + 
  labs(color = "Race / ethnicity",
       caption = "2017-2021 ACS | 1 dot = approximately 200 people")


#Linked interactive maps with mapview
library(viridisLite)

colors <- rocket(n = 100)

#This code is giving a warning
mapview(orange_hispanic, zcol = "estimate", 
        layer.name = "Percent Hispanic, 2017-2021 ACS",
        col.regions = colors)

library(leafsync)

orange_white <- filter(orange_race, variable == "White")

#Hispanic map (M1)
#Again, lots of warnings here:
m1 <- mapview(orange_hispanic, zcol = "estimate", 
              layer.name = "Percent Hispanic, 2017-2021 ACS",
              col.regions = colors)

#White map (M2)
#Warnings here:
m2 <- mapview(orange_white, zcol = "estimate", 
              layer.name = "Percent White, 2017-2021 ACS",
              col.regions = colors)

sync(m1, m2)   #sync function
#Would need to work on these legends; but does produce side-by-side maps

state_age <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2021,
  survey = "acs1",
  geometry = TRUE
)

mapview(state_age, zcol = "estimate",
        col.regions = plasma(7),
        layer.name = "Median age, 2021 ACS")

library(tigris)

#Using shift geometry
age_shifted <- shift_geometry(state_age)

ggplot(age_shifted, aes(fill = estimate)) + 
  geom_sf() + 
  scale_fill_viridis_c(option = "plasma") + 
  theme_void() + 
  labs(fill = "Median age \n2021 ACS")

#ggiraph-- interactive code
#Includes a method called geom_sf_interactive can make plots more interactive
library(ggiraph)

##Creating the map
gg <- ggplot(age_shifted, aes(fill = estimate, data_id = GEOID,
                              tooltip = estimate)) + 
  geom_sf_interactive() + #<<
  scale_fill_viridis_c(option = "plasma") + 
  theme_void() + 
  labs(fill = "Median age\n2021 ACS")

#making it interactive(?)
girafe(ggobj = gg) %>% #<<
  girafe_options(opts_hover(css = "fill:cyan;")) #<<

#Now, can click on each state to get values.


####################################################################
# Part 3: Applications: segregation, diversity, and spatial analysis
####################################################################

#Can use segregation package in R (reasonably new package)
#Note to self: Replicate and send to Sigrid
library(segregation)

#Using orange_race_counts object from above
orange_race_counts

#retaining only White and Hispanic rows in Orange county
#Using two pipe operators here
orange_race_counts %>%
  filter(variable %in% c("White", "Hispanic")) %>%  #Variable either White or Hispanic 
  dissimilarity(
    group = "variable",
    unit = "GEOID",        #This is tract
    weight = "estimate"   #Weight is the population value (the total number in each tract)
  )

#Value is 0.52 for this tract

#Now, looking at LA region counties (4-county area)
#First, get the race counts for all four racial/ethnic groups
la_race_counts <- get_acs(
  geography = "tract",
  variables = c(
    Hispanic = "DP05_0071",
    White = "DP05_0077",
    Black = "DP05_0078",
    Asian = "DP05_0080"
  ),
  state = "CA",
  county = c("Orange", "Los Angeles",           #Specifies grabbing multiple counties at once.
             "San Bernardino", "Riverside")
) %>%
  separate(NAME,                                   #Separate can parse into multiple columns based on a separator
           into = c("tract", "county", "state"),   #In this case, separating the NAME column into state county tract elements (there was typo here in his original code)
           sep = ", ")

la_race_counts

la_race_counts %>%
  filter(variable %in% c("White", "Hispanic")) %>%
  group_by(county) %>%
  group_modify(                 #group_modify takes a set of groups, passes it to a function that calculates that process over the set of groups; more advanced code
    ~dissimilarity(
      data = .x,
      group = "variable",
      unit = "GEOID",
      weight = "estimate"
    )
  )

#Can also calculate something called entropy
#A multigroup segregation measure (also considered a diversity measure)
#Calculates how groups are mixed together; evenness between multiple groups 
#total evenness is 1
#Note that this is a tract-level value
orange_entropy <- orange_race_counts %>%
  group_by(GEOID) %>%
  group_modify(~tibble(
    entropy = entropy(
      data = .x,
      group = "variable",
      weight = "estimate",
      base = 4                  #Why the value of 4? (four groups?)
    )
  ))

orange_entropy

#Getting ready to map

#Need to grab the tracts first
orange_tracts <- tracts("CA", "Orange", year = 2021, cb = TRUE)

#Joining the tracts with the entropy values (by the tract ID)
orange_diversity_geo <- left_join(orange_tracts, orange_entropy, by = "GEOID")

#Note that lots of values come in from tigris 
orange_diversity_geo

#Diversity map (tract entropy values) for orange county
ggplot(orange_diversity_geo, aes(fill = entropy)) + 
  geom_sf() + 
  scale_fill_viridis_c(option = "mako") + 
  theme_void() + 
  labs(fill = "Entropy index")

#####################################
#Final topic: Spatial analysis 
####################################

#Huge topic, just an intro.; 
#Note: I started getting lost here and so left the webinar at this point.

#First topic, identifying neighborhood "hot spots"
#Need to define a "neighborhood" 
#In this case, a tract, and what is nearby
#Needt to define what "nearby" means

#spdep is "workhorse" package in R for spatial analysis
library(spdep)

#Using "queen's case neighbors"
neighbors <- poly2nb(
  orange_diversity_geo, 
  queen = TRUE
)

weights <- nb2listw(neighbors)

G <- localG(
  orange_diversity_geo$entropy, 
  listw = weights
)

orange_localG <- orange_diversity_geo %>%
  mutate(localG = G, 
         Hotspot = case_when(
           localG >= 2.576 ~ "High cluster",
           localG <= -2.576 ~ "Low cluster",
           TRUE ~ "Not significant"
         ))

ggplot(orange_localG, aes(fill = Hotspot)) + 
  geom_sf(color = "grey90") + 
  scale_fill_manual(values = c("red", "blue", "grey")) + 
  theme_void()

### Mapedit / filter_by demo
library(mapedit)

shape <- drawFeatures()

shape_data <- get_acs(
  geography = "tract",
  variables = "", # Choose variable in workshop
  state = "", # Choose state(s) in workshop
  geometry = TRUE,
  filter_by = shape
)

mapview(shape_data, zcol = "estimate")

### mapboxapi / isochrone demo

# You'll need a Mapbox access token for this to work
library(mapboxapi)

mb_token <- "" # Show how to find in workshop

address <- "" # Enter an address in workshop

iso <- mb_isochrone(
  location = address,
  profile = "driving",
  time = 15,
  access_token = mb_token
)

iso_data <- get_acs(
  geography = "tract",
  variables = "", # Choose variable in workshop
  state = "", # Choose state(s) in workshop
  geometry = TRUE,
  filter_by = iso
)

mapview(iso_data, zcol = "estimate")





























