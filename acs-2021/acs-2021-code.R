##########################################################
# Part 1: The American Community Survey, R, and tidycensus
##########################################################
#install.packages(c("tidycensus", "tidyverse"))

#install.packages(c("mapview", "plotly", "ggiraph", 
 #                  "survey", "srvyr"))

library(tidycensus)

#census_api_key("c0be82e842802283abfef4028523f9a1522af019", install = TRUE,overwrite=TRUE)  #only run once,  overwrite if starting a new session, installs in the environment?

#get_acs is core function
median_income <- get_acs(
  geography = "county",  #'tract' for tract
  variables = "B19013_001",
  year = 2021
)

head(median_income)  #moe is margin of error on estimate 90% CI

median_income_1yr <- get_acs(
  geography = "county", # tidy census documentation lists all the geogrpahies available
  variables = "B19013_001",
  year = 2021,
  survey = "acs1" #<< #gets data from 1 year surveys, default is acs5. #1 year is just pops. of >65k
)

income_table <- get_acs(
  geography = "county", 
  table = "B19001", #<< here we use table, above we used a specific variable from the table (...._001)
  year = 2021
)

mn_income <- get_acs(
  geography = "county", 
  variables = "B19013_001", 
  state = "MN", #<< Specific state request
  year = 2021
)

vars <- load_variables(2021, "acs5")  #look up for the variables

View(vars)

age_sex_table <- get_acs(
  geography = "state", 
  table = "B01001", 
  year = 2021,
  survey = "acs1",
)

age_sex_table_wide <- get_acs(
  geography = "state", 
  table = "B01001", 
  year = 2021,
  survey = "acs1",
  output = "wide" #<< output for wide/rather than long
)


##Naming variables as in select/rename
ca_education <- get_acs(
  geography = "county",
  state = "CA",
  variables = c(percent_high_school = "DP02_0062P",
                percent_bachelors = "DP02_0065P",
                percent_graduate = "DP02_0066P"),
  year = 2021
)


#---Exercise
#pull some health insurance data
#'B18135_001'=AGE BY DISABILITY STATUS BY HEALTH INSURANCE COVERAGE STATUS, TOTAL
#'## ACS 5 year data dictionary https://www.socialexplorer.com/data/ACS2019_5yr
# Variable codes can mean different things in different years
# Thus get acs can only get 1 year at a time
# So use get_vars for specific years or get his book
# Try comparison profile for time series?
dhealth <- get_acs(
  geography = "tract",  #'tract' for tract
  variables = "B18135_001",
  year = 2015,
  state= 'CA'
)

############################################
# Part 2: Analyzing and visualizing ACS data
############################################
library(tidyverse)
arrange(median_income, estimate)

arrange(median_income, desc(estimate))

income_states_dc <- filter(median_income, !str_detect(NAME, "Puerto Rico"))
arrange(income_states_dc, estimate)

highest_incomes <- median_income %>%
  separate(NAME, into = c("county", "state"), sep = ", ") %>%
  group_by(state) %>%
  filter(estimate == max(estimate))

md_rent <- get_acs(
  geography = "county",
  variables = "B25031_001",
  state = "MD",
  year = 2021
)

ggplot(md_rent, aes(x = estimate, y = NAME)) + 
  geom_point()

md_plot <- ggplot(md_rent, aes(x = estimate, 
                               y = reorder(NAME, estimate))) +
  geom_point(color = "darkred", size = 2)

library(scales)
md_plot <- md_plot + 
  scale_x_continuous(labels = label_dollar()) +
  scale_y_discrete(labels = function(x) str_remove(x, " County, Maryland|, Maryland"))  #remove County and Maryland

md_plot <- md_plot + 
  labs(title = "Median gross rent, 2017-2021 ACS",
       subtitle = "Counties in Maryland",
       caption = "Data acquired with R and tidycensus",
       x = "ACS estimate",
       y = "") + 
  theme_minimal(base_size = 12)

md_rent %>%
  arrange(desc(estimate)) %>%
  slice(5:9)

md_plot_errorbar <- ggplot(md_rent, aes(x = estimate, 
                                        y = reorder(NAME, estimate))) + 
  geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe),
                width = 0.5, linewidth = 0.5) +
  geom_point(color = "darkred", size = 2) + 
  scale_x_continuous(labels = label_dollar()) + 
  scale_y_discrete(labels = function(x) str_remove(x, " County, Maryland|, Maryland")) + 
  labs(title = "Median gross rent, 2017-2021 ACS",
       subtitle = "Counties in Maryland",
       caption = "Data acquired with R and tidycensus. Error bars represent margin of error around estimates.",
       x = "ACS estimate",
       y = "") + 
  theme_minimal(base_size = 12)

library(plotly)
ggplotly(md_plot_errorbar, tooltip = "x")

library(ggiraph)
md_plot_ggiraph <- ggplot(md_rent, aes(x = estimate, 
                                       y = reorder(NAME, estimate),
                                       tooltip = estimate,
                                       data_id = GEOID)) +
  geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe), 
                width = 0.5, size = 0.5) + 
  geom_point_interactive(color = "darkred", size = 2) + #geom_point_interactive
  scale_x_continuous(labels = label_dollar()) + 
  scale_y_discrete(labels = function(x) str_remove(x, " County, Maryland|, Maryland")) + 
  labs(title = "Median gross rent, 2017-2021 ACS",
       subtitle = "Counties in Maryland",
       caption = "Data acquired with R and tidycensus. Error bars represent margin of error around estimates.",
       x = "ACS estimate",
       y = "") + 
  theme_minimal(base_size = 12)

girafe(ggobj = md_plot_ggiraph) %>%
  girafe_options(opts_hover(css = "fill:cyan;"))


library(htmlwidgets)
plotly_plot <- ggplotly(md_plot_errorbar, tooltip = "x")
saveWidget(plotly_plot, file = "md_plotly.html")


####################################
# Part 3: Working with ACS microdata
####################################
library(tidycensus)

## can also get at PUMA units
hi_pums <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "HI",
  survey = "acs1",
  year = 2021
)

library(tidyverse)
hi_age_39 <- filter(hi_pums, AGEP == 39)
print(sum(hi_pums$PWGTP))
print(sum(hi_age_39$PWGTP))

get_acs("state", "B01003_001", state = "HI", survey = "acs1", year = 2021)


View(pums_variables)

##Get recoded variables
hi_pums_recoded <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "HI",
  survey = "acs1",
  year = 2021,
  recode = TRUE
)

#--Filter on the server side
hi_pums_filtered <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "HI",
  survey = "acs5",
  variables_filter = list(
    SEX = 2,
    AGEP = 30:49
  ),
  year = 2021
)

library(tigris)
library(mapview)
options(tigris_use_cache = TRUE)
# Get the latest version of 2010 PUMAs
# 2021 ACS uses 2010 PUMAS
hi_pumas <- pumas(state = "HI", cb = TRUE, year = 2019)
hi_puma_map <- mapview(hi_pumas)

hi_age_by_puma <- get_pums(
  variables = c("PUMA", "AGEP"),
  state = "HI",
  survey = "acs5"
)

#-- Get weights by person or housing
hi_pums_replicate <- get_pums(
  variables = c("AGEP", "PUMA"),
  state = "HI",
  survey = "acs1",
  year = 2021,
  rep_weights = "person"
)

hi_survey <- to_survey(  #convert to survey object
  hi_pums_replicate,
  type = "person"  ##survey object will recognize survey weights
)

class(hi_survey)

library(srvyr)
hi_survey %>%
  filter(AGEP == 39) %>%
  survey_count() %>%
  mutate(n_moe = n_se * 1.645)

hi_survey %>%
  group_by(PUMA) %>%
  summarize(median_age = survey_median(AGEP)) %>%
  mutate(median_age_moe = median_age_se * 1.645)


##Compare with ACS
##Mostly matches the aggregates
hi_age_puma <- get_acs(
  geography = "puma",
  variables = "B01002_001",
  state = "HI",
  year = 2021,
  survey = "acs1"
)































