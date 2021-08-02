library(dplyr)
library(tidyverse)

# Remove NA rows from county COVID data
us.counties <- us.counties %>% drop_na()

# Create dataframe of county names and fip codes
fips <- us.counties %>%
  select(state, county, fips) %>%
  distinct() %>%
  arrange(fips) %>%
  drop_na()

# Select couty population data for just 2020
county.pops <- county.pops %>%
  select(STATE,COUNTY,STNAME,CTYNAME,POPESTIMATE2020)

# Combine county COVID data with mask use data
joined <- mask.use.by.county %>%
  rename(fips = COUNTYFP) %>%
  right_join(us.counties) %>%
  drop_na()

# Plotting cumulative COVID deaths for single county
joined[joined$fips == 1001,] %>%
  ggplot(aes(date, deaths, group = 1)) +
  geom_line() +
  theme(axis.text.x=element_blank())

# Slimmed dataframe of county populations with column names changed to match us.counties dataframe
county.pops2 <- county.pops %>%
  select(STNAME,CTYNAME,POPESTIMATE2020) %>%
  `colnames<-`(c("state", "county", "population"))

county.pops <- county.pops[county.pops$COUNTY != 0,]

# Removing the words "County" and "Parish" from county names in county.pops2 dataframe to match us.counties dataframe
county.pops2$county <- county.pops2$county %>% str_remove(" County")
county.pops2$county <- county.pops2$county %>% str_remove(" Parish")

# Adding populations to covid.counties dataframe
covid.counties.pop <- merge(county.pops2, arrange(us.counties,fips), by=c("state","county"))
merge(county.pops2, us.counties, by=c("state","county"))

# Creating dataframe with cummulative death rate (deaths per 1000)
death_rate_df <- covid.counties.pop %>%
  mutate(deaths_per_thousand = deaths / (population/1000))

# Visualizing change in cumulative death rate for two different counties
death_rate_df[death_rate_df$fips == 1001 | death_rate_df$fips == 1003,] %>% 
  ggplot(aes(date, deaths_per_thousand, group=county, color=county)) + 
  geom_line()



# Adding combined "Never or Rarely" column to mask use data frame
mask.use.by.county <- mask.use.by.county %>% mutate(NR = NEVER + RARELY)

# Add group column
mask.use.by.county <- mask.use.by.county %>% add_column(group = NA)

# Stratifying counties by percentage of "Never" or "Really" respondents. 
# Group 1 is made of counties with less than 10% responding "Never" or "Rarely"; 
# Group 2 is made of counties with 10-20% responding "Never or Rarely", 
# etc.
mask.use.by.county$group[which(mask.use.by.county$NR < 0.1)] <- "less than 10%"
mask.use.by.county$group[which(mask.use.by.county$NR >= 0.1 & mask.use.by.county$NR < 0.2)] <- "between 10-20%"
mask.use.by.county$group[which(mask.use.by.county$NR >= 0.2 & mask.use.by.county$NR < 0.3)] <- "between 20-30%"
mask.use.by.county$group[which(mask.use.by.county$NR >= 0.3)] <- "more than 30%"

# Converting groups to factors:
mask.use.by.county$group <- factor(mask.use.by.county$group, levels = c("less than 10%","between 10-20%","between 20-30%","more than 30%"))

# Combine death rate and mask use data frames:
complete <- death_rate_df %>%
  rename(COUNTYFP = fips) %>%
  right_join(mask.use.by.county)

# Changing date to numeric:
complete$date <- as.numeric(complete$date)

# Cumulative death rate over time for each group of counties 
# (Based on percentage of "Never" or "Rarely" respondents):
complete %>% 
  group_by(date, group) %>% 
  summarize(deathrate = (sum(deaths)/(sum(population)/1000)))


# Graphing cumulative death rate by group over the course of the pandemic:
complete %>% 
  group_by(date, group) %>% 
  summarize(deathrate = (sum(deaths)/(sum(population)/1000))) %>%
  ggplot(aes(x=date,y=deathrate,col=group)) +
  geom_line() +
  labs(title = "Cumulative death rate for groups of counties subsetted by mask attitudes",
       x = "Days since beginning of pandemic", 
       y = "Cumulative deaths per 1000 people", 
       col = "Respondents saying \"Never\" or \"Rarely\"")



# Graphing cumulative death rate by group starting 300 days after pandemic start:
complete %>% 
  filter(date>300) %>%
  group_by(date, group) %>% 
  summarize(deathrate = (sum(deaths)/(sum(population)/1000))) %>%
  ggplot(aes(x=date,y=deathrate,col=group)) +
  geom_line() +
  labs(title = "Cumulative death rate for groups of counties subsetted by mask attitudes",
       x = "Days since beginning of pandemic", 
       y = "Cumulative deaths per 1000 people", 
       col = "Respondents saying \"Never\" or \"Rarely\"")

