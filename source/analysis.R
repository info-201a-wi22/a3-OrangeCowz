# Introduction + Summary Information

library("dplyr")
library("tidyverse")
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


# The first value is the state with the largest total population of prisoners 
# defined as "Total prison population count is based on the number of people 
# held in prison on December 31 of a given year". 

total_prison_pop_state <- incarceration_trends %>%
  group_by(state) %>%
  filter(year == max(year)) %>%
  summarize(sum(total_jail_pop))

total_prison_pop_state <- total_prison_pop_state %>%
  rename(total_jail_pop = `sum(total_jail_pop)`)

print(total_prison_pop_state)

largest_total_prison_pop_state <- total_prison_pop_state %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  pull(state)
    
# The second value I'd like to find is the total number of black inmates in the state of Washington

black_prison_pop_wa <- incarceration_trends %>%
  group_by(state = WA) %>%
  filter(year == 2017) %>%
  summarize(sum(total_jail_pop))