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
    

# The second value I'd like to find is the percentage of black inmates in the
# state of Florida. 

black_prison_pop_fl <- incarceration_trends %>%
  group_by(state) %>%
  filter(state == "FL") %>%
  filter(year == max(year)) %>%
  select(black_jail_pop) %>%
  summarize(sum(black_jail_pop)) %>%
  pull('sum(black_jail_pop)')

black_prison_pop_fl_percent <- (black_prison_pop_fl/54835) * 100


# The third value I'd like to find is the total number of black inmates in the
# state of Washington

wa_black_prison_pop <- incarceration_trends %>%
  group_by(state) %>%
  filter(state == "WA") %>%
  filter(year == max(year)) %>%
  select(black_jail_pop) %>%
  summarize(sum(black_jail_pop)) %>%
  pull('sum(black_jail_pop)')


# The fourth value I'd like to find is the total number of male adult 
# prisoners in Washington jailed from ICE. 

wa_ice_prison_pop <- incarceration_trends %>%
  group_by(state) %>%
  filter(state == "WA") %>%
  filter(year == max(year)) %>%
  select(total_jail_from_ice) %>%
  summarize(sum(total_jail_from_ice)) %>%
  pull('sum(total_jail_from_ice)')

# The fifth and final value I'd like to calculate is the total number 
# of black prisoners in Florida in 1999. 

year_1999_black_prison_pop_fl <- incarceration_trends %>%
  group_by(state) %>%
  filter(state == "FL") %>%
  filter(year == 1999) %>%
  select(black_jail_pop) %>%
  summarize(sum(black_jail_pop)) %>%
  pull('sum(black_jail_pop)')


# Trends Over Time Chart

wa_trends_over_time_df <- incarceration_trends %>%
  group_by(state) %>%
  filter(state == "WA") %>%
  select(year, black_jail_pop, white_jail_pop, total_jail_pop)



trends_over_time <- ggplot(wa_trends_over_time_df, aes(x = years, y = black_jail_pop, y = white_jail_pop)) +
  genom_line() + 
  genom_smooth() + 
  labs(
    x = "Years",
    y = "Jail Population",
    title = "Plot of the Jail Population of White VS. Black per year",
    subtitle = "Grouped by Team of Starters in the NBA"
  ))






