# Introduction + Summary Information

library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")
library("mapproj")
library("leaflet")
library("DT")
library("knitr")
library("stringr")
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


yearly_wa_black_white_jail_pop <- incarceration_trends %>%
  group_by(year) %>%
  filter(state == "WA") %>%
  summarize(sum(black_jail_pop), sum(white_jail_pop))


df_yearly_wa_black_white_jail_pop <- yearly_wa_black_white_jail_pop %>%
  select(year, 'sum(black_jail_pop)', 'sum(white_jail_pop)') %>%
  gather(key = "Key", value = "value", -year)


trends_over_time <- ggplot(df_yearly_wa_black_white_jail_pop, aes(x = year, y = value)) + 
  geom_line(aes(color = Key), size = 1) + 
  scale_color_manual(values = c("skyblue2", "darkorange1"),
                     labels = c("Black Jail Population", "White Jail Population")) +
  labs(
    x = "Years",
    y = "Jail Population",
    title = "The Prison Population Of White Vs. Black Inmates In Washington Over Years",
    subtitle = "Grouped By Race As Defined By The Key"
  )

trends_over_time  



# Testing 2

yearly_wa_jail_pop <- incarceration_trends %>%
  group_by(year) %>%
  filter(state == "WA") %>%
  summarize(sum(black_jail_pop), sum(white_jail_pop), sum(aapi_jail_pop), sum(latinx_jail_pop), sum(native_jail_pop))



df_yearly_wa_jail_pop <- yearly_wa_jail_pop %>%
  select(year, 'sum(black_jail_pop)', 'sum(white_jail_pop)', 'sum(aapi_jail_pop)', 'sum(latinx_jail_pop)', 'sum(native_jail_pop)') %>%
  gather(key = "Key", value = "value", -year)


testing_trends_over_time <- ggplot(df_yearly_wa_jail_pop, aes(x = year, y = value)) + 
  geom_line(aes(color = Key), size = 1) + 
  scale_color_manual(values = c("skyblue2", "darkorange1", "darkorchid", "darkolivegreen", "burlywood3"),
                     labels = c("Asian American & Pacific Islander Jail Population", "Black Jail Population", "Latinx Population", "Native Population", "White Population")) +
  labs(
    x = "Years",
    y = "Jail Population",
    title = "The Prison Population By Race Of Inmates In Washington Over Years",
    subtitle = "Grouped By Race As Defined By The Key"
  )

testing_trends_over_time  



# Variable Comparison Chart


adult_juvenile_male_jail_pop <- incarceration_trends %>%
  group_by(state) %>%
  summarize(adult_male_jail_pop = sum(male_adult_jail_pop, na.rm = TRUE), juvenile_male_jail_pop = sum(male_juvenile_jail_pop, na.rm = TRUE))


variable_comparison_chart <- ggplot(adult_juvenile_male_jail_pop, aes(x = adult_male_jail_pop, y = juvenile_male_jail_pop)) +
  geom_point(aes(color = state), size = 1) +
  labs(
    x = "Adult Male Jail Population",
    y = "Juvenile Male Jail Population", 
    title = "Total Juvenile Male Jail Population Vs. Adule Male Jail Population",
    subtitle = "Grouped by State"
  )

variable_comparison_chart

