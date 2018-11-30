
#####
#
### If you don't have baseballr already installed, you will need to run the following lines to install it
#
#####
# install.packages("devtools")
devtools::install_github("BillPetti/baseballr")

# Load in packages

require(baseballr)
require(dplyr)


# download statcast data in August 2018
data = scrape_statcast_savant("2018-08-01", "2018-08-31")


data %>%
  # remove foul balls and pitches with no launch speed (balls & strikes)
  filter(description != "foul" & !is.na(launch_speed)) %>%
  # group by each player and them find the mean EV and number of batted balls
  group_by(player_name) %>%
  summarize(EV = mean(launch_speed),
            N = n()) %>%
  # remove all players with less than 20 recorded batted ball events in August
  filter(N >= 20) %>%
  # look at the top 10
  arrange(desc(EV)) %>%
  print(n = 10)


# Now since that above code will pull out all data, we might be more interested in
# the mean EV for different types of contact
# Ground ball: < 10 degrees
# Line Drive: 10-25 degrees 
# Fly ball: 25-50 degrees
# Pop up: >50 degrees

data %>%
  # remove foul balls and pitches with no launch speed (balls & strikes)
  filter(description != "foul" & !is.na(launch_speed)) %>%
  mutate(Type = ifelse(launch_angle < 10, "Ground Ball", NA)) %>%
  mutate(Type = ifelse(launch_angle >= 10 & launch_angle < 25, "Line Drive", Type)) %>%
  mutate(Type = ifelse(launch_angle > 25 & launch_angle <= 50, "Fly Ball", Type)) %>%
  mutate(Type = ifelse(launch_angle > 50, "Pop up", Type)) %>%
  filter(Type == "Line Drive") %>%
  group_by(player_name) %>%
  summarize(`Line Drive EV (mph)` = round(mean(launch_speed), 1),
            Number = n()) %>%
  arrange(desc(`Line Drive EV (mph)`)) %>%
  filter(Number >= 5) %>%
  slice(1:10) %>%
  as.data.frame()



#####
#
### Find out which plyers had the 10 hardest hit balls in August
#
#####


data %>%
  # remove foul balls and pitches with no launch speed (balls & strikes)
  filter(description != "foul" & !is.na(launch_speed)) %>%
  # group by each player and them find the mean EV and number of batted balls
  group_by(player_name) %>%
  top_n(n = 10, wt = launch_speed) %>%
  summarize(EV = mean(launch_speed),
            N = n()) %>%
  # this will remove any player who did not record 10 batted balls in August
  filter(N == 10) %>%
  # look at the top 10
  arrange(desc(EV)) %>%
  slice(1:10)



#####
#
### Find out which plyers had the 10 weakest hit balls in August
#
#####

data %>%
  # remove foul balls and pitches with no launch speed (balls & strikes)
  filter(description != "foul" & !is.na(launch_speed)) %>%
  # group by each player and them find the mean EV and number of batted balls
  group_by(player_name) %>%
  top_n(n = -10, wt = launch_speed) %>%
  summarize(EV = mean(launch_speed),
            N = n()) %>%
  filter(N == 10) %>%
  # look at the top 10
  arrange(EV) %>%
  slice(1:10)
