

require(baseballr)
require(dplyr)
require(ggplot2)

# Some useful functions for dealing with statcast data

# download data for a single pitcher from 2018
df = scrape_statcast_savant_pitcher(start_date = "2018-03-31", end_date = "2018-10-01", pitcherid = 592791)

# Tango Tiger defined barrels as: (calculation is in function below)
# The actual equation is this:
# where (launch_speed * 1.5 - launch_angle) >= 117
# and (launch_speed + launch_angle) >= 124
# and launch_angle <= 50
# and launch_speed >= 98
#http://tangotiger.com/index.php/site/comments/statcast-lab-barrels#2


# calculate *new* barrels and spray angle (I think I found spray angle from a Bill Petti post)
barrel_and_spray <- function(df) {
  df = df %>%
    mutate(barrel = ifelse(launch_speed * 1.5 - launch_angle >= 117 &
                             launch_speed + launch_angle >= 124 &
                             launch_angle <= 50 & launch_speed >= 98, 1, 0),
           hc_x = as.numeric(hc_x),
           hc_y = as.numeric(hc_y),
           spray_angle = round((atan((hc_x-125.42)/(198.27-hc_y))*180/pi*0.75), 1))
  return(df)
}


df = barrel_and_spray(df)

