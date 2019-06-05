
rm(list = ls())

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Usage example
packages<-c("ggplot2", "rvest", "purrr", "baseballr", "ggplot2")
check.packages(packages)

require(rvest)
require(dplyr)
require(purrr)
require(baseballr)
require(ggplot2)


# Load in modelling data
data = read.csv("~/PitcherList/Dan Richards/Modeling Data.csv")

data$HR.PA = data$HR/data$PA

# create the model to predict home runs using data from 2015-2018
mod = lm(HR.PA ~ PA + Brls.PA. + PULL.FB.LD + FB. + Soft. + xBA + LD. + K., data = data)

# chech out variance inflation factors
# car::vif(mod)

# data$pred = predict(mod, newdata = data)
# plot the predictions (if you want)
# ggplot(data, aes(HR, pred*PA)) + geom_point() + facet_wrap(~Season)


#####
#
### Start of Scraping
#
#####

# Loadin manually created ID Map (FanGraphs to MLBIDs)
IDs = read.csv("~/PitcherList/Dan Richards/ID map.csv")
IDs$playerid = as.character(IDs$playerid)
IDs$Name = as.character(IDs$Name)

# Check which players do not have an associated MLB ID (there are lots)
IDs %>% 
  dplyr::filter(is.na(MLBID)) %>% 
  dplyr::select(Name, playerid, MLBID)



# Scrape different Savant leaderboards and bind them together
url = paste0("https://baseballsavant.mlb.com/statcast_leaderboard?year=2019&abs=0&player_type=resp_batter_id","&csv=true")
statcast = readr::read_csv(url)

url = "https://baseballsavant.mlb.com/expected_statistics?type=batter&year=2019&position=&team=&min=1&csv=true"
statcast2 = readr::read_csv(url)

url = "https://baseballsavant.mlb.com/statcast_search/csv?hfPT=&hfAB=&hfBBT=fly%5C.%5C.ball%7Cline%5C.%5C.drive%7C&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=2019%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&hfFlag=&hfPull=Pull%7C&metric_1=&hfInn=&min_pitches=1&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_pas=0"
statcast3 = readr::read_csv(url, na = "null")


# Make sure the scrapes worked by checking a few different players

statcast %>% 
  dplyr::filter(first_name %in% c("Pete","Vladimir","Cesar"))

statcast2 %>% 
  dplyr::filter(first_name %in% c("Pete","Vladimir", "Cesar"))

statcast3 %>% 
  dplyr::filter(player_name %in% c("Pete Alonso","Vladimir Guerrero Jr.", "Cesar Puello"))

# bind the data together
statData = statcast %>% 
  left_join(., statcast2[,c("last_name", "player_id","est_ba","est_slg")]) %>% 
  dplyr::select(last_name, player_id, brl_pa, est_ba, fbld) %>% 
  left_join(., statcast3[,c("pitches","player_id")]) %>% 
  dplyr::rename(Times = pitches)


statData %>% 
  dplyr::filter(player_id %in% c(596129, 665489, 624413, 596105, 527049))



# Grab FanGraphs data from custom made table
url = "https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=10&type=c,6,35,3,44,45,206,209,11,43&season=2019&month=0&season1=2019&ind=0&team=0&rost=0&age=15,58&filter=&players=0&page=1_5000"

df = read_html(url) %>%
  html_nodes('.rgMasterTable') %>% 
  # Extract the raw text as a table
  html_table() %>%
  as.data.frame() %>%
  dplyr::slice(c(-1,-3))

colnames(df) = df[1,]

df = df %>%
  slice(-1) %>%
  dplyr::select(-`#`) %>%
  left_join(., IDs) 

# Find out which names in data do not have a matching MLBID
# Should be none, which may require tweaking as the season progresses
df$Name[is.na(df$MLBID)]

# Check some names
df %>% 
  dplyr::filter(Name %in% c("Peter Alonso","Daniel Vogelbach","Dwight Smith Jr."))

# Bind the FanGraphs data with the statcast data
df = left_join(df, statData,
               by = c("MLBID" = "player_id"))

# Check some players again
df %>% 
  dplyr::filter(Name %in% c("Peter Alonso","Daniel Vogelbach",
                            "Dwight Smith Jr.","Vladimir Guerrero Jr.",
                            "Cesar Puello", "Giancarlo Stanton"))

# If any values of barrels/PA or Times (pulled FB LD) is NA, set to 0
df$brl_pa = ifelse(is.na(df$brl_pa), 0, df$brl_pa)
df$Times = ifelse(is.na(df$Times), 0, df$Times)


# create a function to divide numbers by 100 to get decimal percentages
perc = function(x){
  return(x/100)
}

# Divide certain columns by 100 to get them to the right scale for the predictions
df1 = df %>%
  mutate_all(funs(gsub(" %", "", .))) %>%
  dplyr::select(-Team) %>% 
  mutate_at(vars(-c(Name)), list(as.numeric)) %>% 
  mutate_at(vars(-c(Name, PA, brl_pa, HR, est_ba,
                    Age, playerid, MLBID, Times, fbld)), list(perc)) %>% 
  mutate(Pull.LD.FB = Times/PA) %>% 
  dplyr::rename(K. = `K%`,
                GB. = `GB%`,
                FB. = `FB%`,
                Pull. = `Pull%`,
                Soft. = `Soft%`,
                xBA = est_ba,
                LD. = `LD%`,
                Brls.PA. = brl_pa,
                PULL.FB.LD = Pull.LD.FB) 


# Predict 2019 home runs using model created at start of script
df1$`pHR/PA` = predict(mod, newdata = df1)

# Model predict HR/PA, this will convert to HRs
df1 = df1 %>% 
  mutate(pHR = `pHR/PA`*PA) %>% 
  mutate(pHR = round(pHR, 1),
         Diff = pHR - HR) %>% 
  arrange(desc(pHR)) %>% 
  dplyr::select(Name, HR, pHR) 

# Check some players
df1 %>% 
  dplyr::filter(Name %in% c("Peter Alonso","Daniel Vogelbach",
                            "Dwight Smith Jr.","Vladimir Guerrero Jr."))

# See the top 10 pHR leaders in 2019
df1 %>% 
  arrange(desc(pHR)) %>% 
  head(n = 10)


# plot it if you like
# ggplot(df1, aes(HR, pHR)) + geom_point()

# Write a csv of the prediction data
# write.csv(df1, "~/PitcherList/Dan Richards/Predicted Home Runs.csv", row.names = F)
