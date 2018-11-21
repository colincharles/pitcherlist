
# You can install baseballr from github with:
# install.packages("devtools")
# devtools::install_github("BillPetti/baseballr")

require(rvest)
require(dplyr)
require(baseballr)

moneyPitches = function(playerid){
  # This is the base url for the Fangraphs pitching data 
  baseurl = "https://www.fangraphs.com/pitchfx.aspx?playerid="
  
  # This fills in the scraping URL.
  # Currently scrapes all pitches and only for 2018 season (can be changed
  # to include all seasons)
  scrapeURL = paste0(baseurl, playerid, "&position=P&pitch=all")
  
  pitchTable = read_html(scrapeURL) %>%
    html_nodes('.rgMasterTable') %>% 
    # Extract the raw text as a table
    html_table() %>%
    # Pull out the last table 
    .[[length(.)]] %>%
    # filter for 2018
    dplyr::filter(Season == 2018) %>%
    # Extract a few of the columns
    select(Season, Pitch, Pitches, `O-Swing%`, `Zone%`, `SwStr%`, pVAL) %>%
    # remove the `%` symbols and makes the values numeric
    mutate(`O-Swing%` = as.numeric(gsub("%", "", `O-Swing%`)),
           `Zone%` = as.numeric(gsub("%", "", `Zone%`)),
           `SwStr%` = as.numeric(gsub("%", "", `SwStr%`)))
  
  return(pitchTable)
}

# 2018 Kershaw playerid = 2036

kershaw = moneyPitches(playerid = 2036)

kershaw

# # Find out what working directory (save location) you are currently in
# getwd()
# 
# # Save the file to a csv in your current working directory
# write.csv(kershaw, file = "Kershaw Money Pitches 2018.csv", row.names = F)
# 

#####
#
### Scrape pitch leaderboards for pitchers with more than 50IP and extract 
### players who exceed money pitch criteria
#
##### 

url = "https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=50&type=8&season=2018&month=0&season1=2018&ind=0&team=&rost=&age=&filter=&players=&page=1_5000"

df = read_html(url) %>%
  html_nodes('.rgMasterTable') %>% 
  # Extract the raw text as a table
  html_table() %>%
  as.data.frame() %>%
  dplyr::slice(c(-1,-3))

colnames(df) = df[1,]

df = df %>%
  slice(-1) %>%
  mutate(first_name = sapply(strsplit(Name, split=' ', fixed=TRUE), function(x) (x[1])),
         last_name = sapply(strsplit(Name, split=' ', fixed=TRUE), function(x) (x[2])))

# Run this to initialize the Chadwick Bureau Register look up
playerid_lookup("Garcia", "Karim")


# initialize an empty dataframe
data = NULL
for(i in 1:nrow(df)){
  # Grab player_id using first and last names. Trevor Bauer
  # had 2 entries, but one had a bunch of NAs. Didn't look too much into it
  # but will do more checking later
  player = playerid_lookup(df$last_name[i], df$first_name[i]) %>%
    filter(!is.na(fangraphs_id))
  
  # grab Fangrpahs ID
  id = player$fangraphs_id
  
  # Extract pitch info and add in player name and ID for filtering
  player1 = moneyPitches(id)
  player1$Name = paste(df$first_name[i], df$last_name[i])
  player1$ID = id
  
  data = rbind(data, player1)
}

# Extract the pitches which exceed the "money pitch" criteria
# 40% O-swing, 40% Zone, 15% Swstr
data %>%
  dplyr::filter(`O-Swing%` > 40 & `Zone%` > 40 & `SwStr%` > 15) %>%
  as.data.frame()
