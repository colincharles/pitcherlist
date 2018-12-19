
# You can install baseballr from github with:
# install.packages("devtools")
# devtools::install_github("BillPetti/baseballr")

require(rvest)
require(dplyr)
require(baseballr)
require(purrr)

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
  
  pitchTable$fg_id = playerid
  
  return(pitchTable)
}

# 2018 Kershaw playerid = 2036
kershaw = moneyPitches(playerid = 2036)
kershaw

#####
#
### Now apply the moneyPitches() function to multiple pitchers
#
#####

# Map of 2018 MLB Player Names and IDs
master = read.csv("http://crunchtimebaseball.com/master.csv") %>%
  mutate(fg_name = as.character(fg_name),
         fg_id = as.numeric(as.character(fg_id)))

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
         last_name = sapply(strsplit(Name, split=' ', fixed=TRUE), function(x) (x[2]))) %>%
  # Combine the pitch leaderboards with the Fangraphs ID for scraping game logs
  left_join(., master[,c("fg_name","fg_id")], by = c("Name" = "fg_name")) %>%
  mutate(fg_id = as.numeric(as.character(fg_id)))

                             
data = map_df(unique(df$fg_id), function(i){
  moneyPitches(i)
})
                                 
data = right_join(master[,c("fg_name", "fg_id")], data) %>%
  rename(Name = fg_name) %>%
  select(-fg_id)
  

data %>%
  dplyr::filter(`O-Swing%` > 40 & `Zone%` > 40 & `SwStr%` > 15) %>%
  as.data.frame()   
          
                            
 
