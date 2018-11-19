
# IMake sure you have the pacman library downloaded and installed
require(pacman)

# Once pacman is installed, you can use the p_load functions with any other library and R will download
# install and load in the library into your current session. If the libraries are already downloaded, 
# then it will simply install them. Similar to running: require(dplyr)
p_load(rvest)
p_load(dplyr)

moneyPitches = function(playerid){
  # This is the base url for the Fangraphs pitching data 
  baseurl = "https://www.fangraphs.com/pitchfx.aspx?playerid="
  
  # This fills in the scraping URL.
  # Currently scapes all pitches and only for 2018 season (can be changed
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
    select(Season, Pitch, Pitches, `O-Swing%`, `Zone%`, `SwStr%`) %>%
    # remove the `%` symbols and makes the values numeric
    mutate(`O-Swing%` = as.numeric(gsub("%", "", `O-Swing%`)),
           `Zone%` = as.numeric(gsub("%", "", `Zone%`)),
           `SwStr%` = as.numeric(gsub("%", "", `SwStr%`)))
  
  return(pitchTable)
}

# 2018 Kershaw playerid = 2036

kershaw = moneyPitches(playerid = 2036)

kershaw
