# Need the rvest and dplyr libraries installed
require(rvest)
require(dplyr)

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
    select(Season, Pitch, Pitches, `O-Swing%`, `Zone%`, `SwStr%`, pVAL) %>%
    # remove the `%` symbols and makes the values numeric
    mutate(`O-Swing%` = as.numeric(gsub("%", "", `O-Swing%`)),
           `Zone%` = as.numeric(gsub("%", "", `Zone%`)),
           `SwStr%` = as.numeric(gsub("%", "", `SwStr%`)))
  
  return(pitchTable)
}

# 2018 Kershaw playerid = 2036

kershaw = moneyPitches(playerid = 2036)

# See output from function
kershaw


# Find out what working directory (save location) you are currently in
getwd()

# Save the file to a csv in your current working directory
write.csv(kershaw, file = "Kershaw Money Pitches 2018.csv", row.names = F)

