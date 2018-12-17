

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Usage example
packages<-c("ggplot2", "rvest", "purrr", "dplyr", "knitr", "kableExtra", "httr", "baseballr")
check.packages(packages)


require(rvest)
require(purrr)
require(baseballr)
require(dplyr)
require(rvest)
require(knitr)
require(kableExtra)
require(httr)


# Map of 2018 MLB Player Names and IDs
master = read.csv("http://crunchtimebaseball.com/master.csv") %>%
  mutate(fg_name = as.character(fg_name),
         fg_id = as.numeric(as.character(fg_id)))

# Fangraphs URL for pitch leaders 2018 - minimum 50IP
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
  mutate(GS = as.numeric(GS)) %>%
  filter(GS > 0)


df = left_join(df, master[,c("fg_name","fg_id")], by = c("Name" = "fg_name")) %>%
  mutate(fg_id = as.numeric(as.character(fg_id)))


PlateDiscipline = function(playerid){
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
  
  pitchTable$ID = playerid
  
  return(pitchTable)
}


# Extract some PD values for each player with 50 IP in 2018
data.PD = map_df(unique(df$fg_id), function(i){
  PlateDiscipline(i)
})


data.PD = left_join(data.PD, master[,c("fg_name","fg_id")], by = c("ID" = "fg_id"))

write.csv(data.PD, "2018 Plate Discipline Values.csv", row.names = F)


data.PD %>%
  group_by(Pitch) %>%
  summarize(Num_Pitchers = n(),
            `O-Swing%` = round(mean(`O-Swing%`, na.rm = T), 2),
            `Zone%` = round(mean(`Zone%`, na.rm = T), 2),
            `SwStr%` = round(mean(`SwStr%`, na.rm = T), 2),
            pVAL = round(mean(pVAL), 2)) %>%
  write.csv(., "Mean Pitch Discipline Values 2018.csv", row.names = F)




