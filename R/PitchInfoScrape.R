


PitchInfoScrape = function(fg_id){
  # This is the base url for the Fangraphs pitching data 
  baseurl = "https://www.fangraphs.com/statss.aspx?playerid="
  
  # This fills in the scraping URL.
  # Currently scrapes all pitches and only for 2018 season (can be changed
  # to include all seasons)
  scrapeURL = paste0(baseurl, fg_id, "&position=P")
  
  PitchValues = read_html(scrapeURL) %>%
    html_nodes('.rgMasterTable') %>% 
    # Extract the raw text as a table
    html_table() %>%
    # Pull out the last table 
    .[[12]] %>%
    dplyr::filter(Season == "2018") %>%
    slice(1) %>%
    dplyr::select(-Season, -Team) %>%
    tidyr::gather(., Pitch) %>%
    slice(1:7) %>%
    filter(complete.cases(.)) %>%
    mutate(Source = "Pitch Values",
           fg_id = fg_id)
    
  PitchInfo = read_html(scrapeURL) %>%
    html_nodes('.rgMasterTable') %>% 
    # Extract the raw text as a table
    html_table() %>%
    # Pull out the last table 
    .[[15]] %>%
    dplyr::filter(Season == "2018") %>%
    slice(1) %>%
    dplyr::select(-Season, -Team) %>%
    tidyr::gather(., Pitch) %>%
    filter(complete.cases(.)) %>%
    mutate(Source = "Pitch Info Pitch Values",
           fg_id = fg_id)
    

  pitchData = rbind(PitchValues, PitchInfo)
  
  return(pitchData)
}


xx = PitchInfoScrape(6345)


# Map of 2018 MLB Player Names and IDs
master = read.csv("http://crunchtimebaseball.com/master.csv") %>%
  mutate(fg_name = as.character(fg_name),
         fg_id = as.numeric(as.character(fg_id)))


df = read.csv("~/bb/pitchers50.csv", fileEncoding  = "UTF-8-BOM")

xx = map_df(unique(df$playerid), function(i){
  PitchInfoScrape(i)
})


yy = left_join(xx, master[,c("fg_name","fg_id")])

names(yy)[5] = "Name"
yy$fg_id = NULL



yy = yy %>%
  mutate(Pitch = ifelse(Pitch == "wFA", "wFB", Pitch)) %>%
  mutate(Pitch = ifelse(Pitch == "wFC", "wCT", Pitch)) %>%
  mutate(Pitch = ifelse(Pitch == "wFS", "wSF", Pitch)) %>%
  mutate(Pitch = ifelse(Pitch == "wCU", "wCB", Pitch)) %>%
  mutate(Pitch = ifelse(Pitch == "wSI", "wFB", Pitch))
  

yy %>%
  group_by(Source, Pitch, Name) %>%
  summarize(Value = sum(value)) %>%
  tidyr::spread(., Source, Value) %>%
  ggplot(., aes(`Pitch Info Pitch Values`, `Pitch Values`)) + 
  geom_point() + facet_wrap(~Pitch, scales = "free") + 
  geom_abline(aes(slope = 1, intercept = 0)) + theme_bw()


yy %>%
  group_by(Source, Pitch, Name) %>%
  summarize(Value = sum(value)) %>%
  tidyr::spread(., Source, Value) %>%
  mutate(Difference = `Pitch Info Pitch Values`- `Pitch Values`) %>%
  arrange(desc(Difference))
