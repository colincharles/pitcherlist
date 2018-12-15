
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

# Combine the pitch leaderboards with the Fangraphs ID for scraping game logs
df = left_join(df, master[,c("fg_name","fg_id")], by = c("Name" = "fg_name")) %>%
  mutate(fg_id = as.numeric(as.character(fg_id)))



# create a function to calculate VRP & kVPR
VPR = function(fg_id){
  dat = NULL
  data = pitcher_game_logs_fg(playerid = fg_id, year = 2018) %>%
    filter(GS > 0)
  
  if(nrow(data)  == 0) next
  
  df1 = data.frame(Type = c("ES","NS","PS"), stringsAsFactors = F)
  df2 = data.frame(Type = c("kES","kNS","kPS"), stringsAsFactors = F)
  IP = mean(as.numeric(data$IP))
  
  vol1 = data %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
           ERA = as.numeric(ERA),
           IP = as.numeric(IP)) %>%
    mutate(Type = ifelse(ERA > 4.5, "PS", ifelse(ERA <= 3 & IP >=6, "ES", "NS"))) %>%
    group_by(Type) %>%
    tally() %>%
    full_join(., df1, by = "Type") %>%
    tidyr::spread(., key = Type, value = n, fill = 0) %>%
    mutate(VPR = round(ES/PS, 2),
           Volatility = round((sum(ES, PS)/sum(ES, NS, PS))*100, 2),
           Stability = round((sum(ES, NS)/sum(ES, NS, PS))*100, 2))
  
  
  vol2 = data %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
           IP = as.numeric(IP),
           H = as.numeric(H),
           BB = as.numeric(BB),
           WHIP = (H + BB)/IP,
           SO = as.numeric(SO)) %>%
    mutate(Type = ifelse(SO <= 4 | WHIP > 1.4 | SO >=9 & WHIP > 1.75, "kPS",
                         ifelse(SO >= 7 & WHIP <= 1.15 | SO >= 9 & WHIP <= 1.2 | SO >=5 & WHIP <= 0.75, "kES", "kNS"))) %>%
    group_by(Type) %>%
    tally() %>%
    full_join(., df2, by = "Type") %>%
    tidyr::spread(., key = Type, value = n, fill = 0) %>%
    mutate(kVPR = round(kES/kPS, 2),
           kVolatility = round((sum(kES, kPS)/sum(kES, kNS, kPS))*100,2),
           kStability = round((sum(kES, kNS)/sum(kES, kNS, kPS))*100,2))
  
  dat1 = data.frame(fg_id = fg_id,
                    IP = round(IP,1))
  
  dat1 = cbind(dat1, vol1, vol2)
  
  dat = rbind(dat, dat1)
  return(dat)
}

data.VPR = map_df(unique(df$fg_id), function(i){
  VPR(i)
})

data.VPR = right_join(master[,c("fg_name", "fg_id")], data.VPR) %>%
  rename(Name = fg_name) %>%
  select(-fg_id) %>%
  arrange(desc(VPR))
  
data.VPR
