source("utils.R")
# ?get_targeting
# get_targeting("4145976729", timeframe = "LAST_90_DAYS")
# debugonce(get_targeting)

library(httr)
install.packages("tidyverse")
library(tidyverse)

tstamp <- Sys.time()

source("utils.R")

internal_page_ids <- read_csv("https://raw.githubusercontent.com/favstats/ProvincialeStatenverkiezingen2023/main/data/nl_advertisers.csv") %>%
  mutate(page_id = as.character(page_id))


wtm_data <- read_csv("https://raw.githubusercontent.com/favstats/TK2023/main/data/wtm-advertisers-nl-2023-07-18T21_09_43.051Z.csv") %>% #names
  select(page_id = advertisers_platforms.advertiser_platform_ref,
         page_name = name, party = entities.short_name)  %>%
  mutate(page_id = as.character(page_id)) %>%
  # filter(party == "And") %>% #View
  # count(party, sort = T)  %>%
  mutate(party = case_when(
    str_detect(party, "VVD") ~ "VVD",
    str_detect(party, "\\bCDA\\b") ~ "CDA",
    str_detect(party, "PvdA|Jonge Socialisten") ~ "PvdA",
    str_detect(party, "D66|Jonge Democraten") ~ "D66",
    str_detect(party, "GroenLinks|GL") ~ "GroenLinks",
    str_detect(party, "ChristenUnie|CU") ~ "ChristenUnie",
    str_detect(party, "\\bSP\\b") ~ "SP",
    str_detect(party, "FvD|FVD|Forum voor Democratie") ~ "FvD",
    str_detect(party, "50Plus|50PLUS") ~ "50PLUS",
    str_detect(party, "\\bSGP\\b") ~ "SGP",
    str_detect(party, "PvdD|Partij voor de Dieren") ~ "PvdD",
    str_detect(party, "PVV") ~ "PVV",
    str_detect(party, "DENK") ~ "DENK",
    str_detect(party, "Volt|VOLT") ~ "Volt Nederland",
    str_detect(party, "BIJ1|BiJ") ~ "BIJ1",
    str_detect(party, "BVNL") ~ "BVNL",
    str_detect(party, "Ja21") ~ "JA21",
    str_detect(page_name, "Alliantie") ~ "Alliantie",
    str_detect(page_name, "Partij voor de Dieren") ~ "PvdD",
    str_detect(page_name, "Christine Govaert") ~ "BBB",
    str_detect(page_name, "BVNL|Belang van Nederland") ~ "BVNL",
    T ~ party
  ))

all_dat <- internal_page_ids %>%
  bind_rows(wtm_data) %>%
  distinct(page_id, .keep_all = T) %>%
  add_count(page_name, sort  =T) %>%
  mutate(remove_em = n >= 2 & str_ends(page_id, "0")) %>%
  filter(!remove_em) %>%
  # filter(n >= 2) %>%
  # filter(n >= 2 & str_ends(page_id, "0", negate = T)) %>%
  select(-n)  %>%
  mutate(party = case_when(
    str_detect(party, "VVD") ~ "VVD",
    str_detect(party, "\\bCDA\\b") ~ "CDA",
    str_detect(party, "PvdA|Jonge Socialisten") ~ "PvdA",
    str_detect(party, "D66|Jonge Democraten") ~ "D66",
    str_detect(party, "GroenLinks|GL") ~ "GroenLinks",
    str_detect(party, "ChristenUnie|CU") ~ "ChristenUnie",
    str_detect(party, "\\bSP\\b") ~ "SP",
    str_detect(party, "FvD|FVD|Forum voor Democratie") ~ "FvD",
    str_detect(party, "50Plus|50PLUS") ~ "50PLUS",
    str_detect(party, "\\bSGP\\b") ~ "SGP",
    str_detect(party, "PvdD|Partij voor de Dieren") ~ "PvdD",
    str_detect(party, "PVV") ~ "PVV",
    str_detect(party, "DENK") ~ "DENK",
    str_detect(party, "Volt|VOLT") ~ "Volt Nederland",
    str_detect(party, "BIJ1|BiJ") ~ "BIJ1",
    str_detect(party, "BVNL") ~ "BVNL",
    str_detect(party, "Ja21|JA21") ~ "Ja21",
    str_detect(party, "Alliantie") ~ "Alliantie",
    str_detect(party, "BBB") ~ "BBB",
    T ~ party
  ))


scraper <- function(.x) {
  
  print(paste0(.x$page_name,": ", round(which(internal_page_ids$page_id == .x$page_id)/nrow(internal_page_ids)*100, 2)))
  
  yo <- get_targeting(.x$page_id, timeframe = "LAST_7_DAYS") %>%
    mutate(tstamp = tstamp)
  
  print(nrow(yo))
  
  return(yo)
}

scraper <- possibly(scraper, otherwise = NULL, quiet = F)


### save seperately
enddat <- internal_page_ids %>% 
  arrange(desc(page_id)) %>%
  split(1:nrow(.)) %>%
  map_dfr(scraper)

ds <- enddat$ds[1]

current_date <- paste0("provincies/",  as.character(ds), "_7_2")

#dir.create(current_date)

saveRDS(enddat, file = paste0(current_date, ".rds"))