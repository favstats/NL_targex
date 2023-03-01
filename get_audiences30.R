source("utils.R")
# ?get_targeting
# get_targeting("41459763029", timeframe = "LAST_90_DAYS")
# debugonce(get_targeting)

library(httr)
install.packages("tidyverse")
library(tidyverse)

tstamp <- Sys.time()

source("utils.R")

internal_page_ids <- read_csv("data/nl_advertisers.csv") %>%
    mutate(page_id = as.character(page_id))

scraper <- function(.x) {

    print(paste0(.x$page_name,": ", round(which(internal_page_ids$page_id == .x$page_id)/nrow(internal_page_ids)*100, 2)))

    yo <- get_targeting(.x$page_id, timeframe = "LAST_30_DAYS") %>%
        mutate(tstamp = tstamp)

    print(nrow(yo))

    return(yo)
}

scraper <- possibly(scraper, otherwise = NULL, quiet = F)


### save seperately
yo <- internal_page_ids %>% #count(cntry, sort  =T) %>%
    # filter(!(page_id %in% already_there)) %>%
    # filter(cntry == "GB") %>%
    # slice(1:10) %>%
    split(1:nrow(.)) %>%
    map_dfr(scraper)

current_date <- paste0("provincies/",  as.character(Sys.Date()), "_30")

#dir.create(current_date)

saveRDS(yo, file = paste0(current_date, ".rds"))
