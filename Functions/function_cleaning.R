library(tidyverse)
library(rvest)
library(htmltools)
library(janitor)
library(glue)
library(purrr)
library(htmltab)
# https://stackoverflow.com/questions/59543697/scrape-multiple-tables-from-wikipedia-in-r
# 
# Use this to clean up the table scraping... see if there is a pattern for each table 
# of interest
# 
# https://cran.r-project.org/web/packages/htmltab/vignettes/htmltab.html

season_13 <- "https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_13)"

n_tbls <- read_html(season_13) %>%   html_nodes(".wikitable") %>% length()

map(.x = 1:n_tbls,
    .f = function(mynum) {htmltab(doc = "https://en.wikipedia.org/wiki/RuPaul%27s_Drag_Race_(season_13)",
                                  which = mynum, rm_nodata_cols = F)}) -> res

Filter(function(x) any(names(x) %in% "Viewers(millions)"), res) -> out

# season_13_map <- map_df(season_13, ~ { 
#   .x %>% 
#     read_html(season_13) %>% 
#     html_nodes(".wikitable") %>% 
#     html_table(header=FALSE)
# }
# 
# ) 



# url <- "https://en.wikipedia.org/wiki/List_of_Bollywood_films_of_2019"
# tbls <- map2(url, 4:7, htmltab)
# tbls <- do.call(rbind, tbls)

# map(.x = 1:15,
#     .f = function(mynum) {htmltab(doc = "https://en.wikipedia.org/wiki/List_of_Bollywood_films_of_2019",
#                                   which = mynum, rm_nodata_cols = F)}) -> res
# 
# Filter(function(x) any(names(x) %in% "Opening"), res) -> out