#' Getting the start and end dates for each season... this is a small table so it is 
#' not too crazy of a lift to execute at the beginning of a new season
#'
#' @param wiki_url trunk that comes after https://en.wikipedia.org/wiki/
#'
#' @return
#' @export
#'
#' @examples
get_season_episode <- function(wiki_url="List_of_RuPaul%27s_Drag_Race_episodes"){
  require(tidyverse)
  require(rvest)
  require(htmltools)
  require(janitor)
  require(glue)
  full_url <- glue::glue("https://en.wikipedia.org/wiki/{wiki_url}")
  
  ########### Season Table ##########
  season_start_to_end <-map_df(full_url, ~ { 
    .x %>% 
      read_html(full_url) %>% 
      html_node("table.wikitable") %>% 
      html_table(header=TRUE)
  }
  
  ) %>% 
    select(2,3,5,6)%>% 
    janitor::row_to_names(row=1)%>% 
    rename(  "season"="Season"
             , "episodes"="Episodes"
             , "season_start"="First aired"
             , "season_end"="Last aired") %>% 
    mutate_at(.vars=c("season_start","season_end")
              , str_remove
              ,"\\s\\(.*$" # escape(\\) character (() after all  (.*) to end ($) 
    ) %>% 
    mutate_at("episodes",as.double)
  ################ Episode Table ##############
  episode_table <-map_dfr(full_url, ~ { 
    .x %>% 
      read_html(full_url) %>% 
      html_nodes("table.wikitable") %>% 
      html_table(header=TRUE)
  }
  ) %>% 
    filter(!is.na(`No.overall`)) %>% 
    select(`No.overall`:`No. inseries`) %>%
    mutate(
      `Number in Season`=case_when(  is.na(`No. inseason`)~`No. inseries` # There was a change in naming to the page
                                     , TRUE~`No. inseason` 
      )
    ) %>%
    select(-c("No. inseason","No. inseries")) %>%  # removing columns that we wont need in the end
    rename("air_date"="Original air date") %>% 
    mutate_at("air_date"
              , str_remove  # removes the  date contained in the parens
              ,"\\s\\(.*$" # escape(\\) character (() after all  (.*) to end ($) 
    ) %>% 
    mutate_at( "Title"
               , str_replace
               , "\\[[^\\]]*\\]\\s*" # Need to figure out why this regex works...but it does!
               ,""
    ) 
  ########## Joined Together ##############
  episode_and_season <- episode_table %>% 
    left_join( season_start_to_end
               ,by=c("air_date"="season_start")
    ) %>%
    fill(season) %>%  # Default direction to fill is down
    mutate_at( "air_date"
               ,str_squish  # will need to figure out why this needs to be used... but that is for another day
    )
  to_return <- list(  "season_table"=season_start_to_end
                    , "episode_table"=episode_table
                    , "episode_and_season_table"=episode_and_season
                    )
  return(to_return)
}

get_views_and_ratings <- function(wiki_url="RuPaul%27s_Drag_Race_(season_", max_season=14){
  require(tidyverse)
  require(rvest)
  require(htmltools)
  require(janitor)
  require(glue)
  url_frame <- data.frame(season=seq(max_season)) %>% 
    mutate(
      season_url=glue("https://en.wikipedia.org/wiki/{wiki_url}{season})")
    )
  
  season_level_tables <- url_frame %>% 
    mutate(  
              season_tables=map(season_url, ~ { 
               # .x is the dataframe piped in 
               # and for each row of the frame the 
               # chain of functions is executed 
               # a cleaner way of getthing things 
               # than going down the loop route
               .x %>% 
                 read_html(season_url) %>% 
                 html_nodes("table.wikitable") %>% 
                 html_table(header=TRUE)  
             }
             )
    ) %>% 
    select(season, season_tables) %>% 
    unnest(season_tables) %>% 
    mutate(  season=as.character(season)
             , nested_col_names=map(season_tables, ~{
               .x %>% 
                 names()
             }
             )
             # Still need go get a better grasp on the whole mapping thing
             #, season_tables=map(season_tables ,~mutate(season_tables,season=season))
    )%>% # Season 3 has incomplete data no need to bring it in
    filter(str_detect(nested_col_names,"View"), season!=3)
  
  viewership <- bind_rows(split(season_level_tables$season_tables,season_level_tables$season )) %>% 
    select(  `Episode`=`No.`
             , `Title`
             , `air_date`=`Air date`
             , `Rating`=`Rating(18–49)`
             , `Viewers`=`Viewers(millions)`
    ) %>%
    # Extract the digits and then convert to doubles
    mutate_at( c("Rating","Viewers")
               , str_replace
               , "\\[[^\\]]*\\]\\s*" # Need to figure out why this regex works...but it does!
               ,""
    ) %>%  
    mutate_at(c("Rating","Viewers"), as.double) %>% 
    mutate(
      `Viewers`=`Viewers`*1000000
    )
  
  viewership
}
  
 get_seaon_episode_ratings <- function(){
   
   df_list <- get_season_episode()
   
   episode_and_season <- df_list$episode_and_season_table
   
   viewership <- get_views_and_ratings()
   
   episodes_with_ratings <- episode_and_season %>% 
     left_join( viewership
                , by=c("air_date","Title") 
     )
   
   episodes_with_ratings
 } 
  
  
  
  
  
  