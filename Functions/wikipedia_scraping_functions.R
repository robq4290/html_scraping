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
    rename(  "season_number"="Season"
             , "season_start"="First aired"
             , "season_end"="Last aired"
             , "season_ep_count"="Episodes") %>% 
    mutate_at(.vars=c("season_start","season_end")
              , str_remove
              ,"\\s\\(.*$" 
              # Find white space followed by ( everything until the end of the string
    ) %>% 
    mutate_at("season_ep_count",as.double)
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
      episode_number=case_when(  is.na(`No. inseason`)~`No. inseries` # There was a change in naming to the page
                                     , TRUE~`No. inseason` 
      )
    ) %>%
    select(-c("No. inseason","No. inseries")) %>%  # removing columns that we wont need in the end
    rename(  "air_date"="Original air date"
             , "episode_title"="Title"
             , "overall_ep_number"="No.overall"
    ) %>% 
    mutate_at("air_date"
              , str_remove  # removes the  date contained in the parens
              ,"\\s\\(.*$" # escape(\\) character (() after all  (.*) to end ($) 
    ) %>% 
    mutate_at( "episode_title"
               , str_replace
               , "\\[.*\\]" 
               # \\[  Start at  [
               # .* everything 
               # \\] end at  ]
               ,""
    ) 
  ########## Joined Together ##############
  episode_and_season <- episode_table %>% 
    left_join( season_start_to_end
               ,by=c("air_date"="season_start")
    ) %>%
    fill(season_number) %>%  # Default direction to fill is down
    fill(season_end) %>% 
    mutate_at(  c("air_date","season_end")
               ,str_squish  # will need to figure out why this needs to be used... but that is for another day
    )%>% 
    mutate_at("season_end"
              , list(~case_when(.==air_date~1
                                , TRUE~0)
              )
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
               , "\\[.*\\]" 
               # \\[  Start at  [
               # .* everything 
               # \\] end at  ]
               ,""
    ) %>%  
    mutate_at(c("Rating","Viewers"), as.double) %>% 
    mutate(
      `Viewers`=`Viewers`*1000000
    )%>% 
    rename(  "episode_title"="Title"
             , "episode_number"="Episode"
             , "viewer_rating"="Rating"
             , "viewer_count"="Viewers"
    )
  
  viewership
}
  
 get_season_episode_ratings <- function(){
   
   df_list <- get_season_episode()
   
   episode_and_season <- df_list$episode_and_season_table
   
   viewership <- get_views_and_ratings()
   
   episodes_with_ratings <- episode_and_season %>% 
     left_join( viewership
                , by=c("air_date","episode_title","episode_number") 
     )%>% 
     select(  overall_ep_number
              , season_number
              , episode_number
              , episode_title
              , air_date
              , season_end
              , season_ep_count
              , viewer_rating
              , viewer_count
     )
   
   episodes_with_ratings
 } 
  
  
  
  
  
  