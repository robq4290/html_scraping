get_drag_family_data <- function(wiki_root_url){
  
  drag_house_url <- glue("{wiki_root_url}Category:Queens_by_Drag_House")
  
  drag_house_results <- read_html(drag_house_url)
  
  drag_house_names <- drag_house_results %>% 
    html_nodes(".category-page__member-link") %>% 
    html_text() 
  
  drag_house_urls <-  drag_house_results %>% 
    html_nodes(".category-page__member") %>% 
    html_text("href")%>% 
    str_replace_all( "[\t\n]" , "") %>% 
    str_replace_all(" ","_") %>% # cleaning up the URLs 
    as.data.frame() %>% 
    rename("url_end"=1)
  
  drag_house_frame <- tibble(wiki_root_url,drag_house_urls) %>% 
    mutate(family_url=as.character(glue("{wiki_root_url}{url_end}"))
           , family_name=str_replace(url_end,"Category:","")
           , family_name=str_replace_all(family_name,"_"," ")
    ) %>% 
    filter(!(url_end %in% c("Category:House_of_Stupi","Category:Template_(Family_Tree)"))
    ) %>% 
    mutate( html_results = map( family_url
                                , ~ {.x %>%
                                    read_html()
                                }
    )
    , all_names=map( html_results
                     , ~{.x %>% 
                         html_nodes(".category-page__member") %>% 
                         html_text() %>% 
                         str_replace_all( "[\t\n]" , "")
                     }
    )
    ) %>% 
    select(-html_results) %>% 
    unnest(all_names)  
  
  # Remove all scraping portions of the code and then create a cleaned up frame that 
  # can be used for creating a node graph
  # 
  # Add in flag to id if a queen is a member of more than one family
  # 
  # I will figure out better naming conventions when the frames are loaded into 
  # the in memory database
  drag_family_connections <- drag_house_frame %>% 
    select(family_name, all_names) %>% 
    group_by(all_names) %>% 
    mutate(  mutliple_families=n()
             , multiple_family_flg=case_when(mutliple_families>1~1,
                                             TRUE~0)
    )%>% 
    ungroup()%>%
    select(-mutliple_families)
  
  df_list_out <- list(  "drag_house_frame"=drag_house_frame
                      , "drag_family_connections"=drag_family_connections
                      )
  
  return(df_list_out)
}