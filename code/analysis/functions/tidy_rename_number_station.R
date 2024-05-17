# rename stations in the same city
# tag number for each station

tidy_rename_number_station <- function(df_raw) {
  df <- df_raw %>% 
    left_join(df_raw %>%
                distinct(id) %>%
                mutate(n = row_number()),
              by = "id") %>% 
    mutate(city = ifelse(n == 8, "Lexington (Station 1)", city)) %>% 
    mutate(city = ifelse(n == 16, "Las Vegas (Station 1)", city)) %>% 
    mutate(city = ifelse(n == 52, "Las Vegas (Station 2)", city)) %>% 
    mutate(city = ifelse(n == 12, "Oklahoma City (Station 1)", city)) %>% 
    mutate(city = ifelse(n == 13, "Oklahoma City (Station 2)", city)) %>% 
    mutate(city = ifelse(n == 49, "San Antonio", city)) %>% 
    mutate(city = ifelse(n == 14, "Tulsa (Station 1)", city)) %>% 
    mutate(city = ifelse(n == 1, "Tulsa (Station 2)", city)) %>% 
    mutate(city = ifelse(n == 36, "Monroeville", city))
  
  return(df)
}