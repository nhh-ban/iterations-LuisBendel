
# function to transform the metadata
transform_metadata_to_df <- function(metadata) {
  # returns a dataframe with the transformed data
  return(
    metadata[[1]] %>% 
      map(as_tibble) %>% 
      list_rbind() %>% 
      # why does it for me work with 1 as function and not with "volumeByHour" as function???
      mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>%
      # convert to utc time
      mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
      # unnest coordinates
      unnest_wider(location) %>% 
      unnest_wider(latLon)
  )
}




