
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


# function to convert datetime from latestData column to iso8601 format
to_iso8601 <- function(datetime, offset) {
  # first check, if datetime value is actually in utc
  if (attr(datetime, "tzone") != "UTC") {
    stop("Datetime must be in UTC timezone.")
  # check if it is an integer value
  } else if (!is.integer(offset)) {
      if (round(offset) == offset) {
        # return the date in iso8601 format as a string
        return(
          iso8601(datetime + days(offset)) %>% 
            as.character() %>% 
            paste0(.,"Z")
        )
      }
      else {
        stop("Offset must be an integer.")
      }
  }
}


# function to transform the volumes from the traffic volume data to a dataframe
transform_volumes <- function(data) {
  return(
    data$trafficData$volume$byHour$edges %>% 
      # using a custom function to map the list within lists
      map_df(~ tibble(from = .x$node$from,
                      to = .x$node$to,
                      total = .x$node$total)) %>% 
      unnest_wider(total) %>% 
      mutate(from = as_datetime(from),
             to = as_datetime(to))
  )
}

