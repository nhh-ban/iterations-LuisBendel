# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

# Test if the column names from the dataframe that is given as an input
# are the columns that we want to have
test_stations_metadata_colnames <-
  function(df) {
    # this is the columnnames that we want
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
    #all 5 expected columns must be in the dataframe to pass the test
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  }

# We expect the number of rows for the given dataframe to be between 5000 and 10000
# Test is only passed if this is true for the input dataframe
test_stations_metadata_nrows <-
  function(df) {
    # expected boundaries for row numbers
    min_expected_rows <- 5000
    max_expected_rows <- 10000
    # test passed if nrows between the boundaries
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    # if lower than lower bound, then failed with suspiciously few rows  
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    # otherwise failed with suspiciously many rows
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }

# Checking if the function returns the dataframe with the correct column types
# all column types must be equal to the expected ones to pass the test
test_stations_metadata_coltypes <-
  function(df) {
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }

# check how many values in every column (together) are missing (NA)
# we expect to have at most 199 missing values in all columns together
# test if failed if there are more missing values because then we can assume
# that something is wrong with the function that outputs the dataframe
# or something is wrong with the input dataframe or both
test_stations_metadata_nmissing <-
  function(df) {
    max_miss_vals <- 200
    
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  }

# Test if the time zone attribute of the latestData Datetime-Column is UTC
test_stations_metadata_latestdata_timezone <-
  function(df) {
    
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }

# Running all tests at once using a single function that calls
# all other functions
test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }





