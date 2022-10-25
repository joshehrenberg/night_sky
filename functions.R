#Create a vector of possible timezone inputs
timezones <- paste0("UTC", -12:-1) %>% append("UTC") %>% append(paste0("UTC+", 1:12))
#Because the tz variable in as.Date.POSIXct (the Olson database) takes strings starting with Etc
#and having the opposite sign as the conventionally written timezone
#this function that converts to the Olson name
to_OlsonName <- function(timezone) { 
  if(grepl("\\+", timezone)) {
    timezone <- sub("\\+", "-", timezone)
  }
  else if(grepl("-", timezone)) {
    timezone <- sub("-", "+", timezone)
  }
  timezone <- sub("UTC", "GMT", timezone)
  out_timezone <- paste0("Etc/", timezone)
  out_timezone
}
#simple helpers for converting to and from radians
rad_to_degree <- function(x) {(x * 360) / (2 * pi)}
degree_to_rad <- function(x) { (2 * pi * x / 360 ) }

# A wraper around astroFNs::ut2lst that will take a POSIXct object as date and time and a double as longitude
# rather than a bunch of strings
to_local_sidereal_time <- function(datetime, longitude) {
  #convert the POSIXct into a numeric vector of yr, mo, dy, hr, mi, se
  datetime_args <- format(datetime, format = "%Y-%m-%d-%H-%M-%S", tz = "UTC") %>%
    strsplit("-") %>%
    unlist() %>%
    as.double()
  #convert the longitude to format "dir d m s"
  long_arg <- degree_to_rad(longitude) %>%
    rad2dms()
  long_arg <- 
    if(grepl("\\+", long_arg)){
      sub("\\+", "E ", long_arg)
    }else if (grepl("-", long_arg)){
      sub("-", "W ", long_arg)
    }
  
  long_arg <- strsplit(long_arg, ":") %>%
    unlist()
  long_arg <- paste(paste0(long_arg[1], "d"), paste0(long_arg[2], "m"), paste0(long_arg[3], "s"))
  #Pass the arguments to ut2lst and convert to degrees
  local_sidereal_time <- ut2lst(datetime_args[1], datetime_args[2], datetime_args[3], datetime_args[4], datetime_args[5], datetime_args[6], long_arg) %>%
    hms2rad() %>%
    rad_to_degree()
  local_sidereal_time
}

#A function that takes elevation and azimuth and returns the x location on a stereographic projection
project_x <- function(elevation, azi) {
  x <- (sin(degree_to_rad(azi)) * cos(degree_to_rad(elevation)) / (sin(degree_to_rad(elevation)) + 1))
}

#A function that takes elevation and azimuth and returns the y location on a stereographic projection  
project_y <- function(elevation, azi) {
  (cos(degree_to_rad(azi)) * cos(degree_to_rad(elevation)) / (sin(degree_to_rad(elevation)) + 1))
}
