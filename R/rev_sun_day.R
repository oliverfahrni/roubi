#' rev_sun_day
#'
#' The ouput of the function is a tibble given the suns every minute position and if there is direct sunlight (obstructed by mountains/hills). Currently does not work on arm because of elevatr which uses sf.
#'
#' @param location: need to be a vector = c(long, lat) according to WGS 84,
#' @param day_of_year is the # of day in the year, 1 = 1. January,
#' @param ray_length is radius of interest, arbitrary 10km (m)
#' @param ray_intervals is the distance between point on one ray to check, arbitrary 100m (m)
#' @param zoom_level is the accuracy of the raster to get elevation from a point. 15 is the highest but it is very slow.
#' @return tbl_sun_shadow is a tibble
#' @import tidyverse solrad elevatr rgdal
#' @export
rev_sun_day <- function(location, day_of_year, ray_length, ray_intervals, zoom_level){

  # # -------------------
  # # inputs for construction
  # # day of the year
  # day_of_year <- 60
  #
  # # specify radius of interest = raylength, arbitrary 10km
  # ray_length = 10000
  #
  # # specify number of points to check on an ray; interval = 100m
  # ray_intervals = 100
  #
  # # zoom level of get_aws_points()
  # zoom_level <- 12
  # # https://wiki.openstreetmap.org/wiki/Zoom_levels
  #
  # # example locations
  # # saas fee talstation alpin express
  # # location <- c(7.928178, 46.105742)
  #
  # # saas fee nördlicher dorfrand
  # # location <- c(7.929837, 46.115586)
  #
  # # leukerbad therme
  # location <- c(7.625979, 46.379161)
  # #
  # # # saas fee mitte
  # # # location <- c(7.926368, 46.108584)
  # #
  # # # zermatt
  # # location <- c(7.7494, 46.022227)
  # #
  # # # bern, waffenweg
  # # # location <- c(7.4519618997948855, 46.96146013632291)
  # #
  # # # schloss courgevaux
  # # # location <- c(7.111908, 46.905712)
  #


  # ------------------- Function

  # Location
  coord_0 <- location

  # day of the year
  # get sequence for each minute at a day
  day_0 <- day_of_year
  DOY <- seq(day_0, day_0+1-(1/1440), 1/1440)

  # altitude angle for each minute on day
  alpha <- as_tibble(solrad::Altitude(DOY, Lat = coord_0[2], Lon=coord_0[1], SLon=coord_0[1], DS=0))
  # plot(DOY, t(alpha))

  # azimuth angle for each minute on day
  azimuth <- as_tibble(solrad::Azimuth(DOY, Lat = coord_0[2], Lon=coord_0[1], SLon=coord_0[1], DS=0))
  # plot(DOY, t(azimuth))

  # specify number of points to check for one ray
  ray_points = ray_length/ray_intervals

  # define ratio km to degree (wgs84) for specific location
  long_1 <- 111300 * cos(coord_0[2])
  lat_1 <- 111300

  # some calibration
  prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  # get elevation of location
  # to get elevation --> needs to be a data frame
  coord_0_elev <- elevatr::get_aws_points(as.data.frame(t(coord_0)), prj = prj_dd, z = zoom_level)
  coord_0_elev <- coord_0_elev[[1]]$elevation


  # get "sun-rays" for each hour ("flat", only horizontal value)
  # range only from NO to NW (60 to 300)
  # rays are calculated for each degree --> 240 rays a day
  # horizon at alpha = 10degrees
  # thinkhelper: the ray gous from initial place outwards to the sun




  ray_start <- azimuth[1,] # angle from starting point to sun
  ray_end <- azimuth[nrow(azimuth),]
  rays = tibble(azimut = azimuth$value,
                alpha = alpha$value,
                long = coord_0[1],
                lat = coord_0[2]) %>%
    gather(key = "key",
           value = "value",
           3:4) %>%
    arrange(azimut) %>%
    mutate(azimut = (azimut + 180)*(pi/180),
           alpha = alpha*(pi/180),
           add = ifelse(key == "long",
                        -(sin(azimut)*ray_intervals)/long_1,
                        (cos(azimut)*ray_intervals)/lat_1)

    )

  aux <- as_tibble(matrix(0, nrow = 2*1440, ncol = ray_points))
  rays <- bind_cols(rays, aux)

  rays <- rays %>%
    gather(key = "point",
           value = "value2",
           6:ncol(rays)) %>%
    mutate(point = as.numeric(str_replace(point, "V", ""))) %>%
    arrange(azimut) %>%
    select(-value2) %>%
    group_by(azimut, key) %>%
    mutate(add2 = cumsum(add)) %>%
    ungroup() %>%
    mutate(point_coord = value + add2) %>%
    select(azimut, alpha, point, key, point_coord) %>%
    spread(key = key,
           value = point_coord) %>%
    arrange(azimut, point)


  rays_points <- rays %>%
    select(long, lat)
  rays_points <- as.data.frame(rays_points)

  aux <- elevatr::get_aws_points(rays_points, prj = prj_dd, z = zoom_level)
  rays_elev_points <- aux[[1]]$elevation
  rays_elev <- bind_cols(rays, as_tibble(rays_elev_points))

  # get inclination angle of sun alpha
  rays_sun_check <- rays_elev %>%
    mutate(elevation_diff = value - coord_0_elev,
           long_diff = long - coord_0[1],
           lat_diff = lat - coord_0[2],
           distance_horizontal = sqrt((long_diff*long_1)^2 + (lat_diff*lat_1)^2),
           a_tan = atan(elevation_diff/distance_horizontal),
           sun = ifelse(alpha < 0, 0, ifelse(alpha < a_tan, 0, 1)))

  # to check in google maps
  # write_excel_csv(rays_sun_check %>% mutate(index = paste(azimut, point)) %>% filter(between(azimut, 2.7, 3)) %>% select(lat, long, index), "test2.csv")
  # TODO check if shadow on ray for each min/azimuth


  rays_sun_min <- rays_sun_check %>%
    group_by(azimut) %>%
    summarise(alpha = min(alpha),
              sun_in_min = ifelse(min(sun) == 0, 0, 1))

  min <- as_tibble(seq(1:nrow(rays_sun_min)))
  time <- as_tibble(hms::as.hms(min$value*60))

  hour_major <- as_tibble(seq(1:11))
  time_hour_major <- c(hms::as.hms(hour_major$value*60*60*2))

  hour_minor <- as_tibble(seq(1:24))
  time_hour_minor <- c(hms::as.hms(hour_minor$value*60*60))


  rays_sun_min <- rays_sun_min %>%
    mutate(time = time$value,
           azimut = azimut * (180/pi),
           alpha = alpha *(180/pi)) %>%
    select(time, azimut, alpha, sun_in_min)

  return(rays_sun_min)

# sum(rays_sun_min$sun_in_min)
# # 211 zermatt
#   rm(min, time)
#   #
#   rays_sun_min %>% filter(alpha > 0) %>%
#     ggplot() +
#     geom_line(aes(x = time, y = sun_in_min), size = 2) +
#     geom_line(aes(x = time, y = alpha/30), size = 2, color = "red") +
#     labs(title = "Tagesverlauf der Sonne in Leukerbad Therme am 1. März", x = "Uhrzeit (Winterzeit)", y = "Schatten  -  Sonne (schwarz)") +
#     theme_minimal() +
#     # geom_hline(yintercept = 0.5) +
#     scale_x_time(breaks = time_hour_major,
#                  minor_breaks = time_hour_minor) +
#     scale_y_continuous(sec.axis = sec_axis(~ . *30, name = "Alpha (rot)"))

}
