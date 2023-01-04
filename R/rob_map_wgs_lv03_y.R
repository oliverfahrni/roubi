#' rob_map_wgs_lv03_y
#'
#' rob_map_wgs_lv03_y computes the y value of lv03
#' Use in combination with rob_map_wgs_lv03_x
#'
#' For converting lv03 to wgs84 and vice-versa
#' Original: https://github.com/ValentinMinder/Swisstopo-WGS84-LV03/blob/master/scripts/r/WGS84_CH1903.R
#'
#' @param lat: input coordinate latitude
#' @param lon: input coordinate longitude
#' @return y value of lv03 coordinate
#' @export


rob_map_wgs_lv03_y <- function(lat, lon){
  lat <- rob_map_dezsex(lat)
  lon <- rob_map_dezsex(lon)
  lat_aux <- (lat - 169028.66)/10000
  lon_aux <- (lon - 26782.5)/10000
  y <- {600072.37 +
      211455.93 * lon_aux -
      10938.51 * lon_aux * lat_aux -
      0.36 * lon_aux * (lat_aux^2) -
      44.54 * (lon_aux^3)}
  return(y)
}

