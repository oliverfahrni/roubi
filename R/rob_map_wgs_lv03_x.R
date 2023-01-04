#' rob_map_wgs_lv03_x
#'
#' rob_map_wgs_lv03_x computes the x value of lv03
#' Use in combination with rob_map_wgs_lv03_y
#'
#' For converting lv03 to wgs84 and vice-versa
#' Original: https://github.com/ValentinMinder/Swisstopo-WGS84-LV03/blob/master/scripts/r/WGS84_CH1903.R
#'
#' @param lat: input coordinate latitude
#' @param lon: input coordinate longitude
#' @return x value of lv03 coordinate
#' @export


rob_map_wgs_lv03_x <- function(lat, lon){
  lat <- dec_to_sex(lat)
  lon <- dec_to_sex(lon)
  lat_aux <- (lat - 169028.66)/10000
  lon_aux <- (lon - 26782.5)/10000
  x <- {200147.07 +
      308807.95 * lat_aux +
      3745.25 * (lon_aux^2) +
      76.63 * (lat_aux^2) -
      194.56 * (lon_aux^2) * lat_aux +
      119.79 * (lat_aux^3)}
  return(x)
}
