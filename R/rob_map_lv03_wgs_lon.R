#' rob_map_lv03_wgs_lon
#'
#' rob_map_lv03_wgs_lon computes the longitude value of wgs84
#' Use in combination with rob_map_lv03_wgs_lat
#'
#' For converting lv03 to wgs84 and vice-versa
#  Original: https://github.com/ValentinMinder/Swisstopo-WGS84-LV03/blob/master/scripts/r/WGS84_CH1903.R
#'
#' @param y: input coordinate y of lv03
#' @param x: input coordinate x of lv03
#' @return longitude value of wgs84 coordinate
#' @export


rob_map_lv03_wgs_lon <- function (y, x){
  y_aux <- (y - 600000)/1000000
  x_aux <- (x - 200000)/1000000
  lat <- {16.9023892 +
      3.238272 * x_aux -
      0.270978 * (y_aux^2) -
      0.002528 * (x_aux^2) -
      0.0447   * (y_aux^2) * x_aux -
      0.0140   * (x_aux^3)}
  lat <- lat * 100/36
  return(lat)
}
