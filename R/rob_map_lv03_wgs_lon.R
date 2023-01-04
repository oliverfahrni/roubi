#' rob_map_lv03_wgs_lon
#'
#' rob_map_lv03_wgs_lon computes the longitude value of wgs84
#' Use in combination with rob_map_lv03_wgs_lat
#'
#' For converting lv03 to wgs84 and vice-versa
#  Original: https://github.com/ValentinMinder/Swisstopo-WGS84-LV03/blob/master/scripts/r/WGS84_CH1903.R
#'
#' @param x: input coordinate x of lv03
#' @param y: input coordinate y of lv03
#' @return longitude value of wgs84 coordinate
#' @export


rob_map_lv03_wgs_lon <- function (x, y){
  x_aux <- (x - 600000)/1000000
  y_aux <- (y - 200000)/1000000
  lon <- {2.6779094 +
      4.728982 * x_aux +
      0.791484 * x_aux * y_aux +
      0.1306   * x_aux * (y_aux^2) -
      0.0436   * (x_aux^3)}
  lon <- lon * 100/36
  return(lon)
}
