#' rob_map_dezsex
#'
#' helper function for rob_map_*

rob_map_dezsex <- function(angle){
  angle_chr <- as.character(angle)
  deg <- as.numeric(strsplit(angle_chr, "\\.")[[1]][1])
  min <- as.numeric(strsplit(as.character((angle-deg)*60), "\\.")[[1]][1])
  sec <- (((angle-deg)*60) - min) * 60
  return(sec + min*60 + deg*3600)
}
