CHR_to_seconds <- function(x){
  a <- as.POSIXct(x, tz = '', format = "%H:%M:%S", usetz = FALSE)
  tms <- secondss(format(a, "%H:%M:%S"))
  s <- period_to_seconds(hms(tms))
  return(s)
}