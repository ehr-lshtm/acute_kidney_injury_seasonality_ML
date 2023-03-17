stataweekdate <- function(year, week){
  (year - 1960) * 52 + week - 1
}

statawofd <- function(date){
  if(!is.Date(date)) stop('date should be a Date.')
  dateposix <- as.POSIXlt(date)
  dayofyear <- dateposix$yday
  week <- floor(dayofyear/7) + 1
  week[week %in% 53] <- 52
  year <- dateposix$year + 1900
  list(year=year, week=week)
}

isodate <- function (x = Sys.Date()) {
  xday <- ISOdate(year(x), month(x), day(x), tz = tz(x))
  dn <- 1 + (wday(x) + 5)%%7
  nth <- xday + ddays(4 - dn)
  jan1 <- ISOdate(year(nth), 1, 1, tz = tz(x))
  return(sprintf("%s/%02d", format(nth, "%y"), 1 + (nth - jan1)%/%ddays(7)))
}

helperFunction <- function(x){
  ifelse(is.na(x), "n","y")
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


myfun <- function(x) {
  if(is.numeric(x)){ 
    ifelse(is.na(x), x, paste0(round(x*100L, 2), "%")) 
  } else x 
}

everyother <- function(x) x[seq_along(x) %% 2 == 0]
