#' Downlaod LASS data
#'
#' Downlaod LASS data from \link{ftp://gpssensor.ddns.net:2121/}
#'
#' @param x numeric in {0, 1, 2,...}; the time of LASS data to be downloaded.
#' \describe{
#'   \item{0:}{Newest LASS data}
#'   \item{1:}{LASS data from 1 day before}
#'   \item{2:}{LASS data from 2 days before}
#'   \item{3:}{LASS data from 3 days before}
#'   }
#' @details The LASS data are dowload from Data.Taipe from the following link \link{ftp://gpssensor.ddns.net:2121/}
#' @return a list containing LASS data
#' @author Chih-Lin Wei
#' @export
#' @examples
#' lass <- read.LASS(x=0)

read.LASS <- function(x=0){

  # Get LASS data file names
  if(x==0) url <- c("ftp://gpssensor.ddns.net:2121/data.log") else {
    url<-c("ftp://gpssensor.ddns.net:2121/")
    fn <- getURL(url, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE)
    fn <- paste(url, strsplit(fn, "\r*\n")[[1]], sep = "")
    fn <- sort(fn[nchar(fn)==47], decreasing=TRUE)
    url <- fn[x]
  }

  # Read data and remove the first 8 lines
  dat <- scan(url, what="", sep="\n", skip=8)
  # Remove "LASS/Test/OpenData" sensor
  dat <- grep("LASS/Test/OpenData", dat, value = TRUE, invert=TRUE)
  # "LASS/Test/PM25 Variable names
  vn <- c("sensor","ver_format","fmt_opt","app","ver_app",
          "device_id","tick","date","time","device",
          "s_0","s_1","s_2","s_3","s_4",
          "s_d0","s_t0","s_h0","s_d1","gps_lat",
          "gps_lon","gps_fix","gps_num","gps_alt")

  split_fun <- function(x){
    obs <- strsplit(x, split="[|]")
    obs <- strsplit(obs[[1]], split="=")
    n <- unlist(lapply(obs, FUN=function(x)x[1]))
    out <- unlist(lapply(obs, FUN=function(x)x[2]))
    out[1] <- n[1]
    n[1] <- "sensor"
    out[match(vn, n)]
    }

  out <- ldply(lapply(dat, FUN=split_fun), rbind)
  names(out) <- vn
  for(i in c(2, 11:24)) out[,i] <- as.numeric(as.character(out[,i]))
  return(out)
}



