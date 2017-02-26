#' Downlaod AirBox data
#'
#' Downlaod AirBox data from Data.Taipei
#'
#' @param x numeric in {0, 1}; the type of AirBoxData to be downloaded.
#' \describe{
#'   \item{0:}{hourly instant AirBox data}
#'   \item{1:}{historical 7-day AirBox data}
#'   }
#' @details The airbox data are dowload from Data.Taipe from the following link \link{http://data.taipei/opendata/datalist/datasetMeta?oid=4ba06157-3854-4111-9383-3b8a188c962a}
#' @return a list containing AirBox data and device information
#' @author Chih-Lin Wei
#' @export
#' @examples
#'
#' # Download hourly instant AirBox data
#' system.time(d <-read.AirBox(0))
#'
#' # 7 day average of PM 2.5 at elementary school
#'
#' library(doBy)
#' library(ggplot2)
#' avg <- summaryBy(s_d0~school, data=d, FUN=c(mean, sd))
#' avg <- na.omit(avg)
#' od <- order(avg$s_d0.mean, decreasing=TRUE)
#' avg$school <- factor(avg$school, levels=avg$school[od])
#'
#' Sys.setlocale(category = "LC_ALL", locale = "cht")
#'
#' # Air quality ranks for elementary school in Taipei
#' ggplot(avg, aes(x=school, y=s_d0.mean)) +
#'   geom_point(stat = "identity")+
#'   geom_errorbar(aes(x=school, ymin=s_d0.mean-s_d0.sd, ymax=s_d0.mean+s_d0.sd))+
#'   xlab("")+ylab("PM 2.5")+
#'   coord_flip()+
#'   theme_bw()
#'
#' # Time series of top 5 most polluted elementary school
#' sub <- subset(d, school==levels(avg$school)[1] | school==levels(avg$school)[2] | school==levels(avg$school)[3]| school==levels(avg$school)[4]| school==levels(avg$school)[5])
#' ggplot(data=sub, aes(x=time, y=s_d0, colour=school))+
#'   geom_line()+
#'   ylab("PM 2.5")+
#'   theme_bw()+
#'   theme(axis.text.x = element_text(angle = 90, hjust = 1))

read.AirBox <- function(x=0){

  # Import AirBOX Data
  ser <- "https://tpairbox.blob.core.windows.net/blobfs"
  if(x==0) fn <- "AirBoxData.gz" else{fn <- "AirBoxData_history.gz"}
  url <- paste(ser, fn, sep="/")
  dst <- paste(getwd(), fn, sep="/")
  download.file(url, dst, mode="wb")
  d <- scan(dst, what="", sep="{")
  # Remove unecessary symbols
  d <- gsub("}]}", "", gsub("},", "", d[-1:-2]))

  split_fun <- function(x){
    out <- strsplit(x, split=",")
    out <- c(gsub(".*time:","",out[[1]][1]), gsub(".*:", "", out[[1]][-1]))
    return(out)
  }

  # Arrange data frame structure
  da <- ldply(lapply(d, FUN=split_fun))
  # keep variable names before ":"
  names(da) <- gsub("\\:.*", "", strsplit(d[1], split=",")[[1]])
  for(i in c(3:8)) da[,i] <- as.numeric(da[,i])
  # Set the time format
  da$time <- as.POSIXct(da$time, format="%Y-%m-%d %H:%M:%S", tz="Asia/Taipei")
  file.remove(dst)

  # Import AriBox device data
  dev_file <- "AirBoxDevice.gz"
  url <- paste(ser, dev_file, sep="/")
  dst <- paste(getwd(), dev_file, sep="/")
  download.file(url, dst, mode="wb")
  d <- scan(dst, what="", sep="{")
  # Remove unecessary symbols
  d <- gsub("}]}", "", gsub("},", "", d[-1:-2]))
  # Variable names
  na0 <- gsub("\\:.*", "", strsplit(d[1], split=",")[[1]])

  split_fun <- function(x){
    # Separate each data entry
    out <- strsplit(x, split=",")
    # Variable names
    na <- gsub(":.*", "", out[[1]])
    # Keep values after ":
    out <- gsub(".*:", "", out[[1]])
    names(out) <- na
    # Match variable names
    out <- out[na0]
    return(out)
  }

  # Arrange data frame structure
  de <- ldply(lapply(d, FUN=split_fun))
  for(i in c(2:4, 8:12)) de[,i] <- as.numeric(de[,i])
  file.remove(dst)

  # Import school ID data
  url <- "http://data.taipei/opendata/datalist/datasetMeta/download?id=4ba06157-3854-4111-9383-3b8a188c962a&rid=121311db-55f0-4bf3-908c-5456d8491d43"
  sc <- read.table(url, sep=",", header=TRUE, stringsAsFactors=FALSE)
  names(sc) <- c("device_id", "school", "airbox.taipei")

  # Combine AirBox data, device information and school ID data
  de <- de[match(da$device_id, de$device_id),]
  sc <- sc[match(da$device_id, sc$device_id),]
  cbind(da, de[,-1], sc[,-1])
}
