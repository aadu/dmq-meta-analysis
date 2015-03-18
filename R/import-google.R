#' Import dataset from google sheets
#'
#' @import RCurl
#' @param url
#' @param na_strings Vector of missing-value strings
#' @return DataFrame of google sheet
#' @export
import.google <- function(df_url, na_strings=c("-", "NA", "", "--", "---")){
  options(RCurlOptions=list(
        cainfo=system.file("CurlSSL", "cacert.pem", package="RCurl")))
  if(!capabilities()["http/ftp"])
    stop("No internet capabilities.")
  if(.Platform$OS.type == "unix" && is.null(nsl("cran.r-project.org")))
    stop("Internet appears to be down.")
  file <- getURL(url=df_url)
  con <- textConnection(file)
  df <- read.csv(con, na.strings=na_strings,
    header=TRUE, stringsAsFactors=FALSE)
  close(con)
  return(df)
}


#' @describeIn import.google
#' @export
load_dmq_data <- function(study="studies"){
  cat("Valid studies include 'studies', 'personality', 'norms', and 'cor'")
  paths = list()
  prefix = "https://docs.google.com/spreadsheet/pub?key="
  paths['studies'] = paste0(prefix,
                            "0Ar2MWSi_hKI6dEM4X2F3eUVUN1pQRlE5OD",
                            "lUZEt4RkE&single=true&gid=0&output=csv")
  paths['personality'] = paste0(prefix,
                                "0Ar2MWSi_hKI6dGthVmttRkFXbDg1SlV6cjlvR",
                                "2MtWHc&single=true&gid=1&output=csv")
  paths['norms'] = paste0(prefix,
                          "0Ar2MWSi_hKI6dFBVaXhtVXRmazdycjFOYkEtej",
                          "JqU1E&single=true&gid=1&output=csv")
  paths['cor'] = paste0(prefix,
                        "0Ar2MWSi_hKI6dGNFOWktUl85Sm1nRzVYeWFsZi",
                        "11dXc&single=true&gid=1&output=csv")
  all_studies = import.google(paths[study])
  # Exclude studies with anything in the "exclude column"
  studies = subset(all_studies, is.na(exclude))
  # Number of Studies
  cat(number_of_studies = length(unique(studies$ref)))
  return(studies)
}
