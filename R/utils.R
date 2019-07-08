
## examine the validity of input URL

patient.info_url <- function(x) {
  if (!grepl("^http", x) | !grepl("patient\\.info", x)) {
    stop("Input error: this function only accepts links from patient.info")
  }
  if (grepl("patient.info", x) && !grepl("forums", x)) {
    stop("Input error: this function only scrapes the forum pages (patient.info/forums) from patient.info")
  } else {
    return(x)
  }
}


patient.info_url_post <- function(x) {
  patient.info_url(x)
  if (!grepl("discuss", x) | grepl("browse", x)) {
    stop("Input error: this function only scrapes the post pages (patient.info/forums/discuss/...) from patient.info")
  } else {
    ## remove trailing slash (if there is one)
    x <- sub("/$", "", x)
    ## remove everything before slash
    x <- sub(".*/", "", x)
    ## create full URL
    paste0("https://patient.info/forums/discuss/", x)
  }
}

patient.info_url_group <- function(x) {
  patient.info_url(x)
  if (!grepl("discuss", x) && !grepl("browse", x)) {
    stop("Input error: this function only scrapes the group pages (patient.info/forums/discuss/browse/...) from patient.info")
  } else {
    ## remove trailing slash (if there is one)
    x <- sub("/$", "", x)
    ## remove everything before slash
    x <- sub(".*/", "", x)
    ## create full URL
    paste0("https://patient.info/forums/discuss/browse/", x)
  }
}


