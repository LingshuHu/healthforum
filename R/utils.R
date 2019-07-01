

patient.info_url <- function(x) {
  if (grepl("^http", x) && !grepl("patient\\.info", x)) {
    stop("This function only accepts forum links from patient.info ")
  }
  if (grepl("patient.info", x) && !grepl("forums/discuss", x)) {
    stop("This function only scrapes the forum pages (forums/discuss) from patient.info")
  }
  ## remove trailing slash (if there is one)
  x <- sub("/$", "", x)
  ## remove everything before slash
  x <- sub(".*/", "", x)
  ## create full URL
  paste0("https://patient.info/forums/discuss/", x)
}
