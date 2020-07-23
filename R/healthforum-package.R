#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(gsub("\\\n", " ", disclaimer))
}

disclaimer <- "`healthforum` was developed to collect publicly available data from
the website *patient.info*. The purpose of this package is to facilitate
academic research. It is the final user's responsibility to store the data
securely and obey all applicable local, state, and federal laws. Users may
want to contact *patient.info* to obtain a consent of collecting and using data.
Their contact address is: Data Protection Officer, Fulford Grange,
Micklefield Lane, Rawdon, Leeds, LS19 6BA. Email: privacy@emishealth.com."
