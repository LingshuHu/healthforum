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

disclaimer <- "`healthforum` was developed to collect publicly available data from the website *patient.info*. The purpose of this package is to facilitate academic research. It is the final user's responsibility to store the data securely and obey all applicable local, state, and federal laws and ethical guidelines. For informed consent procedures for using patients.info forum data, please contact their Data Protection Officer at privacy@emishealth.com. Their address is Fulford Grange, Micklefield Lane, Rawdon, Leeds, LS19 6BA. You may also want to contact your local IRB to obtain information about privacy policy."
