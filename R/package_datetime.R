#' Package datetimes
#'
#' Determine the datetimes for packages by first parsing the package
#' `DESCRIPTION` and then, if no date is found, checking the `DESCRIPTION` file
#' modification time.
#'
#' @param package
#'
#' @return
#' @noRd
#' @keywords internal
#'
#' @examples
#'
#' package_datetime(c("utils", "makepipe"))
#'
package_datetime <- function(package) {
  stopifnot(is.character(package))
  missing_package <- setdiff(package, utils::installed.packages()[, 1])
  if (length(missing_package) > 0) {
    stop(
      "Package not installed: `",
      paste(missing_package, sep = "`, `"), "`",
      call. = FALSE
    )
  }

  out <- package_description_date(package)
  missing_package <- character(0)
  for (i in seq_along(out)) {
    if (is.na(out[i]) | is.null(out[i])) {
      out[i] <- package_description_mtime(package[i])
    }

    if (is.na(out[i]) | is.null(out[i])) {
      missing_package <- c(missing_package, package[i])
    }
  }

  if (length(missing_package) > 0) {
    stop(
      "Cannot determine date for package: `",
      paste(missing_package, sep = "`, `"), "`",
      call. = FALSE
    )
  }

  as.POSIXct(out)
}

package_description_date <- function(package) {
  stopifnot(is.character(package))
  out <- as.POSIXct(character(0))
  for (pkg in package) {
    date_fields <- c("Date", "Packaged", "Date/Publication", "Built")
    desc <- utils::packageDescription(pkg, fields = date_fields)
    if (!(is.list(desc) && length(names(desc)) >= 1)) return(NULL)

    for (fld in desc) {
      r <- fld
      if (is.null(r) | is.na(r)) next
      if (fld == "Built") r <- strsplit(r, split = "; ", fixed = TRUE)[[1L]][[3L]]
      r <- as.POSIXct(r, optional = TRUE)
      if (!is.na(r)) break
    }

    if (is.na(r) | is.null(r)) r <- as.POSIXct(NA)
    out <- c(out, r)
  }

  out
}

package_description_mtime <- function(package) {
  stopifnot(is.character(package))
  desc_loc <- paste0(find.package(package), "/DESCRIPTION")
  file.mtime(desc_loc)
}
