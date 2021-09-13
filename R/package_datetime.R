#' Package datetimes
#'
#' Determine the datetimes for packages by checking the `DESCRIPTION` file
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

  out <- as.POSIXct(c())
  missing_package <- character(0)
  for (i in seq_along(package)) {
    out[i] <- package_description_mtime(package[i])

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


package_description_mtime <- function(package) {
  stopifnot(is.character(package))
  desc_loc <- paste0(find.package(package), "/DESCRIPTION")
  file.mtime(desc_loc)
}
