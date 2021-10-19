#' Package datetimes
#'
#' Determine the datetimes for packages by checking the `DESCRIPTION` file
#' modification time.
#'
#' @param package A character vector containing the names of packages
#'
#' @return A `POSIXct` vector with names corresponding to `package`
#' @noRd
#' @keywords internal
#'
#' @examples
#'
#' package_datetime(c("utils", "makepipe"))
package_datetime <- function(package) {
  stopifnot(is.character(package))
  out <- vapply(
    package,
    function(pkg) as.numeric(package_description_mtime(pkg)),
    FUN.VALUE = numeric(1)
  )

  as.POSIXct(out, origin = "1970-01-01")
}

package_description_mtime <- function(package) {
  stopifnot(is.character(package))
  desc_loc <- paste0(find.package(package), "/DESCRIPTION")
  file.mtime(desc_loc)
}
