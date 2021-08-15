#' rename
#'
#' This function will create a directory if it doesnt exist, then copy the file and finally rename it. Most functions I have found will move the file and therein delete it. Which this function avoids.
#'
#' @param from path where the files are coming from
#' @param to path where you want the files to go to
#'
#' @return
#' @export
#'
#' @examples
rename <- function(from, to) {
  todir <- to
  if (!isTRUE(file.info(todir)$isdir))
    dir.create(paste0(dirname(to), "//"), recursive = TRUE) # create the directory to move the file into

  file.copy(from = from,  to = paste0(dirname(to), ""))
  file.rename(paste0(dirname(to),"/", basename(from)),
              to)
}
