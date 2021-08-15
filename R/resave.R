#' resave
#'
#' This function will save an RData file, if one does not exists it will create it.
#'
#' @param list List of files from your environment you want to save
#' @param file Name of the RData file you want to save
#'
#' @return
#' @export
#'
resave <- function(..., list = character(), file) {
  #  add objects to existing Rdata file. Original code written by "flodel"
  # on StackOverflow (http://www.linkedin.com/in/florentdelmotte)  .
  if (file.exists(file) == FALSE){
    delete_me = data.frame()
    save(delete_me, file = file)
    rm(delete_me)
  } else if (file.exists(file) == TRUE){
    previous  <- load(file)

    if (sjmisc::str_contains(previous, "delete_me") == TRUE){
      previous <- previous[previous != "delete_me"]
    }

    var.names <- c(list, as.character(substitute(list(...)))[-1L])
    for (var in var.names) assign(var, get(var, envir = parent.frame()))
    save(list = unique(c(previous, var.names)), file = file)
  }
}
