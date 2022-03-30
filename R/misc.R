#' spss2date
#'
#' Used to convert SPSS dates.   # src: https://stackoverflow.com/questions/37880975/spss-date-format-when-imported-into-r
#'
#' @param list List of files from your environment you want to save
#' @param file Name of the RData file you want to save
#'
#' @return
#' @export
#'
spss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")

# ▬ clear.labels ------
#' clear.labels
#'
#' clears labels from your df   # src: https://stackoverflow.com/questions/2394902/remove-variable-labels-attached-with-foreign-hmisc-spss-import-functions
#'
#' @param list List of files from your environment you want to save
#' @param file Name of the RData file you want to save
#'
#' @return
#' @export
#'

# src: https://stackoverflow.com/questions/2394902/remove-variable-labels-attached-with-foreign-hmisc-spss-import-functions
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in seq_along(x)) {
      class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled')
      attr(x[[i]],"label") <- NULL
    }
  } else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}

# my_rowmeans ----------
#' my_rowmeans
#'
#' take row means. Used in A-CAP project
#'
#' @param list List of files from your environment you want to save
#' @param file Name of the RData file you want to save
#'
#' @return
#' @export
#'
my_rowmeans = function(...) Reduce(`+`, list(...))/length(list(...))


# ▬ say_something -----
# say_something ----------
#' say_something
#'
#' Used to tap into Windows text-to-speech. Will only work on Windows-based machines.
#'
#' @param list List of files from your environment you want to save
#' @param file Name of the RData file you want to save
#'
#' @return
#' @export
#'
say_something <- function(message , voice) {

  voice <- paste0("\"$speak.SelectVoice('Microsoft ", voice, " Desktop');" )
  message <- paste0("$speak.Speak('", message, "');\"")

  system2(command = "PowerShell",
          args = c("-Command",
                   "\"Add-Type -AssemblyName System.Speech;",
                   "$speak = New-Object System.Speech.Synthesis.SpeechSynthesizer;",
                   voice,
                   message
          ))
}
