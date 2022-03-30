#' spss2date
#'
#' This function will save an RData file, if one does not exists it will create it.
#'
#' @param list List of files from your environment you want to save
#' @param file Name of the RData file you want to save
#'
#' @return
#' @export
#'
spss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")

# ▬ clear.labels ------
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
my_rowmeans = function(...) Reduce(`+`, list(...))/length(list(...))


# ▬ say_something -----
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
