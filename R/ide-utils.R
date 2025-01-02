get_selection <- function() {

  if (!rstudioapi::isAvailable()) {
    return("Supported IDE (RStudio / Positron) is not running.")
  }

  if (rstudioapi::hasFun("selectionGet")) {
    
    selection <- rstudioapi::selectionGet()$value

  } else {

    context   <- rstudioapi::getActiveDocumentContext() 
    if (is.null(context)) {
      selection <- "Unable to get the current document selection!"
    } else {
      selection <- rstudioapi::primary_selection(context)$text
    }
  }

  selection
}

set_selection <- function(text) {

  if (!rstudioapi::isAvailable()) 
    stop(call. = FALSE, 
      "Supported IDE (RStudio / Positron) is not running.")

  if (rstudioapi::hasFun("selectionSet")) {
    
    rstudioapi::selectionSet(value = text)

  } else {

    context  <- rstudioapi::getActiveDocumentContext() 
    location <- rstudioapi::primary_selection(context)$range

    rstudioapi::modifyRange(location = location,
                            text = text)
    
  }
}

is_rstudio <- function() {
  as.logical(as.integer(Sys.getenv("RSTUDIO", 0)))
}

is_positron <- function() {
  as.logical(as.integer(Sys.getenv("POSITRON", 0)))
}
