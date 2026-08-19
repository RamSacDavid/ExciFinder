normalizar <- function(texto) {
  if (is.null(texto) || length(texto) == 0 || texto == "") return("")
  texto <- gsub("<[^>]*>", " ", texto)
  texto <- stri_trans_general(tolower(texto), "Latin-ASCII")
  # KNOWN LEGACY BEHAVIOUR — covered by characterization tests and scheduled for deliberate replacement.
  texto <- gsub("z", "c", texto)
  return(texto)
}
