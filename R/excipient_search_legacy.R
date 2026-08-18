buscar_medicamento_por_excipiente_legacy <- function(meds, i, term_busqueda, cache_pdf) {
  nreg <- meds$nregistro[i]
  doc_ft <- meds$docs[[i]] %>% filter(tipo == 1)
  pdf_url <- if(nrow(doc_ft) > 0) doc_ft$url[1] else "#"

  found <- FALSE
  if (exists(nreg, envir = cache_pdf)) {
    if (grepl(term_busqueda, get(nreg, envir = cache_pdf))) found <- TRUE
  } else {
    txt <- obtener_seccion_61_legacy(nreg)
    if (!grepl(term_busqueda, txt) && pdf_url != "#") {
      try({
        pdf_txt <- obtener_texto_pdf_legacy(pdf_url)
        txt <- paste(txt, pdf_txt)
      }, silent = TRUE)
    }
    assign(nreg, txt, envir = cache_pdf)
    if (grepl(term_busqueda, txt)) found <- TRUE
  }

  data.frame(nombre = meds$nombre[i], estado = found, url = pdf_url)
}

buscar_excipiente_legacy <- function(principio_activo, excipiente, limite, cache_pdf) {
  meds <- obtener_medicamentos_legacy(principio_activo, limite)
  if (is.null(meds)) return(NULL)

  term_busqueda <- normalizar(excipiente)

  withProgress(message = 'Consultando CIMA...', value = 0, {
    results <- lapply(1:nrow(meds), function(i) {
      incProgress(1/nrow(meds), detail = "Analizando PDF de ficha técnica de CIMA AEMPS")
      buscar_medicamento_por_excipiente_legacy(meds, i, term_busqueda, cache_pdf)
    })
  })

  bind_rows(results) %>% arrange(nombre)
}
