obtener_medicamentos_legacy <- function(principio_activo, limite) {
  res_pa <- GET(
    "https://cima.aemps.es/cima/rest/medicamentos",
    query = list(practiv1 = principio_activo)
  )
  if (status_code(res_pa) != 200) return(NULL)

  fromJSON(rawToChar(res_pa$content))$resultados %>% head(limite)
}

obtener_seccion_61_legacy <- function(nreg) {
  url_doc <- paste0(
    "https://cima.aemps.es/cima/rest/docSegmentado/contenido/1?nregistro=",
    nreg
  )
  res_doc <- GET(url_doc)
  txt <- ""

  if (status_code(res_doc) == 200) {
    sec61 <- fromJSON(rawToChar(res_doc$content)) %>% filter(seccion == "6.1")
    if (nrow(sec61) > 0) txt <- normalizar(sec61$contenido)
  }

  txt
}

obtener_texto_pdf_legacy <- function(pdf_url) {
  normalizar(paste(pdftools::pdf_text(pdf_url), collapse = " "))
}
