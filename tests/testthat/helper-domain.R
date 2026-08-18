domain_env <- new.env(parent = baseenv())
domain_files <- c(
  "medicinal_products.R",
  "excipients.R",
  "source_documents.R",
  "evidence.R",
  "verification.R"
)

for (domain_file in domain_files) {
  sys.source(
    file.path(project_root(), "R", "domain", domain_file),
    envir = domain_env
  )
}
