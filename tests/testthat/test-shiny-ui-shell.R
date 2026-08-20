test_that("custom shell preserves the public UI contract", {
  rendered <- htmltools::renderTags(application_env$build_excifinder_ui())
  ui_html <- rendered$html

  for (input_id in c("pa", "excipiente", "buscar", "downloadData")) {
    expect_match(ui_html, paste0('id="', input_id, '"'), fixed = TRUE)
  }
  expect_match(rendered$head, 'href="excifinder.css"', fixed = TRUE)
  expect_match(ui_html, "excifinder-header", fixed = TRUE)
  expect_match(ui_html, "excifinder-search-card", fixed = TRUE)
  expect_match(ui_html, "excifinder-utility-row", fixed = TRUE)
  expect_match(ui_html, "excifinder-clinical-notice", fixed = TRUE)
})

test_that("custom shell has no AdminLTE or shinydashboard components", {
  ui_source <- paste(readLines(
    file.path(project_root(), "R", "ui", "shiny_ui.R"),
    warn = FALSE,
    encoding = "UTF-8"
  ), collapse = "\n")

  for (component in c(
      "dashboardPage", "dashboardHeader", "dashboardSidebar", "dashboardBody"
  )) {
    expect_false(grepl(component, ui_source, fixed = TRUE))
  }
  expect_false(grepl("shinydashboard::", ui_source, fixed = TRUE))
  expect_false(grepl("tags$style", ui_source, fixed = TRUE))
  expect_false(grepl('style = "', ui_source, fixed = TRUE))
})

test_that("external CSS keeps layout protection and semantic state colours", {
  css <- paste(readLines(
    file.path(project_root(), "www", "excifinder.css"),
    warn = FALSE,
    encoding = "UTF-8"
  ), collapse = "\n")
  state_colours <- c(
    "#a82d35", "#fbe9e9",
    "#287a45", "#e8f5ec",
    "#5f6870", "#eeeeee",
    "#b85f00", "#fff0df"
  )

  expect_match(css, "overflow-x: hidden", fixed = TRUE)
  expect_match(css, "overflow: hidden", fixed = TRUE)
  expect_match(css, "@media (min-width: 768px)", fixed = TRUE)
  for (colour in state_colours) {
    expect_match(css, colour, fixed = TRUE)
  }
  for (state_class in c(
      "state-identified", "state-not-identified",
      "state-indeterminate", "state-conflicting"
  )) {
    expect_match(css, state_class, fixed = TRUE)
  }
})
