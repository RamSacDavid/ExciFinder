test_that("custom shell preserves the public UI contract", {
  rendered <- htmltools::renderTags(application_env$build_excifinder_ui())
  ui_html <- rendered$html

  for (input_id in c("pa", "excipiente", "buscar", "downloadData")) {
    expect_match(ui_html, paste0('id="', input_id, '"'), fixed = TRUE)
  }
  expect_match(rendered$head, 'href="excifinder.css"', fixed = TRUE)
  expect_match(rendered$head, 'src="excifinder.js"', fixed = TRUE)
  expect_match(ui_html, "excifinder-header", fixed = TRUE)
  expect_match(ui_html, "excifinder-search-card", fixed = TRUE)
  expect_match(ui_html, "excifinder-utility-row", fixed = TRUE)
  expect_match(ui_html, "excifinder-clinical-notice", fixed = TRUE)
})

test_that("focus handler targets the selected master button safely", {
  script <- paste(readLines(
    file.path(project_root(), "www", "excifinder.js"),
    warn = FALSE,
    encoding = "UTF-8"
  ), collapse = "\n")

  expect_match(
    script,
    'Shiny.addCustomMessageHandler(\n    "excifinder-focus-master-product"',
    fixed = TRUE
  )
  expect_match(script, "new MutationObserver", fixed = TRUE)
  expect_match(
    script,
    'button.excifinder-master-item[data-product-id]',
    fixed = TRUE
  )
  expect_match(
    script,
    "buttons[index].dataset.productId === message.product_id",
    fixed = TRUE
  )
  expect_match(
    script,
    'buttons[index].getAttribute("aria-pressed") === "true"',
    fixed = TRUE
  )
  expect_match(script, "focus({ preventScroll: true })", fixed = TRUE)
  expect_match(script, "observer.disconnect()", fixed = TRUE)
  expect_false(grepl("message.product_id]", script, fixed = TRUE))
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

test_that("UI exposes an accessible master-detail browser without visible DT", {
  ui_source <- paste(readLines(
    file.path(project_root(), "R", "ui", "shiny_ui.R"),
    warn = FALSE,
    encoding = "UTF-8"
  ), collapse = "\n")
  server_source <- paste(readLines(
    file.path(project_root(), "R", "ui", "shiny_server.R"),
    warn = FALSE,
    encoding = "UTF-8"
  ), collapse = "\n")

  expect_match(ui_source, "excifinder-master-detail", fixed = TRUE)
  expect_match(ui_source, "excifinder-clinical-detail", fixed = TRUE)
  expect_match(ui_source, 'tags$button', fixed = TRUE)
  expect_match(ui_source, 'aria-pressed', fixed = TRUE)
  expect_match(ui_source, "selected_product_id", fixed = TRUE)
  expect_false(grepl("DTOutput", ui_source, fixed = TRUE))
  expect_false(grepl("renderDT", server_source, fixed = TRUE))
  expect_false(grepl("DT::datatable", server_source, fixed = TRUE))
  expect_false(grepl("scrollX", server_source, fixed = TRUE))
})

test_that("master-detail CSS is responsive, sticky only on desktop, and keyboard visible", {
  css <- paste(readLines(
    file.path(project_root(), "www", "excifinder.css"),
    warn = FALSE,
    encoding = "UTF-8"
  ), collapse = "\n")

  expect_match(css, ".excifinder-master-detail", fixed = TRUE)
  expect_match(css, "grid-template-columns: minmax(0, 1fr)", fixed = TRUE)
  expect_match(css, "grid-template-columns: minmax(0, 34fr) minmax(0, 66fr)", fixed = TRUE)
  expect_match(css, "position: sticky", fixed = TRUE)
  expect_match(css, "top: 16px", fixed = TRUE)
  expect_match(css, ":focus-visible", fixed = TRUE)
  expect_match(css, "overflow-wrap: anywhere", fixed = TRUE)
})
