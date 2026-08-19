excifinder_http_user_agent <- function() {
  paste0("ExciFinder/", excifinder_version())
}

excifinder_http_retry_statuses <- function() {
  c(429L, 500L, 502L, 503L, 504L)
}

excifinder_http_policy <- function() {
  retry_statuses <- excifinder_http_retry_statuses()
  list(
    timeout_seconds = 15,
    max_attempts = 3L,
    pause_base = 0.5,
    pause_cap = 4,
    pause_min = 0.5,
    user_agent = excifinder_http_user_agent(),
    terminate_on = setdiff(400:599, retry_statuses)
  )
}

excifinder_http_get <- function(url, query = list(), headers = list()) {
  policy <- excifinder_http_policy()
  configs <- c(
    list(
      httr::timeout(policy$timeout_seconds),
      httr::user_agent(policy$user_agent)
    ),
    headers
  )
  do.call(
    httr::RETRY,
    c(
      list(
        verb = "GET",
        url = url,
        query = query,
        times = policy$max_attempts,
        pause_base = policy$pause_base,
        pause_cap = policy$pause_cap,
        pause_min = policy$pause_min,
        terminate_on = policy$terminate_on,
        quiet = TRUE
      ),
      configs
    )
  )
}
