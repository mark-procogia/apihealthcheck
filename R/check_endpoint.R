#' Get API endpoint status
#'
#' @param endpoint
#' An atomic character vector, the URL of API endpoint that needs to be checked
#'
#' @return
#' An atomic character vector, the status code of the API endpoint
#' @details
#' This function is not exported
#'
get_endpoint_status <- function(endpoint){
  
  config <- httr::authenticate(user = Sys.getenv("API_USER"), password = Sys.getenv("API_PASSWORD"), type = "basic")
  
  httr::GET(
    url = endpoint,
    config = config,
    handle = NULL,
    headers = httr::add_headers(
      .headers = c("Content-Type" = "application/json", "Accept" = "application/json")
    )
  ) |> httr::status_code()
}

#' Check the health of API endpoints
#'
#' @param endpoint_name
#' An atomic character vector, the name, saved as R environment variables
#' locally, of the API endpoint
#' @param response_time_limit
#' The maximum response time for the API endpoints, beyond which the endpoint
#' will be considered unhealthy.
#' @inheritParams get_endpoint_status
#' @export
#' @details
#' This function is exported for external use
#'
test_api_endpoint <- function(endpoint,
                              endpoint_name = NULL,
                              response_time_limit = 5) {
  if (!is.null(endpoint_name)) {
    endpoint <- Sys.getenv(endpoint_name)
  }
  
  endpoint_status <- R.utils::withTimeout(
    testthat::expect_no_error(get_endpoint_status(endpoint = endpoint)),
    timeout = response_time_limit ,
    onTimeout = "silent"
  )
  
  testthat::expect_true(!is.null(endpoint_status), label = glue::glue(
    "Endpoint {ifelse(is.null(endpoint_name), endpoint, endpoint_name)} took too long to respond."
  ))
  
  if (!is.null(endpoint_status)){
    testthat::expect_equal(
      endpoint_status,
      200,
      label = glue::glue(
        "Endpoint {ifelse(is.null(endpoint_name), endpoint, endpoint_name)} failed"
      )
    )
  }
}