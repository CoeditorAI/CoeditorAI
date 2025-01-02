get_api_url <- function() {

  api_url <- Sys.getenv("COEDITORAI_API_URL", "")

  if (api_url == "") {
    return("https://coeditorai.com/api/v1")
  }

  return(api_url)
}

get_versions <- function() {

  api_url <- get_api_url()

  tryCatch({

    request <-
      httr2::request(api_url) |>
      httr2::req_timeout(getOption("coeditorai.timeout")) |>
      httr2::req_user_agent(paste0("CoeditorAI-RStudio/",
                                   utils::packageVersion("CoeditorAI"))) |>
      httr2::req_url_path_append("versions")

    response <-
      request |>
      httr2::req_perform()

    body <-
      response |>
      httr2::resp_body_json(simplifyVector = TRUE)

    body

  }, error = function(e) {

    return(NULL)
  })

}

is_api_alive <- function() {

  api_url <- get_api_url()

  tryCatch({


    request <-
      httr2::request(api_url) |>
      httr2::req_timeout(getOption("coeditorai.timeout")) |>
      httr2::req_user_agent(paste0("CoeditorAI-RStudio/",
                                   utils::packageVersion("CoeditorAI"))) |>
      httr2::req_url_path_append("test")

    response <-
      request |>
      httr2::req_perform()

    body <-
      response |>
      httr2::resp_body_json(simplifyVector = TRUE)

    body$is_alive

  }, error = function(e) {

    return(FALSE)
  })
}

create_post_api_request <- function(
    input_text_id = NULL,
    api_url       = get_api_url(),
    user          = Sys.getenv("COEDITORAI_USER_EMAIL"),
    password      = Sys.getenv("COEDITORAI_USER_PASSWORD"),
    ...
    ) {

  args <- list(...)
  session_id <- args$session_id

  request <-
    create_api_request(api_url  = api_url,
                       user     = user,
                       password = password)

  req_body <- list()

  if (rstudioapi::isAvailable()) {

    req_body$ide <- list(
      mode       = rstudioapi::versionInfo()$mode,
      version    = as.character(rstudioapi::versionInfo()$version),
      is_rstudio = is_rstudio(),
      is_positron = is_positron()
    )
  }

  req_body$coeditorai <- list(session_id    = session_id,
                              input_text_id = input_text_id)

  request <-
    request |>
    httr2::req_body_json(req_body)

  return(request)
}

create_get_api_request <- function(
    api_url       = get_api_url(),
    user          = Sys.getenv("COEDITORAI_USER_EMAIL"),
    password      = Sys.getenv("COEDITORAI_USER_PASSWORD"),
    ...
    ) {

  request <-
    create_api_request(api_url  = api_url,
                       user     = user,
                       password = password)

  return(request)
}

create_api_request <- function(api_url  = get_api_url(),
                               user     = Sys.getenv("COEDITORAI_USER_EMAIL"),
                               password = Sys.getenv("COEDITORAI_USER_PASSWORD")
                               ) {
  request <-
    httr2::request(api_url) |>
    httr2::req_timeout(getOption("coeditorai.timeout")) |>
    httr2::req_auth_basic(user     = user,
                          password = password) |>
    httr2::req_user_agent(paste0("CoeditorAI/",
                                 utils::packageVersion("CoeditorAI")))
  return(request)
}

print_debug_info <- function(request) {

  cli::cli_alert("Preparing request...")
  print(request)
  cli::cli_alert_info("Body:")
  cat(jsonlite::toJSON(request$body$data, auto_unbox = TRUE), "\n")
}

transform_snapshot <- function(x) {

  gsub(x = x,
       pattern = "useragent: 'CoeditorAI.*?'",
       replacement = "useragent: 'CoeditorAI/redacted'")
}
