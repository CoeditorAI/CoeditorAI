mod_user_subscription_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::as_fill_carrier(
    shiny::uiOutput(ns("user_subscription"))
  )
}


mod_user_subscription_server <- function(input, output, session,
                                         api_is_alive,
                                         current_user
                                         ) {
  ns <- session$ns

  output$user_subscription <- shiny::renderUI({

    api_alive <- api_is_alive()
    shiny::req(api_alive)

    current_user <- current_user()
    shiny::req(current_user$is_authorized)

    subscription_plan <- ifelse("freemium-user" %in% current_user$roles,
                                "freemium-user",
                                current_user$roles)

    is_subscription_active <- current_user$is_valid

    bslib::as_fill_item(
      min_height = "130px",
      bslib::value_box(
        title           = "Subscription Plan",
        value           = shiny::span(subscription_plan,
                                      style = ""),
        theme_color     = ifelse(is_subscription_active,
                                 "success", "danger"),
        showcase        = bsicons::bs_icon("currency-dollar"),
        showcase_layout = bslib::showcase_left_center(),
        full_screen     = FALSE,
        fill            = TRUE,
        {
          if (isFALSE(is_subscription_active)) {

            shiny::span(
              style = "font-size: 1em;",
              bsicons::bs_icon("exclamation-triangle"),
              "Your subscription plan is not active!",
              "Please visit ",
              shiny::a(href = "https://coeditorai.com", "CoeditorAI.com"),
              "website ",
              "to learn more about the subscription plans.")

          } else {

            shiny::span(
              style = "font-size: 1em;",
              if (current_user$is_valid) {
                shiny::span(
                  bsicons::bs_icon("check2-circle"),
                  "Your freemium plan is active."
                )
              } else {
                shiny::span(
                  "You have reached your freemium plan limit."
                )
              }
            )
          }
        }
      )
    )
  })

}
