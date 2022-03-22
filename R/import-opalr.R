#' @title Import data from Opal
#'
#' @description Let user set-up an Opal connection, then import the data.
#'
#' @inheritParams import-globalenv
#'
#' @template module-import
#'
#' @export
#' @name import-import-opalr-ui
#'
#' @importFrom shiny NS actionLink
#' @importFrom shinyWidgets textInputIcon
#' @importFrom htmltools tags tagList
#'
#' @example examples/from-googlesheets.R
import_opalr_ui <- function(id, title = TRUE) {

  ns <- NS(id)

  if (isTRUE(title)) {
    title <- tags$h4(
      i18n("Import from Opal"),
      class = "datamods-title"
    )
  }

  tags$div(
    class = "datamods-import",
    html_dependency_datamods(),
    title,
    textInputIcon(
      inputId = ns("url"),
      label = i18n("Enter an Opal URL:"),
      icon = phosphoricons::ph("link"),
      width = "100%"
    ),
    textInputIcon(
      inputId = ns("username"),
      label = i18n("Enter an Opal username:"),
      icon = phosphoricons::ph("user"),
      width = "100%"
    ),
    dbConnectGUI::passwordInputIcon(
      inputId = ns("password"),
      label = i18n("Enter the Opal password:"),
      icon = phosphoricons::ph("key"),
      width = "100%"
    ),
    shinyWidgets::actionBttn(
      inputId = ns("connect"),
      label = i18n("Connect"),
      icon = phosphoricons::ph("arrows-clockwise")
    ),
    selectInput(
      inputId = ns("datasource"),
      label = "Datasource",
      choices = character(0),
      selectize = TRUE
    ),
    tags$div(
      id = ns("import-placeholder"),
      alert(
        id = ns("import-result"),
        status = "info",
        tags$b(i18n("Nothing pasted yet!")),
        i18n("Please paste a valid Opal link in the dialog box above."),
        dismissible = TRUE
      )
    ),
    uiOutput(
      outputId = ns("container_confirm_btn"),
      style = "margin-top: 20px;"
    )
  )
}


#' @inheritParams import_globalenv_server
#'
#' @export
#'
#' @importFrom shiny moduleServer
#' @importFrom shiny reactiveValues observeEvent removeUI reactive req
#' @importFrom htmltools tags tagList
#'
#' @rdname import-opalr-server
import_opalr_server <- function(id,
                                       btn_show_data = TRUE,
                                       trigger_return = c("button", "change"),
                                       return_class = c("data.frame", "data.table", "tbl_df"),
                                       reset = reactive(NULL)) {

  trigger_return <- match.arg(trigger_return)

  module <- function(input, output, session) {

    ns <- session$ns
    imported_rv <- reactiveValues(data = NULL, name = NULL)
    temporary_rv <- reactiveValues(data = NULL, name = NULL, status = NULL)

    observeEvent(reset(), {
      temporary_rv$data <- NULL
      temporary_rv$name <- NULL
      temporary_rv$status <- NULL
    })

    output$container_confirm_btn <- renderUI({
      if (identical(trigger_return, "button")) {
        button_import()
      }
    })

    observeEvent(input$trigger, {
      if (identical(trigger_return, "change")) {
        hideUI(selector = paste0("#", ns("confirm-button")))
      }
    })

    connect <- function(session = getDefaultReactiveDomain()) {
      req(input$url, input$username, input$password)
      o <- try(opalr::opal.login(username = input$username,
                                 password = input$password,
                                 url      = input$url), silent = TRUE)
      if (inherits(o, "try-error")) { # https://opal-demo.obiba.org/
        toggle_widget(inputId = "confirm", enable = FALSE)
        insert_error(mssg = i18n(attr(o, "condition")$message))
        temporary_rv$status <- "error"
        temporary_rv$data <- NULL
      } else {
        toggle_widget(inputId = "confirm", enable = TRUE)
        insert_alert(
          selector = ns("import"),
          status = "success",
          make_success_alert(
            o,
            trigger_return = trigger_return,
            btn_show_data = btn_show_data
          )
        )
        ds <- opalr::opal.datasources(o)
        updateSelectizeInput(session = session,
                             ns("datasource"),
                             choices = ds$name)
        temporary_rv$status <- "success"
        # temporary_rv$data <- imported
        opalr::opal.logout(o)
      }
    }

    observeEvent(input$connect, {
      connect()
    }, ignoreInit = TRUE)

    observeEvent(input$password, {
    }, ignoreInit = TRUE)

    observeEvent(input$link, {
    }, ignoreInit = TRUE)

    observeEvent(input$see_data, {
      connect()
      show_data(temporary_rv$data, title = i18n("Imported data"))
    })

    observeEvent(input$confirm, {
      imported_rv$data <- temporary_rv$data
    })

    if (identical(trigger_return, "button")) {
      return(list(
        status = reactive(temporary_rv$status),
        name = reactive(imported_rv$name),
        data = reactive(as_out(imported_rv$data, return_class))
      ))
    } else {
      return(list(
        status = reactive(temporary_rv$status),
        name = reactive(temporary_rv$name),
        data = reactive(as_out(temporary_rv$data, return_class))
      ))
    }
  }

  moduleServer(
    id = id,
    module = module
  )
}



# Utils -------------------------------------------------------------------

