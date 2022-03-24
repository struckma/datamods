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
      value = "https://opal-demo.obiba.org/",
      label = i18n("Enter an Opal URL:"),
      icon = phosphoricons::ph("link"),
      width = "100%"
    ),
    textInputIcon(
      inputId = ns("username"),
      value = "administrator",
      label = i18n("Enter an Opal username:"),
      icon = phosphoricons::ph("user"),
      width = "100%"
    ),
    dbConnectGUI::passwordInputIcon(
      inputId = ns("password"),
      label = i18n("Enter the Opal password:"),
      icon = phosphoricons::ph("key"),
      value = "password",
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
      choices = character(0)
    ),
    selectInput(
      inputId = ns("table"),
      label = "Table",
      choices = character(0)
    ),
    checkboxInput(
      inputId = ns("metadata"),
      label = "Import Metadata"
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
    imported_rv <- reactiveValues(data = NULL, name = NULL, ds = NULL)
    temporary_rv <- reactiveValues(data = NULL, name = NULL, status = NULL,
                                   ds = NULL)

    observeEvent(reset(), {
      temporary_rv$data <- NULL
      temporary_rv$name <- NULL
      temporary_rv$status <- NULL
      temporary_rv$ds <- NULL
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

    update_tables <- function() {
      choices <-
        temporary_rv$ds$table[temporary_rv$ds$name == input$datasource]
      if (length(choices) > 0) {
        choices <- trimws(strsplit(choices, "|", fixed = TRUE)[[1]])
        updateSelectInput(session = session,
                          inputId = "table",
                          choices = choices)
      }
    }

    connect <- function() {
      req(input$url, input$username, input$password)
      o <- try(opalr::opal.login(username = input$username,
                                 password = input$password,
                                 url      = input$url), silent = TRUE)
      if (inherits(o, "try-error")) { # https://opal-demo.obiba.org/
        toggle_widget(inputId = "confirm", enable = FALSE)
        insert_error(mssg = i18n(attr(o, "condition")$message))
        temporary_rv$status <- "error"
        temporary_rv$data <- NULL
        temporary_rv$ds <- NULL
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
        temporary_rv$ds <- ds

        updateSelectInput(session = session,
                          selected = input$datasource,
                          inputId = "datasource",
                          choices = ds$name)

        if (input$metadata) {
          imported <- try(
            opal2dataquieR(
              opalr::opal.table_dictionary_get(o,
                                               input$datasource,
                                               input$table
                                              )
            ),
            silent = TRUE
          )
        } else {
          imported <- try(
            opalr::opal.table_get(o, input$datasource, input$table),
            silent = TRUE
          )
        }

        if (inherits(imported, "try-error")) { # https://opal-demo.obiba.org/
          toggle_widget(inputId = "confirm", enable = FALSE)
          insert_error(mssg = i18n(attr(imported, "condition")$message))
          temporary_rv$status <- "error"
          temporary_rv$data <- NULL
        } else {
          temporary_rv$status <- "success"
          temporary_rv$data <- imported
        }
        opalr::opal.logout(o)
      }
    }

    observeEvent((input$metadata), {
      connect()
    }, ignoreInit = TRUE)

    observeEvent(input$datasource, {
      update_tables()
    }, ignoreInit = TRUE)

    observeEvent((input$table), {
      connect()
    }, ignoreInit = TRUE)

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

#' ensure that a column LABEL is there. OPAL provides label, but also localized ones such as label:de or label:en.
#'
#' @param df_labelled the data frame with label columns
#' @param preferred_locale the prefered locale, en by default
#'
#' @return df_labelled enriched by a valid label column
#' @author Jörg Henke
#'
util_ensure_default_label <- function(df_labelled, preferred_locale = "en") {
  cn <- colnames(df_labelled)
  if ("label" %in% cn) {
    # all is well, leave it as it is (other labels can become variable attributes)
  } else {
    localized_labels <- cn[grepl("label", cn)]
    if (length(localized_labels) > 0) {
      if (length(localized_labels) > 1) {
        preferred_colname <- sprintf("label:%s", preferred_locale)
        if (preferred_colname %in% localized_labels) {
          df_labelled$label <- df_labelled[[preferred_colname]]
          df_labelled[[preferred_colname]] <- NULL # we don't need that column any longer
        } else {
          df_labelled$label <- df_labelled[[localized_labels[[1]]]]
          df_labelled[[localized_labels[1]]] <- NULL # we don't need that column any longer
        }
      } else {
        df_labelled$label <- df_labelled[[localized_labels[1]]]
        df_labelled[[localized_labels[1]]] <- NULL # we don't need that column any longer
      }
    } else {
      df_labelled$label <- "OPAL did not provide a label column"
      warning("OPAL did not provide a label column")
    }
  }
  return(df_labelled)
}

#' convert opal data types to dataquieR compatible ones
#' see also: https://gitlab.com/umg_hgw/sc/implementation/opalrepository/-/issues/1
#'
#' @param opal_datatypes the opal data types vector
#'
#' @return the same vector with dataquieR compatible data types
#'
#' @author Jörg Henke
#'
util_dataquieR_datatypes <- function(opal_datatypes) {
  mapping <- c("integer" = "integer",
               "decimal" = "float",
               "text" = "string",
               "binary" = "integer",
               "locale" = "string",
               "boolean" = "integer",
               "datetime" = "datetime",
               "date" = "datetime",
               "point" = "string",
               "linestring" = "string",
               "polygon" = "string")
  return(mapping[opal_datatypes])
}

#' prepare the categories dataframe to have one line for all variables
#'
#' @param df_categories the OPAL Categories data frame
#'
#' @return a data frame with OPAL Categories the dataquieR way
#'
#' @author Adrian Richter
#'
#' @importFrom data.table dcast
util_prepare_categories <- function(df_categories) {
  xm <- dcast(setDT(df_categories), variable ~ name + label, fill = 0)
  xm <- as.data.frame(apply(xm, 2, function(x) {
    ifelse(x == 0, NA, x)
  }))
  vl <- gsub("_", " = ", names(xm))
  nc <- ncol(xm)
  for (i in 1:nrow(xm)) {
    xm$VALUE_LABELS[i] <- paste0(vl[2:nc][!is.na(xm[i, 2:nc])], collapse = " | ")
  }
  xm <- xm[, c("variable", "VALUE_LABELS")]
  names(xm) <- c("VAR_NAMES", "VALUE_LABELS")
  return(xm)
}

#' extract the missings from the values
#'
#' @param df_categories the OPAL categories sheet as a data frame
#' @param ordered_variables the variables in the correct order
#'
#' @return the full vector of missing lists ordered by the ordered_variables
#' @author Jörg Henke
#'
#' @importFrom stats setNames
#' @importFrom data.table dcast setDT
util_extract_missinglist <- function(df_categories, ordered_variables) {
  df_missing <- df_categories[df_categories$missing, c("variable", "name"), drop = FALSE]
  if (nrow(df_missing) > 0) {
    df_missing

    xm <- dcast(setDT(df_missing), variable ~ name, fill = 0)

    paste0withoutNA <- function(x) {
      paste0(x[!is.na(x)], collapse = "|")
    }

    merged <- setNames(apply(xm[, -1, drop = FALSE], 1, paste0withoutNA), nm = xm[, 1, drop = TRUE])

    return(merged[ordered_variables])
  } else {
    return(NA_character_)
  }
}

rename <- function(df, rnvec) {
  if (length(names(rnvec)) == 0) {
    stop("Need names")
  }
  df[, rnvec] <- df[, names(rnvec)]
  df[, names(rnvec)] <- NULL
  df
}

#'
#' @author Jörg Henke
#'
opal2dataquieR <- function(opal_input) {

  df_variables <- opal_input$variables
  df_categories <- opal_input$categories

  df_vars <- df_variables[!is.na(df_variables$index), , drop = FALSE]
  df_vars <- util_ensure_default_label(df_vars)
  df_categories <- util_ensure_default_label(df_categories)
  df_vars <- rename(df_vars, c("index"       = "VARIABLE_ORDER",
                               "name"        = "VAR_NAMES",
                               "description" = "LONG_LABEL",
                               "label"       = "LABEL"))
  df_vars$DATA_TYPE <- util_dataquieR_datatypes(df_vars$valueType)
  df_vars$valueType <- NULL

  df_vars <- merge(df_vars, util_prepare_categories(df_categories = df_categories),
                   by = "VAR_NAMES",
                   all.x = TRUE)

  df_vars$MISSING_LIST <- util_extract_missinglist(df_categories = df_categories, ordered_variables = df_vars$VAR_NAMES)

  attr(df_vars, "extra_cols") <- setdiff(names(df_vars), names(dataquieR::WELL_KNOWN_META_VARIABLE_NAMES))
  attr(df_vars, "missing_cols") <- setdiff(names(dataquieR::WELL_KNOWN_META_VARIABLE_NAMES), names(df_vars))
  return(df_vars)
}
