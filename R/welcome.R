#'@name welcome_server
#'@title POC shiny module server
#'@description POC shiny module server
#'@param id id
#'@param lang lang a reactive version of the language. Default is \code{NULL} (optional)
#'@export
welcome_server <- function(id, lang = NULL){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #default
    i18n_translator <- reactive({
      if(is.reactive(lang)) set_translation_language(lang())
      print(sprintf("Welcome to fdishinyr ecosystem in lang '%s'", translator()$get_translation_language()))
      translator()
    })
    i18n <- function(key){
      i18n_translator()$t(key)
    }
    
    output$main <- renderUI({
      bs4Dash::box(
        title = i18n("WELCOME"),
        width = 12,
        status = "primary",
        tags$p(i18n("WELCOME_PARAGRAPH"))
      )
    })
  })
  
}

#'@name welcome_ui
#'@title POC shiny module UI
#'@description POC shiny module UI
#'@param id id
#'@export
welcome_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main"))
}