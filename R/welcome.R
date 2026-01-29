#'@name welcome_server
#'@title POC shiny module server
#'@description POC shiny module server
#'@param id id
#'@param lang lang a reactive version of the language. Default is \code{NULL} (optional)
#'@export
welcome_server <- function(id, lang = NULL){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    INFO("Welcome server - START")
    
    #i18n translation mechanism
    #functional for both static language set-up (lang = NULL) or dynamic language set-up
    #(case where the language is passed as reactive)
    i18n_translator <- reactive({
      if(is.reactive(lang)) set_translation_language(lang())
      INFO("Welcome to fdishinyr ecosystem in lang '%s'", translator()$get_translation_language())
      translator()
    })
    
    #i18n util function
    #The function wraps the translator translation (t) function
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
    
    INFO("Welcome server - END")
    
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