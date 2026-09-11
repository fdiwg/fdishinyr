#' FDI shiny utils
#'
#' fdishinyr provides common widgets for use with shiny.
#' 
#' @importFrom stats qnorm sd setNames
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data .env
#' @rawNamespace import(shiny, except = c(tabsetPanel,column))
#' @import shiny.i18n
#' @importFrom shinycssloaders withSpinner
#' @importFrom bs4Dash dashboardPage dashboardHeader dashboardBrand dashboardSidebar dashboardControlbar dashboardBody tabsetPanel box bs4InfoBox valueBox column
#' @import htmlwidgets
#' @import rlang
#' @import plotly
#' @import scales
#' @importFrom DT datatable renderDT DTOutput formatRound
#' 
#' @name fdishinyr
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' 
"_PACKAGE"
