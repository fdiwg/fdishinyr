.onLoad <- function (libname, pkgname) {
  
  assign(".fdishinyr", new.env(), envir= asNamespace(pkgname))
  
  .fdishinyr$translator <- shiny.i18n::Translator$new(
    translation_csvs_path = system.file("extdata", "i18n/translations", package = "fdishinyr"),
    translation_csv_config = system.file("extdata", "i18n/config.yml", package = "fdishinyr")
  )
  .fdishinyr$translator$set_translation_language("en")
}