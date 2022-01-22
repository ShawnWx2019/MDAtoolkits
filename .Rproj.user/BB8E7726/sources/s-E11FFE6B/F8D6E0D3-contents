#' Access files in the current app
#' 
#' NOTE: If you manually change your package name in the DESCRIPTION, 
#' don't forget to change it here too, and in the config file. 
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#' 
#' @param ... character vectors, specifying subdirectory and file(s) 
#' within your package. The default, none, returns the root of the app. 
#' 
#' @noRd
app_sys <- function(...){
  system.file(..., package = "MDAtoolkits")
}


#' Read App Config
#' 
#' @param value Value to retrieve from the config file. 
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE. 
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' 
#' @noRd
get_golem_config <- function(
  value, 
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE", 
    Sys.getenv(
      "R_CONFIG_ACTIVE", 
      "default"
    )
  ), 
  use_parent = TRUE
){
  config::get(
    value = value, 
    config = config, 
    # Modify this if your config file is somewhere else:
    file = app_sys("golem-config.yml"), 
    use_parent = use_parent
  )
}

library(bslib)
library(shiny)
library(rlang)
library(curl)
library(shinyjqui)
library(shinyjs)

theme <- bs_theme(
  bg = "#0b3d91", fg = "white", primary = "#FCC780",
  #base_font = font_google("Quicksand"),
  base_font = font_google("Kalam"),
  code_font = font_google("Space Mono")
)
# bs_theme_preview(theme)

# enlarged auto fonts
if (is_installed("thematic")) {
  thematic::thematic_shiny(
    font = thematic::font_spec("auto", scale = 2, update = TRUE)
  )
}

# Source in ggplot2 examples

theme <- bs_global_get()
if ("3" %in% theme_version(theme)) {
  warning("This example app requires Bootstrap 4 or higher", call. = FALSE)
}

rounded <- isTRUE(as.logical(bs_get_variables(theme %||% bslib::bs_theme(), "enable-rounded")))

#' pill tabpanel
#' 
#' @param ... same as tabPanle(). 
#' 
#' @noRd


pill <- function(...) {
  shiny::tabPanel(..., class = "p-3 border", class = if (rounded) "rounded")
}

#' tab tabpanel
#' 
#' @param ... same as tabPanle(). 
#' 
#' @noRd

tab <- function(...) {
  shiny::tabPanel(..., class = "p-3 border border-top-0", class = if (rounded) "rounded-bottom")
}

#' gradient theme
#' 
#' @param theme_color see bslib. 
#' 
#' @noRd
gradient <- function(theme_color = "primary") {
  bg_color <- paste0("bg-", theme_color)
  bgg_color <- paste0("bg-gradient-", theme_color)
  bg_div <- function(color_class, ...) {
    div(
      class = "p-3", class = color_class,
      paste0(".", color_class), ...
    )
  }
  fluidRow(
    column(6, bg_div(bg_color)),
    column(6, bg_div(bgg_color))
  )
}