mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  sidebarMenu(
    menuItem("Accueil", tabName = "home", icon = icon("home")),
    menuItem("Données", tabName = "data", icon = icon("database")),
    menuItem("Modélisation", tabName = "modeling", icon = icon("calculator")),
    menuItem("Résultats", tabName = "results", icon = icon("chart-bar"))
  )
}
