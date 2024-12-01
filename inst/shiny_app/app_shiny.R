library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(readxl)
library(ggplot2)

#Source classe mnlmixte
source("../../R/mnlmixte.R")

#Source modules UI
source("modules_app/mod_sidebar_ui.R")
source("modules_app/mod_acceuil_ui.R")
source("modules_app/mod_donnes_ui.R")
source("modules_app/mod_modelisation_ui.R")
source("modules_app/mod_resultats_ui.R")

#Source modules server
source("modules_app/mod_donnees_server.R")
source("modules_app/mod_modelisation_server.R")
source("modules_app/mod_resultats_server.R")

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Régression Logistique Multinomiale"),
  dashboardSidebar(mod_sidebar_ui("sidebar")),
  dashboardBody(
    tabItems(
      mod_home_ui("home"),
      mod_data_ui("data"),
      mod_modeling_ui("modeling"),
      mod_results_ui("results")
    )
  )
)


#Paramètre pour augmenter la limite de chargement à 30Mo
options(shiny.maxRequestSize = 30*1024^2)

# Serveur
server <- function(input, output, session) {
  data_info <- mod_data_server("data")
  model <- mod_modeling_server("modeling", data_info)
  mod_results_server("results", model, data_info)
}

# Lancement de l'application
shinyApp(ui = ui, server = server)
