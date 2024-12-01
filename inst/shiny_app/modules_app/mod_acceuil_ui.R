mod_home_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "home",
    fluidRow(
      column(width = 6,
             h2("Bienvenue dans l'application de Régression Logistique Multinomiale"),
             p("Ce travail à été réalisé dans le cadre du cours de Programmation R en Master SISE."),
             p("Cette application vous permet de :"),
             tags$ul(
               tags$li("Charger et préparer vos données."),
               tags$li("Configurer et entraîner un modèle de régression logistique multinomiale."),
               tags$li("Visualiser les résultats et interpréter les coefficients.")
             ),
             p("Utilisez le menu à gauche pour naviguer entre les différentes étapes."),
      ),
      column(width = 6,
             h3("Flux de l'application : "),
             img(src = "workflow_image.png", height = "800px", alt = "Diagramme du flux",
                 style = "display: block; margin-left: auto; margin-right: auto;")
      )
    )
  )
}
