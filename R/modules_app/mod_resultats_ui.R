mod_results_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "results",
    tabsetPanel(
      tabPanel("Coefficients",
               fluidRow(column(width = 12, h3("Tableau des coefficients"), DTOutput(ns('coefficients_table')))),
               fluidRow(column(width = 12, h2("Visualisation des coefficients"), plotOutput(ns('coefficients_plot'))))
      ),
      tabPanel("Probabilités Prédictives",
               fluidRow(column(width = 12, h3("Visualisation des probabilités prédictives"), plotOutput(ns('proba_distribution'))))
      ),
      tabPanel("Évolution de la Perte",
               fluidRow(column(width = 12, h3("Graphique de la perte par époque"), plotOutput(ns('loss_plot'))))
      ),
      tabPanel("Métriques",
               fluidRow(column(width = 12, h3("Métriques du modèle"), verbatimTextOutput(ns('metrics'))))
      )
    )
  )
}
