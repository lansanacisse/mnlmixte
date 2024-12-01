mod_modeling_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "modeling",
          fluidRow(
            column(width = 4,
                   h3("Paramètres du Modèle"),
                   numericInput(ns("learning_rate"), "Taux d'apprentissage", value = 0.01, min = 0.001, step = 0.001),
                   numericInput(ns("epochs"), "Nombre d'époques", value = 1000, min = 100, step = 100),
                   numericInput(ns("regularization"), "Régularisation", value = 0.01, min = 0, step = 0.01),
                   selectInput(ns("parallelism"), "Parallélisme calculs", choices = c("TRUE", "FALSE"), selected = "TRUE"),
                   actionButton(ns("train"), "Entraîner le Modèle", disabled = FALSE),
                   p("Attention à ne pas laisser de valeurs manquantes, sinon l'entraînement ne se débloquera pas.")
            ),
            column(width = 8,
                   h2("Résumé du Modèle"),
                   verbatimTextOutput(ns("model_summary"))
            )
          )
  )
}
