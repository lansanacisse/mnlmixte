mod_data_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "data", 
          fluidRow(
            column(width = 4,
                   h3("1 - Chargement des données"),
                   radioButtons(ns("file_type"), "Type de fichier :",
                                choices = c("CSV" = "csv", "Excel" = "xlsx"),
                                selected = "csv"),
                   fileInput(ns('file'), "Charger un fichier", accept = c(".csv", ".xlsx")),
                   uiOutput(ns("sep_ui")),
                   
                   h3("2 - Choix des variables"),
                   uiOutput(ns('target')),
                   uiOutput(ns('active_vars'))
            ),
            column(width = 8,
                   h3("Aperçu des données"),
                   DTOutput(ns('table'))
            )
          ),
          fluidRow(
            column(width=12,
                   h3("Résumé des données"),
                   verbatimTextOutput(ns('summary'))
            )
          ),
          fluidRow(
            column(width = 4,
                   h3("3 - Gestion des valeurs manquantes"),
                   h4("Gestion des variables actives quantitatives"),
                   radioButtons(ns("missing_quant_method"), "Méthode :",
                                choices = c("Supprimer les lignes" = "remove_rows",
                                            "Supprimer les colonnes" = "remove_cols",
                                            "Remplir par la moyenne" = "fill_mean",
                                            "Remplir par la médiane" = "fill_median",
                                            "Aucune modification" = "none"),
                                selected = "none"),
                   h4("Gestion des variables actives catégorielles"),
                   radioButtons(ns("missing_cat_method"), "Méthode :",
                                choices = c("Supprimer les lignes" = "remove_rows",
                                            "Supprimer les colonnes" = "remove_cols",
                                            "Remplir par le mode (modalité la plus fréquente)" = "fill_mode",
                                            "Aucune modification" = "none"),
                                selected = "none"),
                   h4("Gestion de la variable cible"),
                   radioButtons(ns("missing_target_method"), "Méthode :",
                                choices = c("Supprimer les lignes" = "remove_rows",
                                            "Remplir par le mode (modalité la plus fréquente)" = "fill_mode",
                                            "Aucune modification" = "none"),
                                selected = "none"),
                   actionButton(ns("handle_missing"), "Appliquer les modifications")
            ),
            column(width = 8,
                   h3("Aperçu après traitement"),
                   DTOutput(ns('cleaned_table'))
            )
          )
  )
}
