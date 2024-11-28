library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(readxl)
library(ggplot2)
library(dplyr)

#Source 
source("mnlmixte.R")

# Interface utilisateur
ui <- dashboardPage(
  
  # Titre de l'application
  dashboardHeader(title = "Régression Logistique Multinomiale"),
  
  # Barre latérale
  dashboardSidebar(
    sidebarMenu(
      # Menu barre latérale
      menuItem("Accueil", tabName = "home", icon = icon("home")),
      menuItem("Données", tabName = "data", icon = icon("database")),
      menuItem("Modélisation", tabName = "modeling", icon = icon("calculator")),
      menuItem("Résultats", tabName = "results", icon = icon("chart-bar"))  
    )
  ),
  
  # Affichage principal
  dashboardBody(
    # Activer shinyjs pour gérer l'état des boutons
    useShinyjs(),  
    
    # Onglets
    tabItems(
      
      # Onglet Accueil
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 6,
            h2("Bienvenue dans l'application de Régression Logistique Multinomiale"),
            p("Ce travail à été réalisé dans le cadre du cours de Programmation R en Master SISE."),
            p("Cette application vous permet de :"),
            tags$ul(
              tags$li("Charger et préparer vos données."),
              tags$li("Configurer et entraîner un modèle de régression logistique multinomiale."),
              tags$li("Visualiser les résultats et interpréter les coefficients.")
            ),
            p("Utilisez le menu à gauche pour naviguer entre les différentes étapes."),
            h4("Ressources utiles"),
            tags$ul(
              tags$li(a("Documentation de l'application", href = "https://example.com", target = "_blank")),
              tags$li(a("Introduction à la régression logistique multinomiale", href = "https://example.com", target = "_blank"))
            )
          ),
          column(
            width = 6,
            h3("Flux de l'application"),
            img(src = "workflow_image.png", height = "300px", alt = "Diagramme du flux")
          )
        )
      ),
      
      # Onglet chargement des données et choix des variables
      tabItem(tabName = "data", 
              # Structure en colonnes
              fluidRow(
                # Colonne de gauche : Chargement et sélection des variables
                column(width = 4,
                       h3("1 - Chargement des données"),
                       radioButtons("file_type", "Type de fichier :",
                                    choices = c("CSV" = "csv", "Excel" = "xlsx"),
                                    selected = "csv"),
                       
                       fileInput('file', "Charger un fichier", accept = c(".csv", ".xlsx")),
                  
                       conditionalPanel(
                         condition = "input.file_type == 'csv'", 
                         radioButtons("sep", "Séparateur :",
                                      choices = c("Point-virgule (;)" = ";",
                                                  "Tabulation (\\t)" = "\t",
                                                  "Virgule (,)" = ","),
                                      selected = ";")
                       ),
                       # Choix des variables
                       h3("2 - Choix des variables"),
                       uiOutput('target'),
                       uiOutput('active_vars'),
                       # Résumé des données
                       h3("Résumé des données"),
                       verbatimTextOutput('summary')
                ),
                
                # Colonne de droite : Affichage du tableau de données
                column(width = 8,
                       h3("Aperçu des données"),
                       DTOutput('table')
                )
              ),
              
              # Structure en colonnes
              fluidRow(
                # Colonne de gauche : Gestion des valeurs manquantes - Variables quantitatives et qualitatives
                column(width=4,
                       h3("3 - Gestion des valeurs manquantes"),
                       h4("Gestion des variables quantitatives"),
                       radioButtons("missing_quant_method", "Méthode :",
                                    choices = c("Supprimer les lignes" = "remove_rows",
                                                "Supprimer les colonnes" = "remove_cols",
                                                "Remplir par la moyenne" = "fill_mean",
                                                "Remplir par la médiane" = "fill_median"),
                                    selected = "fill_mean"),
               
                       h4("Gestion des variables catégorielles"),
                       radioButtons("missing_cat_method", "Méthode :",
                                    choices = c("Supprimer les lignes" = "remove_rows",
                                                "Supprimer les colonnes" = "remove_cols",
                                                "Remplir par le mode (modalité la plus fréquente)" = "fill_mode"),
                                    selected = "fill_mode"),
                       actionButton("handle_missing", "Appliquer les modifications")
                ),
                
                #Colonne de droite - Apercu des données après traitement
                column(width = 8,
                       h3("Aperçu après traitement"),
                       DTOutput('cleaned_table')
                )
        )
      ),
      
      # Onglet Modèlisation
      tabItem(tabName = "modeling",
              fluidRow(
                column(
                  width=4,
                  # Configuration des paramètres
                  h3("Paramètres du Modèle"),
                  numericInput("learning_rate", "Taux d'apprentissage", value = 0.01, min = 0.001, step = 0.001),
                  numericInput("epochs", "Nombre d'époques", value = 1000, min = 100, step = 100),
                  numericInput("regularization", "Régularisation", value = 0.01, min = 0, step = 0.01),
                  # Bouton entrainement
                  actionButton("train", "Entraîner le Modèle")
                ),
                column(
                  width=8,
                  # Affichage résumé
                  h2("Résumé du Modèle"),
                  verbatimTextOutput("model_summary")
                )
              )
      ),
      
      # Onglet Résultats
      tabItem(
        tabName = "results",
        tabsetPanel(
          tabPanel(
            #Tableau coefficients
            title = "Coefficients",
            fluidRow(
              column(
                width=12,
                h3("Tableau des coefficients"),
                DTOutput('coefficients_table')
              )
            ),
            fluidRow(
              column(
                width=12,
                #Visualisation graphique des coefficients
                h2("Visualisation des coefficients"),
                plotOutput('coefficients_plot')
              )
            )
          ),
          tabPanel(
              title = "Probabilités Prédictives",
              fluidRow(
                column(
                  width=12,
                  #Visuaisation des probas par classe
                  h3("Visualisation des probabilités prédicitives"),
                  plotOutput('proba_distribution')
                )
              )
            ),
          tabPanel(
            title = "Évolution de la Perte",
            fluidRow(
              column(
                width = 12,
                #Visualisation graphique de la perte
                h3("Graphique de la perte par époque"),
                plotOutput('loss_plot')
              )
            )
          ),
          tabPanel(
            title = "Metriques",
            fluidRow(
              column(
                width = 12,
                # Visualisation des metriques 
                h3("Métriques du modèle"),
                verbatimTextOutput('metrics')
              )
            )
          )
        )
      )     
    )
  )
)


# Serveur
server <- function(input, output, session) {
  # Activer shinyjs pour gérer l'état des boutons
  #useShinyjs()
  
  # Désactive les boutons
  disable("handle_missing")
  disable("train")
  
  
  # Chargement du fichier CSV ou Excel
  data <- reactive({
    # On s'assure que le fichier est bien chargé
    req(input$file)
    
    # Lecture du fichier selon le type sélectionné
    if (input$file_type == "csv") {
      read.csv(input$file$datapath, sep = input$sep, fill = TRUE)
    } else if (input$file_type == "xlsx") {
      read_excel(input$file$datapath)
    }
  })
  
  # Mise a jour dynamique des options des variables 
  observe({
    req(data())
    columns_names <- names(data())
    
    # Mise à jour des choix pour la variable cible 
    updateSelectInput(session, 'target', choices= columns_names)
    
    # Mise à jour des choix pour les variabels actives 
    updateSelectizeInput(session, 'active_vars', choices = columns_names, server = TRUE)
  })
  
  # Observe le changement de la variable cible et met à jour la liste des variables actives
  observeEvent(input$target, {
    req(data())
    columns_names <- names(data())
    
    # Mise à jour des choix pour les variables actives en excluant la variable cible sélectionnée
    updateSelectizeInput(session, 'active_vars', 
                         choices = setdiff(columns_names, input$target), 
                         server = TRUE)
  })
  
  # Observe l'événement des sélections de variables
  observe({
    req(input$target, input$active_vars)
    
    # Active le bouton de gestion des valeurs manquantes si une variable cible et des variables actives sont choisies
    if (!is.null(input$target) && length(input$active_vars) > 0) {
      shinyjs::enable("handle_missing")  # Réactive le bouton
    } else {
      shinyjs::disable("vars_choices")  # Désactive le bouton si les sélections sont invalides
    }
  })
  
  # Sélection de la variable cible 
  output$target <- renderUI({
    req(data())
    selectInput('target', "Variable cible :", choices = NULL)
  })
  
  #Sélection des variables actives 
  output$active_vars <- renderUI({
    req(data())
    selectizeInput('active_vars', "Variables actives :", choices = NULL, multiple = TRUE)
  })
  
  # Affichage du contenu du fichier
  output$table <- renderDT({
    datatable(data(), options = list(scrollX = TRUE))
  })
  
  # Résumé des données
  output$summary <- renderPrint({
    req(data())
    summary(data())
  })
  
  # Gestion des valeurs manquantes
  
  #Objet pour stocker les données traitées 
  cleaned_data <- reactiveVal(NULL)
  
  observeEvent(input$handle_missing, {
    req(data())
    df <- data()  # Copie des données initiales
    
    # Traitement des variables quantitatives
    if (input$missing_quant_method == "remove_rows") {
      df <- df[complete.cases(df), ]  # Supprimer lignes avec des NA
    } else if (input$missing_quant_method == "remove_cols") {
      df <- df[, colSums(is.na(df)) == 0]  # Supprimer colonnes avec des NA
    }  else if (input$missing_quant_method == "fill_mean") {
      df <- df %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
    } else if (input$missing_quant_method == "fill_median") {
      df <- df %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
    }
    
    # Traitement des variables catégorielles
    if (input$missing_cat_method == "fill_mode") {
      mode <- function(x) {
        ux <- unique(x[!is.na(x)])
        ux[which.max(tabulate(match(x, ux)))]
      }
      df <- df %>% mutate(across(where(is.factor), ~ ifelse(is.na(.), mode(.), .)))
    } 
    
    # Mise à jour des données traitées
    cleaned_data(df)
  })
  
  # Affichage des données traitées
  output$cleaned_table <- renderDT({
    req(cleaned_data())
    datatable(cleaned_data(), options = list(scrollX = TRUE))
  })
  
  # Observe l'événement de clic sur le bouton "Appliquer les modifications"
  observeEvent(input$handle_missing, {
    #Réactiver le bouton "Entrainer le modèle 
    shinyjs::enable("train")
  })
  
  # Entrainement du modèle
  # Initialiser un objet reactiveValues pour stocker le modèle
  values <- reactiveValues(model = NULL)
  
  observeEvent(input$train, {
    # Barre de chargement
    withProgress(message = "Entraînement en cours...", value = 0, {
      #Entrainement
      req(cleaned_data(), input$target, input$active_vars)
      X <- cleaned_data()[, input$active_vars]
      y <- cleaned_data()[[input$target]]
      values$model <- MNLMIXTE$new(learning_rate = input$learning_rate, epochs = input$epochs, regularization = input$regularization)
      values$model$fit(X, y)
      output$model_summary <- renderPrint({ values$model$summary() })
    })
  })
  
  # Affichage table des coefficients
  output$coefficients_table <- renderDT({
    req(values$model$coefficients)
    coef_df <- as.data.frame(values$model$coefficients)
    colnames(coef_df) <- values$model$classes
    coef_df <- cbind(Feature = rownames(coef_df), coef_df)
    datatable(coef_df, options = list(scrollX = TRUE))
  })
  
  # Affichage graphique des coefficients
  output$coefficients_plot <- renderPlot({
    req(values$model$coefficients, values$model$classes) 
    # Transformer les coefficients en dataframe
    coef_df <- as.data.frame(values$model$coefficients)
    colnames(coef_df) <- values$model$classes  
    coef_df$Feature <- rownames(coef_df)       
    # Restructurer les données en format long pour ggplot2
    coef_long <- reshape2::melt(coef_df, id.vars = "Feature", variable.name = "Classe", value.name = "Coefficient")
    # Graphique
    ggplot(coef_long, aes(x = Feature, y = Coefficient, fill = Classe)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Coefficients des variables explicatives par classe",
           x = "Variables explicatives", y = "Coefficients", fill = "Classes") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Affichage graphique de la distribution des probabilités par classe
  output$proba_distribution <- renderPlot({
    req(values$model$predict_proba)
    probas <- values$model$predict_proba(data())
    probas_df <- as.data.frame(probas)
    colnames(probas_df) <- values$model$classes 
    # Restructurer les données en format long pour ggplot2
    probas_long <- reshape2::melt(probas_df)
    ggplot(probas_long, aes(x = variable, y = value, fill = variable)) +
      geom_violin() +
      labs(title = "Distribution des probabilités par classe",
           x = "Classe", y = "Probabilité") +
      theme_minimal()
  })
  
  #Affichage graphique de la perte
  output$loss_plot <- renderPlot({
    req(values$model$loss_history)
    loss_df <- data.frame(Epoch = seq_along(values$model$loss_history),
                          Loss = values$model$loss_history)
    ggplot(loss_df, aes(x = Epoch, y = Loss)) +
      geom_line(color = "blue", size = 1) +
      labs(title = "Évolution de la perte au cours des époques",
           x = "Époque", y = "Perte") +
      theme_minimal()
  })
  
  #Affichage des metriques
  output$metrics <- renderPrint({
    req(cleaned_data())
    X <- cleaned_data()[, input$active_vars]
    y <- cleaned_data()[[input$target]]
    values$model$evaluate(X, y)
  })
}


# Lancement de l'application
shinyApp(ui = ui, server = server)
