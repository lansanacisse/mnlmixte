library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(readxl)
library(ggplot2)
library(dplyr)

#Source 
source("mnlmixte.R")
source("modules_app/mod_sidebar_ui.R")
source("modules_app/mod_acceuil_ui.R")
source("modules_app/mod_donnes_ui.R")
source("modules_app/mod_modelisation_ui.R")
source("modules_app/mod_resultats_ui.R")

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
  # Activer shinyjs pour gérer l'état des boutons
  useShinyjs()
  
  # Désactive les boutons
  shinyjs::disable("handle_missing")
  shinyjs::disable("train")
  
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
  
  # Objet pour stocker les données traitées 
  cleaned_data <- reactiveVal(NULL)
  
  observeEvent(input$handle_missing, {
    req(data())
    df <- data()
    selected_vars <- c(input$target, input$active_vars)
    
    # Filtre les colonnes de df pour ne garder que celles sélectionnées
    df <- df[, selected_vars, drop = FALSE]
    
    # Gestion des valeurs manquantes pour les variables quantitatives
    quantitative_cols <- sapply(df, is.numeric)  # Identifier les colonnes quantitatives
    if (input$missing_quant_method == "remove_rows") {
      df <- df[complete.cases(df[, quantitative_cols]), ]  # Supprime les lignes avec des valeurs manquantes dans les variables quantitatives
    } else if (input$missing_quant_method == "remove_cols") {
      df <- df[, colSums(is.na(df[, quantitative_cols])) == 0]  # Supprime les colonnes quantitatives avec des valeurs manquantes
    } else if (input$missing_quant_method == "fill_mean") {
      df[, quantitative_cols] <- lapply(df[, quantitative_cols], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))  # Remplir par la moyenne
    } else if (input$missing_quant_method == "fill_median") {
      df[, quantitative_cols] <- lapply(df[, quantitative_cols], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))  # Remplir par la médiane
    }
    
    # Gestion des valeurs manquantes pour les variables catégorielles
    categorical_cols <- sapply(df, is.factor) | sapply(df, is.character)  # Identifier les colonnes catégorielles
    target_name <- input$target
    categorical_cols[target_name] <- FALSE #Exclusion de la variable cible
    
    if (input$missing_cat_method == "remove_rows") {
      df <- df[complete.cases(df[, categorical_cols]), ]  # Supprime les lignes avec des valeurs manquantes dans les variables catégorielles
    } else if (input$missing_cat_method == "remove_cols") {
      df <- df[, colSums(is.na(df[, categorical_cols])) == 0]  # Supprime les colonnes catégorielles avec des valeurs manquantes
    } else if (input$missing_cat_method == "fill_mode") {
      df[, categorical_cols] <- lapply(df[, categorical_cols], function(x) {
        mode_value <- names(sort(table(x), decreasing = TRUE))[1]  # Mode (modalité la plus fréquente)
        ifelse(is.na(x), mode_value, x)
      })  
    }
    
    # Gestion des valeurs manquantes pour la variable cible
    target_name <- input$target
    target_col <- df[[target_name]] 
    
    if (input$missing_target_method == "remove_rows") {
      df <- df[complete.cases(df[, target_name]), ]  # Supprime les lignes avec des valeurs manquantes dans la variable cible
    } else if (input$missing_target_method == "fill_mode") {
      mode_value <- names(sort(table(target_col), decreasing = TRUE))[1]
      df[[target_name]] <- ifelse(is.na(target_col), mode_value, target_col)  # Remplir la variable cible par le mode
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
  observe({
    # Récupérer les données nettoyées
    data <- cleaned_data()
    
    # Vérifier s'il reste des valeurs manquantes
    if (!is.null(data) && any(is.na(data))) {
      # Désactiver le bouton "train" s'il reste des NA
      shinyjs::disable("train")
    } else {
      # Activer le bouton "train" s'il n'y a plus de NA
      shinyjs::enable("train")
    }
  })
  
  # Entrainement du modèle
  # Initialiser un objet reactiveValues pour stocker le modèle
  values <- reactiveValues(model = NULL)
  
  observeEvent(input$train, {
    # Barre de chargement
    withProgress(message = "Entraînement en cours...", value = 0, {
      #Entrainement
      req(cleaned_data(), input$target, input$active_vars)
      # Filtrer les colonnes qui existent dans cleaned_data()
      existing_vars <- intersect(input$active_vars, colnames(cleaned_data()))
      #Séparation variables actives / variable cible
      X <- cleaned_data()[, existing_vars]
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
    probas <- values$model$predict_proba(cleaned_data())
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
    # Filtrer les colonnes qui existent dans cleaned_data()
    existing_vars <- intersect(input$active_vars, colnames(cleaned_data()))
    # Séparation variables actives / variable cible
    X <- cleaned_data()[, existing_vars]
    y <- cleaned_data()[[input$target]]
    values$model$evaluate(X, y)
  })
}


# Lancement de l'application
shinyApp(ui = ui, server = server)
