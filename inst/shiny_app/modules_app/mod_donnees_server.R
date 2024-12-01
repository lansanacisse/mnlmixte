mod_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data <- reactive({
      req(input$file)
      if (input$file_type == "csv") {
        req(input$sep)  # Vérifier la présence de `sep` si le fichier est un CSV
        read.csv(input$file$datapath, sep = input$sep, fill = TRUE)
      } else if (input$file_type == "xlsx") {
        read_excel(input$file$datapath)
      }
    })
    
    # Générer dynamiquement l'UI du séparateur pour le fichier CSV
    output$sep_ui <- renderUI({
      if (input$file_type == "csv") {
        radioButtons(ns("sep"), "Séparateur :",
                     choices = c("Point-virgule (;)" = ";",
                                 "Tabulation (\\t)" = "\t",
                                 "Virgule (,)" = ","),
                     selected = ";")
      }
    })
    
    # Rendre dynamiquement les éléments selectInput et selectizeInput
    observe({
      req(data())
      columns_names <- names(data())
      
      # Mise à jour de la variable cible
      updateSelectInput(session, 'target', choices = columns_names, selected = columns_names[1])
      
      # Mise à jour des variables actives
      updateSelectizeInput(session, 'active_vars', choices = columns_names, server = TRUE)
    })
    
    # Sélection de la variable cible
    output$target <- renderUI({
      req(data())
      selectInput(ns('target'), "Variable cible :", choices = NULL)
    })
    
    # Sélection des variables actives
    output$active_vars <- renderUI({
      req(data())
      selectizeInput(ns('active_vars'), "Variables actives :", choices = NULL, multiple = TRUE)
    })
    
    # Mise à jour dynamique des variables actives en excluant la variable cible sélectionnée
    observeEvent(input$target, {
      req(data())
      columns_names <- names(data())
      
      # Exclure la variable cible des variables actives
      updateSelectizeInput(session, 'active_vars', 
                           choices = setdiff(columns_names, input$target), 
                           server = TRUE)
    })
    
    # Affichage du tableau de données
    output$table <- renderDT({
      req(data())
      datatable(data(), options = list(scrollX = TRUE))
    })
    
    # Affichage du résumé des données
    output$summary <- renderPrint({
      req(data())
      summary(data())
    })
    
    #Gestion des NA
    # Réactive pour stocker les données nettoyées
    cleaned_data <- reactiveVal(NULL)
    
    observeEvent(input$handle_missing, {
      req(data())  
      df <- data()
      selected_vars <- c(input$target, input$active_vars)  # Variables sélectionnées
      # Filtre les colonnes de df pour ne garder que celles sélectionnées
      df <- df[, selected_vars, drop = FALSE]  
      
      # 1. Gestion des valeurs manquantes pour les variables quantitatives
      quantitative_cols <- sapply(df, is.numeric)  # Vérifier les colonnes quantitatives
      if (input$missing_quant_method == "remove_rows") {
        df <- df[complete.cases(df[, quantitative_cols]), ]  # Supprime les lignes avec des valeurs manquantes dans les variables quantitatives
      } else if (input$missing_quant_method == "remove_cols") {
        df <- df[, colSums(is.na(df[, quantitative_cols])) == 0]  # Supprime les colonnes quantitatives avec des valeurs manquantes
      } else if (input$missing_quant_method == "fill_mean") {
        df[, quantitative_cols] <- lapply(df[, quantitative_cols], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))  # Remplir par la moyenne
      } else if (input$missing_quant_method == "fill_median") {
        df[, quantitative_cols] <- lapply(df[, quantitative_cols], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))  # Remplir par la médiane
      }
      
      # 2. Gestion des valeurs manquantes pour les variables catégorielles
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
      
      # 3. Gestion de la variable cible
      target_name <- input$target
      target_col <- df[[target_name]] 
      
      if (input$missing_target_method == "remove_rows") {
        df <- df[complete.cases(df[, target_name]), ]  # Supprime les lignes avec des valeurs manquantes dans la variable cible
      } else if (input$missing_target_method == "fill_mode") {
        mode_value <- names(sort(table(target_col), decreasing = TRUE))[1]
        df[[target_name]] <- ifelse(is.na(target_col), mode_value, target_col)  # Remplir la variable cible par le mode
      }
      
      # Mise à jour des données nettoyées
      cleaned_data(df)
    })
    
    # Affichage de la table après traitement
    output$cleaned_table <- renderDT({
      req(cleaned_data()) 
      datatable(cleaned_data(), options = list(scrollX = TRUE))  # Afficher la table nettoyée
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
    
    return(reactive({
      list(
        cleaned_data = cleaned_data(),
        target = input$target,
        active_vars = input$active_vars
      )
    }))
  })
}
