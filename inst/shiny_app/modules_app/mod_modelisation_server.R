mod_modeling_server <- function(id, data_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    values <- reactiveValues(model = NULL)
    
    observeEvent(input$train, {
      # Barre de chargement
      withProgress(message = "Entraînement en cours...", value = 0, {
        #Entrainement
        req(data_info())
        
        # Filtrer les colonnes qui existent dans cleaned_data()
        existing_vars <- intersect(data_info()$active_vars, colnames(data_info()$cleaned_data))
        #Séparation variables actives / variable cible
        X <- data_info()$cleaned_data[, existing_vars]
        y <- data_info()$cleaned_data[[data_info()$target]]
        values$model <- MNLMIXTE$new(learning_rate = input$learning_rate, epochs = input$epochs, regularization = input$regularization)
        values$model$fit(X, y)
      })
    })
    
    # Résumé du modèle
    output$model_summary <- renderPrint({
      req(values$model)  
      values$model$summary()
    })
    
    # Retourner le modèle
    return(reactive ({values$model}) )
  })
}