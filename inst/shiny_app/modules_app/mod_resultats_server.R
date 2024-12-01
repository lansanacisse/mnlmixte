mod_results_server <- function(id, model, data_info) {
  moduleServer(id, function(input, output, session) {
    
    cleaned_data <- reactive({ data_info()$cleaned_data })
    
    # Affichage table des coefficients
    output$coefficients_table <- renderDT({
      req(model())
      coef_df <- as.data.frame(model()$coefficients)
      colnames(coef_df) <- model()$classes
      #coef_df <- cbind(Feature = rownames(coef_df), coef_df)
      datatable(coef_df, options = list(scrollX = TRUE))
    })
    
    # Affichage graphique des coefficients
    output$coefficients_plot <- renderPlot({
      req(model()$coefficients, model()$classes) 
      # Transformer les coefficients en dataframe
      coef_df <- as.data.frame(model()$coefficients)
      colnames(coef_df) <- model()$classes  
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
      req(model())
      probas <- model()$predict_proba(cleaned_data())
      probas_df <- as.data.frame(probas)
      colnames(probas_df) <- model()$classes
      probas_long <- reshape2::melt(probas_df)
      ggplot(probas_long, aes(x = variable, y = value, fill = variable)) +
        geom_violin() +
        labs(title = "Distribution des probabilités par classe", x = "Classe", y = "Probabilité")
    })
    
    #Affichage graphique de la perte
    output$loss_plot <- renderPlot({
      req(model())
      loss_df <- data.frame(Epoch = seq_along(model()$loss_history), 
                            Loss = model()$loss_history)
      ggplot(loss_df, aes(x = Epoch, y = Loss)) + 
        geom_line(color = "blue", size = 1) +
        labs(title = "Évolution de la perte", x = "Époque", y = "Perte")+
        theme_minimal()
    })
    
    #Affichage des metriques
    output$metrics <- renderPrint({
      req(model(), cleaned_data(), data_info())
      # Filtrer les colonnes qui existent dans cleaned_data()
      existing_vars <- intersect(data_info()$active_vars, colnames(cleaned_data()))
      # Séparation variables actives / variable cible
      X <- cleaned_data()[, existing_vars]
      y <- cleaned_data()[[data_info()$target]]
      model()$evaluate(X, y)
    })
  })
}
