function(input, output, session) {
  
####################################   A4   ####################################
  

  # Variable pour ranger la data selon le choix de l'utilisateur
  A4_react <- reactiveValues(data = NULL)

    
  # Commande reliant le bouton "Secteurs principaux"  
  observeEvent(input$secteur1, {
    A4_react$data <- data_enquete_A4 %>%
      filter(!is.na(A4_secteur)) %>%
      count(A4_secteur) %>%
      arrange(desc(n)) %>% 
      mutate(A4_secteur = as_factor(A4_secteur)) %>% 
      dplyr::rename("Secteur"= "A4_secteur")
  })
  # Commande reliant le bouton "Secteurs principaux"  
  observeEvent(input$secteur2, {
    A4_react$data <- data_enquete_A4 %>%
      filter(!is.na(A4_secteur_autre)) %>%
      count(A4_secteur_autre) %>%
      arrange(desc(n)) %>% 
      mutate(A4_secteur_autre = as_factor(A4_secteur_autre)) %>% 
      dplyr::rename("Secteur"= "A4_secteur_autre")
  })
  
  # Commandes pour les sorties
  perimetre_A4 <- reactive({
    temp <- A4_react$data
    return(temp)
  })
  
  output$plot_A4 <- renderPlot({
    if (is.null(A4_react$data)) return()
    perimetre_A4()  %>% 
      ggplot() +
      geom_col(aes(x = Secteur, y = n, fill = Secteur),
               alpha = 0.7) +
      geom_text(aes(x = Secteur, y = n, label = n),
                hjust = 0.5,
                vjust = -0.5) +
      labs(
        title = "Répartition des secteurs d'activités",
        subtitle = "Principaux secteurs",
        x = "Secteurs",
        y = "Réponses"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank()    
      )
  })
  
  output$plot_pie_A4 <- renderPlot({
    if (is.null(A4_react$data)) return()
    temp <- perimetre_A4()
    pie(temp$n, 
        labels = paste0(temp$Secteur, " (", temp$n, ")"),
        border = "white", 
        col = couleur_A4,
        main = "Répartition des secteurs d'activités")
  })
  
  output$table_A4 <- renderTable({
    if (is.null(A4_react$data)) return()
    perimetre_A4() %>% 
      dplyr::rename("Secteur d'activité"= "Secteur") %>% 
      dplyr::rename("Effectif" = "n")
  })
################################################################################ 
  
}
