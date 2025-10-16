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
  # Commande reliant le bouton "Tous les secteurs"  
  observeEvent(input$secteur3, {
    A4_react$data <- data_enquete_A4_fusion %>%
      filter(!is.na(A4_secteur)) %>%
      count(A4_secteur) %>%
      arrange(desc(n)) %>% 
      mutate(A4_secteur = as_factor(A4_secteur)) %>% 
      dplyr::rename("Secteur"= "A4_secteur")
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
        x = "Secteurs",
        y = "Réponses"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank()    
      )
  })
  
  output$plot_pie_A4 <- renderPlot({
    if (is.null(A4_react)) return()
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
  
  
  
####################################   A4   ####################################  
  
  
  # Commandes pour les sorties
  perimetre_A6 <- reactive({
    temp <- data_enquete_A6 %>%
      filter(!is.na(A6_experience)) %>%
      count(A6_experience) %>%
      arrange(A6_experience) %>% 
      mutate(A6_experience = as_factor(A6_experience)) %>% 
      dplyr::rename("Experience"= "A6_experience")
    return(temp)
  })  
  
  output$plot_A6 <- renderPlot({
    perimetre_A6()  %>% 
      ggplot() +
      geom_col(aes(x = Experience, y = n, fill = n),
               alpha = 0.7) +
      geom_text(aes(x = Experience, y = n, label = n),
                hjust = 0.5,
                vjust = -0.5) +
      labs(
        title = "Répartition des secteurs d'activités",
        x = "Années d'expérience",
        y = "Réponses"
      ) +
      theme_minimal()
  })
  
  output$table_A6 <- renderTable({
    perimetre_A6() 
  })
}
