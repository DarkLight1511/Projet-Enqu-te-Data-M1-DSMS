function(input, output, session) {
  
  ####################################   A4   ####################################
  
  
  # Variable pour ranger la data selon le choix de l'utilisateur
  choix_A4 <- reactive({
    temp <- data_enquete_A4
    # Filtre sur le poste
    if (input$Choix_poste_A4 == "Tous les postes") {
      temp <- temp
    } else {
      temp <- temp %>% filter(A3_poste == input$Choix_poste_A4)
    }
    # Filtre sur le genre
    if (input$Choix_genre_A4 == "Tous") {
      temp <- temp
    } else {
      if (input$Choix_genre_A4 == "Homme") {
        temp <- temp%>% filter(Y1A_genre == "Un homme")
      }
      if (input$Choix_genre_A4 == "Femme") {
        temp <- temp%>% filter(Y1A_genre == "Une femme")
      }
    }
    return(temp)
  })
  
  choix_A4_fusion <- reactive({
    temp <- data_enquete_A4_fusion
    # Filtre sur le poste
    if (input$Choix_poste_A4 == "Tous les postes") {
      temp <- temp
    } else {
      temp <- temp %>% filter(A3_poste == input$Choix_poste_A4)
    }
    # Filtre sur le genre
    if (input$Choix_genre_A4 == "Tous") {
      temp <- temp
    } else {
      if (input$Choix_genre_A4 == "Homme") {
        temp <- temp%>% filter(Y1A_genre == "Un homme")
      }
      if (input$Choix_genre_A4 == "Femme") {
        temp <- temp%>% filter(Y1A_genre == "Une femme")
      }
    }
    return(temp)
  })
  
  A4_react <- reactiveValues(data = NULL)
  couleur_A4 <- brewer.pal(12, "Set3") 
  
  # Commande reliant le bouton "Autres secteurs"  
  observeEvent(list(input$secteur2, input$Choix_poste_A4, input$Choix_genre_A4), {
    A4_react$data <- choix_A4() %>%
      filter(!is.na(A4_secteur_autre)) %>%
      count(A4_secteur_autre) %>%
      arrange(desc(n)) %>% 
      mutate(A4_secteur_autre = as_factor(A4_secteur_autre)) %>% 
      dplyr::rename("Secteur"= "A4_secteur_autre")
  })
  # Commande reliant le bouton "Tous les secteurs"  
  observeEvent(list(input$secteur3, input$Choix_poste_A4, input$Choix_genre_A4), {
    A4_react$data <- choix_A4_fusion() %>%
      filter(!is.na(A4_secteur)) %>%
      count(A4_secteur) %>%
      arrange(desc(n)) %>% 
      mutate(A4_secteur = as_factor(A4_secteur)) %>% 
      dplyr::rename("Secteur"= "A4_secteur")
  })  
  # Commande reliant le bouton "Secteurs principaux"  
  observeEvent(list(input$secteur1, input$Choix_poste_A4, input$Choix_genre_A4), {
    A4_react$data <- choix_A4() %>%
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
  
  
  
####################################   A5   ####################################
  
  choix_A5 <- reactive({
    temp <- data_enquete_A5
    
    # Filtre sur le poste
    if (input$Choix_poste_A5 != "Tous les postes") {
      temp <- temp %>% filter(A3_poste == input$Choix_poste_A5)
    }
    
    # Filtre sur le genre
    if (input$Choix_genre_A5 != "Tous") {
      if (input$Choix_genre_A5 == "Homme") {
        temp <- temp %>% filter(Y1A_genre == "Un homme")
      }
      if (input$Choix_genre_A5 == "Femme") {
        temp <- temp %>% filter(Y1A_genre == "Une femme")
      }
    }
    
    return(temp)
  })
  
  
  A5_react <- reactiveValues(data = NULL)
  couleur_A5 <- brewer.pal(5, "Set2")
  
  
  observeEvent(
    list(input$Choix_poste_A5, input$Choix_genre_A5),
    {
      A5_react$data <- choix_A5() %>%
        filter(!is.na(A5_teletravail)) %>%
        count(A5_teletravail) %>%
        arrange(desc(n)) %>%
        mutate(A5_teletravail = as_factor(A5_teletravail)) %>%
        dplyr::rename("Fréquence" = "A5_teletravail")
    }
  )
  
  
  perimetre_A5 <- reactive({
    A5_react$data
  })
  
  
  output$plot_A5 <- renderPlot({
    if (is.null(A5_react$data)) return()
    
    perimetre_A5() %>%
      ggplot() +
      geom_col(aes(x = Fréquence, y = n, fill = Fréquence),
               alpha = 0.7) +
      geom_text(aes(x = Fréquence, y = n, label = n),
                vjust = -0.5) +
      labs(
        title = "Fréquence de télétravail",
        x = "Fréquence",
        y = "Réponses"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank()    
      )
  })
  
  
  output$plot_pie_A5 <- renderPlot({
    if (is.null(A5_react$data)) return()
    
    temp <- perimetre_A5()
    pie(
      temp$n,
      labels = paste0(temp$Fréquence, " (", temp$n, ")"),
      col = couleur_A5,
      border = "white",
      main = "Fréquence de télétravail"
    )
  })
  
  
  output$table_A5 <- renderTable({
    if (is.null(A5_react$data)) return()
    
    perimetre_A5() %>%
      dplyr::rename("Fréquence de télétravail" = "Fréquence") %>%
      dplyr::rename("Effectif" = "n")
  })
  
  
################################################################################   
  
  
####################################   A6   ####################################  
  
choix_A6 <- eventReactive(
    list(input$Choix_poste_A6, input$Choix_genre_A6),
    {
      temp <- data_enquete_A6
      
      # Filtre sur le poste
      if (input$Choix_poste_A6 == "Tous les postes") {
        temp <- temp
      } else {
        temp <- temp %>% filter(A3_poste == input$Choix_poste_A6)
      }
      # Filtre sur le genre
      if (input$Choix_genre_A6 == "Tous") {
        temp <- temp
      } else {
        if (input$Choix_genre_A6 == "Homme") {
          temp <- temp%>% filter(Y1A_genre == "Un homme")
        }
        if (input$Choix_genre_A6 == "Femme") {
          temp <- temp%>% filter(Y1A_genre == "Une femme")
        }
      }
      
      temp %>%
        filter(!is.na(A6_experience)) %>%
        count(A6_experience) %>%
        arrange(desc(A6_experience)) %>%
        mutate(A6_experience = as_factor(A6_experience)) %>%
        dplyr::rename("Experience" = "A6_experience")
    }
  )
  
  output$plot_A6 <- renderPlot({
    ggplot(choix_A6()) +
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
    choix_A6() 
  })
################################################################################ 
  
  
  
####################################   B   #####################################
  # ============================================================
  #  B - Outils en data : B1 (connaissance) et B2 (fréquence)
  # ============================================================
  
  # Mettre tous les noms de colonnes en minuscules
  names(data_enquete_B) <- tolower(names(data_enquete_B))
  
  # --- Palettes de couleurs ---
  couleur_B1 <- c("Oui" = "dodgerblue2", "Non" = "firebrick1")
  couleur_B2 <- c(
    "Jamais" = "firebrick1",
    "Occasionnellement" = "orange",
    "Régulièrement" = "darkgreen"
  )
  
  # --- reactiveVal pour le dernier outil sélectionné ---
  outil_reactif <- reactiveVal(NULL)
  
  # --- observeEvent pour chaque bouton ---
  observeEvent(input$python, { outil_reactif("python") })
  observeEvent(input$R, { outil_reactif("r") })
  observeEvent(input$SAS, { outil_reactif("sas") })
  observeEvent(input$PowerBI, { outil_reactif("powerbi") })
  observeEvent(input$Tableau, { outil_reactif("tableau") })
  observeEvent(input$qlik, { outil_reactif("qlik") })
  observeEvent(input$Excel, { outil_reactif("excel") })
  observeEvent(input$SQL, { outil_reactif("sql") })
  observeEvent(input$Spark, { outil_reactif("spark") })
  observeEvent(input$Spss, { outil_reactif("spss") })
  observeEvent(input$java, { outil_reactif("c_java") })
  observeEvent(input$datastudio, { outil_reactif("datastudio") })
  observeEvent(input$Matlab, { outil_reactif("matlab") })
  observeEvent(input$Aucun, { outil_reactif("aucun") })
  
  
  # ============================================================
  #                     QUESTION B1
  # ============================================================
  
  choix_outil_B1 <- eventReactive(outil_reactif(), {
    req(outil_reactif())
    tool <- outil_reactif()
    col_name <- paste0("b1_utilisation_", tolower(tool))
    
    temp <- data_enquete_B
    
    # --- Filtrage selon le poste ---
    if (input$Choix_poste_B != "Tous les postes") {
      temp <- temp %>% filter(a3_poste == input$Choix_poste_B)
    }
    
    # --- Filtrage selon le genre ---
    if (input$Choix_genre_B == "Homme") {
      temp <- temp %>% filter(y1a_genre == "Un homme")
    } else if (input$Choix_genre_B == "Femme") {
      temp <- temp %>% filter(y1a_genre == "Une femme")
    }
    
    # --- Nettoyage et comptage ---
    temp <- temp %>%
      mutate(
        !!col_name := if_else(
          .data[[col_name]] %in% c(NA, "N/A", "NA", "na", ""),
          "Non",
          .data[[col_name]]
        ),
        !!col_name := as.character(.data[[col_name]])
      ) %>%
      count(.data[[col_name]])
    
    names(temp) <- c("Réponse", "Effectif")
    
    list(data = temp, outil = tool)
  })
  
  
  # ============================================================
  #                     QUESTION B2
  # ============================================================
  
  choix_outil_B2 <- eventReactive(outil_reactif(), {
    req(outil_reactif())
    tool <- outil_reactif()
    
    col_B1 <- paste0("b1_utilisation_", tolower(tool))
    col_B2 <- paste0("b2_frequence_utilisation_", tolower(tool))
    
    temp <- data_enquete_B
    
    # --- Filtrage selon le poste ---
    if (input$Choix_poste_B != "Tous les postes") {
      temp <- temp %>% filter(a3_poste == input$Choix_poste_B)
    }
    
    # --- Filtrage selon le genre ---
    if (input$Choix_genre_B == "Homme") {
      temp <- temp %>% filter(y1a_genre == "Un homme")
    } else if (input$Choix_genre_B == "Femme") {
      temp <- temp %>% filter(y1a_genre == "Une femme")
    }
    
    # --- Prendre uniquement ceux qui ont répondu "Oui" à B1 ---
    temp <- temp %>%
      filter(.data[[col_B1]] == "Oui")
    
    # --- Nettoyage et comptage des fréquences ---
    temp <- temp %>%
      mutate(
        !!col_B2 := if_else(
          .data[[col_B2]] %in% c(NA, "N/A", "NA", "na", ""),
          "Jamais",
          .data[[col_B2]]
        ),
        !!col_B2 := as.character(.data[[col_B2]])
      ) %>%
      count(.data[[col_B2]])
    
    names(temp) <- c("Fréquence", "Effectif")
    
    list(data = temp, outil = tool)
  })
  
  
  # ============================================================
  #                     SORTIES
  # ============================================================
  
  # --- B1 : Graphique ---
  output$plot_B1 <- renderPlot({
    req(choix_outil_B1())
    temp <- choix_outil_B1()$data
    tool <- choix_outil_B1()$outil
    
    temp$Réponse <- ifelse(temp$Réponse %in% c("Oui"), "Oui", "Non")
    
    pie(
      temp$Effectif,
      labels = paste0(temp$Réponse, " (", temp$Effectif, ")"),
      border = "white",
      col = couleur_B1[temp$Réponse],
      main = paste("Connaissance de", tool)
    )
  })
  
  # --- B2 : Graphique ---
  output$plot_B2 <- renderPlot({
    req(choix_outil_B2())
    temp <- choix_outil_B2()$data
    tool <- choix_outil_B2()$outil
    
    pie(
      temp$Effectif,
      labels = paste0(temp$Fréquence, " (", temp$Effectif, ")"),
      border = "white",
      col = couleur_B2[temp$Fréquence],
      main = paste("Fréquence d’utilisation de", tool)
    )
  })
  
  output$table_B1 <- renderTable({ req(choix_outil_B1()); choix_outil_B1()$data })
  output$table_B2 <- renderTable({ req(choix_outil_B2()); choix_outil_B2()$data })
  
################################################################################   

  
  
}
################################################################################ 



