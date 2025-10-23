fluidPage(
  theme = shinytheme("cerulean"),
  
  # Insertion d'image
  img(src = "Résultats-denquête.png",
      height = "100x"),
  
  # Application title
  titlePanel("Résultats d'enquête"),
  
  # Page
  navbarPage(
    "Questions",
    
###################################### A4 ######################################   

    tabPanel("A4 - Secteurs d'activité",
             
             sidebarLayout(
               sidebarPanel(
                 # Titre de la question A4
                 h3("Dans quel secteur travaillez-vous ?"), 
                 # Trois boutons pour la question A4
                 actionButton("secteur1", "Secteurs principaux"),
                 actionButton("secteur2", "Autres secteurs"), 
                 actionButton("secteur3", "Tous les secteurs"), 
                 hr(),
                 selectInput("Choix_poste_A4", "Choix du poste occupé: ",
                             choices = c("Tous les postes", 
                                         unique(data_enquete_base$A3_poste))),
                 radioButtons("Choix_genre_A4", " Choix du genre", 
                              choices = c("Tous","Homme","Femme"),
                              selected = "Tous", inline = TRUE),
               ),
               
               mainPanel(tabsetPanel(
                 tabPanel("Graphique", plotOutput("plot_A4")),
                 tabPanel("Camembert", plotOutput("plot_pie_A4")),
                 tabPanel("Tableau", tableOutput("table_A4"))
               ))
             )),
################################################################################ 
    
    tabPanel("A5", "C'est moi qui ai fait ça!!"),
    
###################################### A6 ######################################

    tabPanel("A6 - Années d'expérience",
             
             sidebarLayout(
               sidebarPanel(
                 # Titre de la question A6
                 h3("Combien d’années d’expérience avez-vous en data ?"),
                 selectInput("Choix_poste_A6", "Choix du poste occupé: ",
                             choices = c("Tous les postes", 
                                         unique(data_enquete_base$A3_poste))),
                 radioButtons("Choix_genre_A6", " Choix du genre", 
                              choices = c("Tous","Homme","Femme"),
                              selected = "Tous", inline = TRUE),
               ),
               
               mainPanel(tabsetPanel(
                 tabPanel("Graphique", plotOutput("plot_A6")),
                 tabPanel("Tableau", tableOutput("table_A6"))
               ))
             )),
################################################################################,
    


###################################### B #######################################

    tabPanel(
  "B - Outils en data",

  fluidRow(
    # Panel de gauche
    column(
      width = 4,
      style = "min-width: 400px; max-width: 450px;",  # simulateur de "3.5"
      wellPanel(
        h3("B1 - Connaissez-vous ces différents outils ?"),

        # CSS pour espacer et aligner les boutons
        tags$head(
          tags$style(HTML("
            .outil-btn {
              display: inline-block;
              margin: 5px;
              width: 110px;
              height: 60px;
              text-align: center;
              vertical-align: middle;
            }
            .outil-btn img {
              display: block;
              margin: auto;
            }
          "))
        ),

        # Grille de boutons
        div(
          class = "outil-container",
          actionButton("python", tagList(img(src="Python-Logo.png", height="25px"), "Python"), class="outil-btn"),
          actionButton("R", tagList(img(src="R.png", height="25px"), "R"), class="outil-btn"),
          actionButton("SAS", tagList(img(src="SAS.png", height="25px"), "SAS"), class="outil-btn"),
          actionButton("PowerBI", tagList(img(src="PowerBI.png", height="25px"), "PowerBI"), class="outil-btn"),
          actionButton("Tableau", tagList(img(src="Tableau.png", height="25px"), "Tableau"), class="outil-btn"),
          actionButton("qlik", tagList(img(src="qlik.png", height="25px"), "Qlik"), class="outil-btn"),
          actionButton("Excel", tagList(img(src="Excel.png", height="25px"), "Excel"), class="outil-btn"),
          actionButton("SQL", tagList(img(src="SQL.png", height="25px"), "SQL"), class="outil-btn"),
          actionButton("Spark", tagList(img(src="Spark.png", height="25px"), "Spark"), class="outil-btn"),
          actionButton("Spss", tagList(img(src="Spss.png", height="25px"), "Spss"), class="outil-btn"),
          actionButton("java", tagList(img(src="java.png", height="25px"), "Java"), class="outil-btn"),
          actionButton("datastudio", tagList(img(src="datastudio.png", height="25px"), "Datastudio"), class="outil-btn"),
          actionButton("Matlab", tagList(img(src="Matlab.png", height="25px"), "Matlab"), class="outil-btn"),
          actionButton("Aucun", tagList(img(src="Aucun.png", height="25px"), "Aucun"), class="outil-btn")
        )
      )
    ),

    # Panel du milieu
    column(
      width = 5,
      wellPanel(
        tabsetPanel(
          tabPanel("Graphique", plotOutput("plot_B2")),
          tabPanel("Tableau", tableOutput("table_B2"))
        )
      )
    ),

    # Panel de droite
    column(
      width = 3,
      wellPanel(
        h3("B2 - À quelle fréquence utilisez-vous ces outils ?"),
        selectInput("Choix_poste_B", "Choix du poste occupé: ",
                    choices = c("Tous les postes", 
                                unique(data_enquete_base$A3_poste))),
        radioButtons("Choix_genre_B", " Choix du genre", 
                     choices = c("Tous","Homme","Femme"),
                     selected = "Tous", inline = TRUE),
        ))
  )
),

################################################################################,
  
############## Bouton pour afficher/masquer la sélection de thème ############## 
  absolutePanel(
    bottom = 10, left = 10,  
    width = 250,             
    draggable = FALSE,       
    
    actionButton("toggle_theme", "🎨 Changer le thème"),
    
    br(),
    
    conditionalPanel(
      condition = "input.toggle_theme % 2 == 1",
      themeSelector()
    )
  )
################################################################################ 
))
