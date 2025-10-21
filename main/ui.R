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
    
    tabPanel("B2", "C'est moi qui ai fait ça!!")
  ),
  
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
)
