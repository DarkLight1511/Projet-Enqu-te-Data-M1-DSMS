fluidPage(
  theme = shinytheme("cerulean"),
  themeSelector(),
  
  # Insertion d'image
  img(src = "Résultats-denquête.png",
      height = "100x"),
  
  # Application title
  titlePanel("Résultats d'enquête"),
  
  # Page n°1
  navbarPage(
    "Questions",
    
    tabPanel("A4 - Secteurs d'activité",
             
             sidebarLayout(
               sidebarPanel(
                 actionButton("secteur1", "Secteurs principaux"),
                 actionButton("secteur2", "Autres secteurs"), 
                 actionButton("secteur3", "Tous les secteurs"), 
                 hr(),
               ),
               
               mainPanel(tabsetPanel(
                 tabPanel("Graphique", plotOutput("plot_A4")),
                 tabPanel("Camembert", plotOutput("plot_pie_A4")),
                 tabPanel("Tableau", tableOutput("table_A4"))
               ))
             )),
    
    tabPanel("A5", "C'est moi qui ai fait ça!!"),
    
    tabPanel("A6", "C'est moi qui ai fait ça!!"),
    
    tabPanel("B2", "C'est moi qui ai fait ça!!")
  )
)
