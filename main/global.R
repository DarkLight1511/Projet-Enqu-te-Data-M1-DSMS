############################# CHARGER LES PACKAGES #############################
#install.packages("readr")
#install.packages("dplyr")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("forcats")
#install.packages("lubridate")
#install.packages("shinythemes")
#install.packages("plotly")

library(readr) 
library(dplyr)
library(ggplot2)
library(forcats)
library(lubridate)
library(shiny)
library(shinythemes)
library(plotly)
library(stringr)

################################ IMPORTATION ###################################

#data_enquete <- read_csv2("main/data/enquete_data_raw.csv")

data_enquete <- read_csv2("data/enquete_data_raw.csv")

################################  FONCTIONS  ###################################

### Question A4 ###
data_enquete_A4 <- data_enquete %>% 
  select(id, date, A4_secteur, A4_secteur_autre) %>% 
  arrange(id)
View(data_enquete_A4)

### Filtres sur les réponses attentdues ###
data_enquete_filtree <- data_enquete %>% 
  filter(!str_detect(A2_statut, "recherche d'emploi")) %>%  #Exclut les réponses "En recherche d'emploi"
  filter(!str_detect(A2_statut, "Etudiant")) %>%  #Exclut les réponses "Étudiant"
  filter(A3_poste !="Autre")  #Exclut les réponses venant des postes "Autres"
View(data_enquete_filtree)


rename <- function(origin){
  a <- 0
  new <- origin
  condition_rename <- str_detect(origin$A3_poste, "Chargé d’études")
  for (i in condition_rename){
    a <- a+1
    if (is.na(i)){
    }else{
      if(i){
        #print(a)
        new$A3_poste[a] <- "Data Analyst_new"
      }
    }
  }
  return(new)
}
data_enquete_new <- rename(data_enquete_filtree)
View(data_enquete_new)


################################################################################


























