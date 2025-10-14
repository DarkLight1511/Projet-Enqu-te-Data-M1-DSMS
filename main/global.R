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
library(RColorBrewer)

################################ IMPORTATION ###################################

#data_enquete <- read_csv2("main/data/enquete_data_raw.csv")

data_enquete <- read_csv2("data/enquete_data_raw.csv")

################################  FONCTIONS  ###################################

### Filtres sur les réponses attendues ###
filtre <- function(origin){
  new <- origin %>% 
    filter(!str_detect(A2_statut, "recherche d'emploi")) %>%  #Exclut les réponses "En recherche d'emploi"
    filter(!str_detect(A2_statut, "Etudiant")) %>%  #Exclut les réponses "Étudiant"
    filter(A3_poste !="Autre")  #Exclut les réponses venant des postes "Autres"
}

### Rename sur les postes ###
rename <- function(origin){
  # Change les "Chargé d’études" en "Data Analyst" 
  a <- 0
  new <- origin
  condition_rename <- str_detect(origin$A3_poste, "Chargé d’études")
  for (i in condition_rename){
    a <- a+1
    if (is.na(i)){
    }else{
      if(i){
        new$A3_poste[a] <- "Data Analyst"
      }
    }
  }
  # Change les "Statisticien" en "Data Scientist" 
  a <- 0
  condition_rename <- str_detect(origin$A3_poste, "Statisticien")
  for (i in condition_rename){
    a <- a+1
    if (is.na(i)){
    }else{
      if(i){
        new$A3_poste[a] <- "Data Scientist"
      }
    }
  }
  
  return(new)
}

#data_enquete_renamed <- rename(data_enquete)
#data_enquete_filtered <- filtre(data_enquete)
data_enquete_base <- rename(filtre(data_enquete))
#View(data_enquete_base)


### Question A4 ###
A4 <- function(origin){
  new <- origin %>% 
    select(id, date, A4_secteur, A4_secteur_autre) %>% 
    arrange(id)
}
data_enquete_A4 <- A4(data_enquete_base)
#View(data_enquete_A4)

temp <- data_enquete_A4 %>%
  filter(!is.na(A4_secteur_autre)) %>%
  count(A4_secteur_autre) %>%
  arrange(desc(n)) %>% 
  mutate(A4_secteur_autre = as_factor(A4_secteur_autre)) %>% 
  dplyr::rename("Secteur"= "A4_secteur_autre")
View(temp)

################################################################################


























