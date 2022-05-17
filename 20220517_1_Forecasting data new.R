# neue Version mit den neu aufbereiteter Matrix (VBA)
# Date: 17.05.2022
# Author: Marc Maurer & Vanessa Kleemann

# installation and loading of required packages:

  #install
  #install.packages("readxl")
  #install.packages("tidyverse")
  #install.packages("pacman")
  #install.packages("urca")
  #install.packages("vars")
  #install.packages("mFilter")
  #install.packages("tseries")
  #install.packages("forecast")
  #install.packages("panelvar")
  install.packages ("mice") # zur Darstellung, welche Daten fehlen 

  #load the packages
  library(readxl)
  library(tidyverse)
  library(pacman)
  library(urca)
  library(vars)
  library(mFilter)
  library(tseries)
  library(forecast)
  library(panelvar)
  library(mice)

# Einlesen der Rohdaten 
  # privat
  setwd("C:/Users/marcm/OneDrive/One_Note/TUHH/Projektseminar/Datenanalyse R")
  # Draeger
  #setwd("C:/Users/mauremar/Desktop/Privat/Uni/R")
  getwd()
  df<-read_excel(file.choose())
 
  
# Struktur und Darstellung der ROhdaten 
  
  str(df) #shows the structure of the data
  view(df)
  md.pattern(df)
  summary(df)
  sum(is.na(df)) # gibt die Anzahl der Missing Data
  
  
# Missing Data ersetzen
  # man kann die Daten rauswerfen (omit) macht wenig sinn 
  # man kann den mean über die gesamte spalte nehmen
  #--> wert würde sich wiederholen, und Entwicklungen rausnehmen 
  
  #?mice
  x1 <- df
  
  ggplot(data=x1)+ geom_point(mapping = aes(x=Due_date,y=P19Bill))

  # um alle Korrelatioskoeffizienten in einer und alle  p values in einer
  # zweiten Tabelle anzuzeigen:
  p_load(Hmisc)

  # die Daten müssen dafür in einer Matrix vorliegen und alle gleich sein:
  x1 %>%
    as.matrix() %>%
    rcorr()
  
  # Nächste Schritte: Daten auffüllen mit mice fkt. 
  # Daten Darstellen, vlt sollten wir hier differenzieren zwischen den wirklichen 
  # Rohdaten und den Structurierten
  # (vlt. kann Frank helfen, die Matrix in R zu erstellen und mit Missing data)
  
