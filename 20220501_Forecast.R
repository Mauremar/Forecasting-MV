#Version 20220427_1 Marc


#Notes
# um die Daten besser anzeigen zu k?nnen k?nnten wir 
# das due Date ?ndern, frage ist in welches Format
#
#install.packages("readxl")
library(readxl)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("pacman")
library(pacman)

# privat 
setwd("C:/Users/marcm/OneDrive/One_Note/TUHH/Projektseminar/Datenanalyse R")
# Draeger
#setwd("C:/Users/mauremar/Desktop/Privat/Uni/R")
#getwd()
df<-read_excel("Daten_ForecastingChallenge - Q42017.xlsx")
str(df) #shows the structure of the data

# filter, erase "nan"
idx <- which(df[,"Billing"]!="nan")
x1 <- df[idx,]

# eigener filter:
x2<-filter(df,Billing!="nan")
x2

# difference between forecast and billing
x1$diff <- as.numeric(unlist(x1[,5])) - as.numeric(unlist(x1[,6]))
x2$diff <- as.numeric(unlist(x2[,5])) - as.numeric(unlist(x2[,6]))
x2


#plots
  plot(x1$Fc_horizon, x1$diff, col=2)# Alle DIfferenzen von allen Produkten 
  hist(x1$diff, breaks=1000)          # Wie die Differenzen verteilt sin 
  
  
  # One variable, by groups // funktioniert nicht mit diff nur mit Fc_and Order
  # da dann die Sp_number NA anzeigt-->Lösung mit x2
  boxplot(x2$diff ~ Sp_number, 
          data = x2,
          horizontal = T)
  
  #mit ggplot2 "Histogram all Products" // auch möglich mit diff dann x1 statt df 
  df %>%
    ggplot(aes(x = Fc_and_order, 
               fill = Sp_number)) + 
    geom_density(alpha = 0.5) +
    theme(legend.position = "right")
  
  
  #erstellt mehrere Scatterplots, nützlich, um Muster zu erkennen (Erklärung siehe One note)
  # x1 %>%
  #   select(Due_date:diff) %>%
  #   plot()




#p_load(psych)
# describe ist more powerful als summary
# describe(x1$Fc_and_order which(x1[,"Sp_number"]=="Product_19") )
# Funktioniert beides noch nicht

# versuche pearson corralation coefficient herauszufinden:
 str((x1))
 x1$Billing <-as.numeric(unlist(x1[,6]))  # dient dazu den type of billing auf numeric zu setzen
 x1$Sp_number <-as.numeric(unlist(x1[,1]))
 str(x1)
 cor(x1) # hierfür muss x1 komplett numerisch sein
 # bedeutet das ergebnis, dass eine Korrelation zwischen FC and Order und Billing besteht?!
 # müsste noch " Product" durch eine Zahl ersetzen ersetzen, was würde mir das bringen?
   # # auf 2 NK gerundes //klappt noch nicht
   # UCBAdmissions %>%
   # x1 %>%
   #   + cor()%>%
   #   +round(2)
 

 # für eine Detailiertere Correlationsanalyse zwischen 2 Variablen use this:
  cor.test(x1$Fc_and_order,x1$Billing)
  cor.test(x1$Fc_date,x1$diff)
 
 # um alle Korrelatioskoeffizienten in einer und alle  p values in einer
  # zweiten Tabelle anzuzeigen: 
     p_load(Hmisc)
     
    # die Daten müssen dafür in einer Matrix vorliegen und alle gleich sein:
     x1 %>%
       as.matrix() %>%
       rcorr()
 
 
# CLEAN UP #################################################
# 
# # Clear environment
# rm(list = ls()) 
# 
# # Clear packages
# p_unload(all)  # Remove all add-ons
# detach("package:datasets", unload = TRUE)  # For base
# 
# # Clear console
# cat("\014")  # ctrl+L
# 
# # Clear mind :)

#----------------------------------------------------
# Fragments
#Frank:
# idx <- which(df[,"Sp_number"]=="Product_19")
# x1 <- df[idx,]
# idx <- which(x1[,"Billing"]!="nan")
# x2 <- x1[idx,]
# x2$diff <- as.numeric(unlist(x2[,5])) - as.numeric(unlist(x2[,6]))
#
# plot(x2$Fc_horizon, x2$diff, col=2)
# 
# df2 <- df
# df2$diff <- as.numeric(unlist(df[,5])) - as.numeric(unlist(df[,6]))
# plot(df2$Fc_horizon, df2$diff, col=2)

# hist(df2$diff, breaks=1000)

#Productfilter
# #filter f?r Product 19
# filter19<-filter(df,Sp_number=="Product_19")
# str(filter19)
# 
# #plot versuch
# #plot(filter19$Due_date,filter19$Fc_and_order,type = "b",main = "P19 Forecats ?ber Date")
# plot(filter19$Due_date,filter19$Fc_and_order,col = "red3")

#filter f?r das due date 
# filter19Kw<-filter(df,Due_date==201743)
# View(filter19Kw)

#plot(filter19Kw$Fc_date,filter19Kw$Fc_and_order,type = "b",main = "P19 Forecasts ?ber Date")



