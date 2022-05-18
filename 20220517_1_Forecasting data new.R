# neue Version mit den neu aufbereiteter Matrix (VBA)
# Date: 18.05.2022
# Author: Marc Maurer & Vanessa Kleemann
# note: heute den ersten Forecast mit P19 und H1 geschafft

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
  install.packages("corplot")
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
  library(corplot)

# Einlesen der Rohdaten 
  # privat
  setwd("C:/Users/marcm/OneDrive/One_Note/TUHH/Projektseminar/Datenanalyse R")
  # Draeger
  #setwd("C:/Users/mauremar/Desktop/Privat/Uni/R")
  getwd()
  df<-read_excel(file.choose())
 
  
# Struktur und Darstellung der ROhdaten 
  # hier koennte mit dem Urspruenglichen Datensatz gearbeitet werden 
  
  str(df) #shows the structure of the data
  view(df)
  md.pattern(df)
  summary(df)
  sum(is.na(df)) # gibt die Anzahl der Missing Data
  
# hier koennte der Ursprungsdatensatz mit den Entsprechenden Analysen stehen 
  # waere auch in einem zweiten COde moeglich
  
  
# Missing Data ersetzen
  # man kann die Daten rauswerfen (omit) macht wenig sinn 
  # man kann den mean über die gesamte spalte nehmen
  #--> wert würde sich wiederholen, und Entwicklungen rausnehmen 
  
  # hierfür muss die Spalte P20H13 auch num sein 
  #@ Vanessa aus welchen Gründen auch immer, nur diese spalte hatte character
  #Eigenschaften
  df$P20H13<- as.numeric(df$P20H13)
  str(df)
  
    #-------Einschub Daten auffüllen
    repl <- apply(df,2, function(x) mean(x, na.rm=TRUE))
    # wende eine funktion auf die Spalten(2) an die funktion ist der mittelwert der
    #Spalten in denen ein fehlender Wert ist
    for(i in 1:dim(df)[2]) df[is.na(df[,i]),i] <- repl[i]
    #die werte in der Spalte mit index i werden ersetzt, wenn in dem i kein Wert steht
    x1 <- df
    view(x1)
    #----------Einschub Ende
  
  ggplot(data=x1)+ geom_point(mapping = aes(x=Due_date,y=P19Bill))
  
#------------ neu aufbereitete Matrix analysieren -------------

  # um alle Korrelatioskoeffizienten in einer und alle  p values in einer
  # zweiten Tabelle anzuzeigen:
  p_load(Hmisc)

  # die Daten müssen dafür in einer Matrix vorliegen und alle gleich sein:
  x1 %>%
    as.matrix() %>%
    rcorr()
  
  # um alle Korrelatioskoeffizienten in einer und alle  p values in einer
  # zweiten Tabelle anzuzeigen:
  p_load(Hmisc)

  # die Daten müssen dafür in einer Matrix vorliegen und alle gleich sein:
  x1 %>%
    as.matrix() %>%
    rcorr()
  # hier wäre es interessant genau die Produkte welche einen P wert < 0.05 haben
  # in den Forecast miteinzubeziehen 
  # Korrelationen zwischen P19, P20, P22, P30, P39, 55
  # p wert größer 0,05 bei P19 zu 48, 50, (57),(67)
  # @ vanessa vlt. könntest du hier herausfinden welche Faktoren (statistisch signif)
  # miteinander korrelieren, damit das Var modell nicht zu groß wird 
  # korrelationp <- cor.mtest
  # link: https://www.youtube.com/watch?v=RgZhBDqEIq8
  
  
# ------------------Forecasting-------------
  P19Bill<- ts(x1$P19Bill,start = c(201500),end = c(201600),frequency = 12)
  P19Fc1<-ts(x1$P19H1,start = c(201500),end = c(201600),frequency = 12)
  P19Fc2<-ts(x1$P19H2,start = c(201500),end = c(201600),frequency = 12)
  P19Fc3<-ts(x1$P19H3,start = c(201500),end = c(201600),frequency = 12)
  P19Fc4<-ts(x1$P19H4,start = c(201500),end = c(201600),frequency = 12)
  P19Fc5<-ts(x1$P19H5,start = c(201500),end = c(201600),frequency = 12)
  P19Fc6<-ts(x1$P19H6,start = c(201500),end = c(201600),frequency = 12)
  P19Fc7<-ts(x1$P19H7,start = c(201500),end = c(201600),frequency = 12)
  P19Fc8<-ts(x1$P19H8,start = c(201500),end = c(201600),frequency = 12)
  P19Fc9<-ts(x1$P19H9,start = c(201500),end = c(201600),frequency = 12)
  P19Fc10<-ts(x1$P19H10,start = c(201500),end = c(201600),frequency = 12)
  P19Fc11<-ts(x1$P19H11,start = c(201500),end = c(201600),frequency = 12)
  P19Fc12<-ts(x1$P19H12,start = c(201500),end = c(201600),frequency = 12)
  P19Fc13<-ts(x1$P19H13,start = c(201500),end = c(201600),frequency = 12)
  
  # plot the series
  autoplot(cbind(P19Bill,P19Fc1,P19Fc2,P19Fc3,P19Fc4,P19Fc5,P19Fc6,P19Fc7,P19Fc8,P19Fc9,P19Fc10,P19Fc11,P19Fc12,P19Fc13))
  # 
  # #OLS
  OLS1 <- lm(P19Bill~P19Fc1)
       # Fc ist independent, Diff ist dependent
       # das heisst wir gehen davon aus, dass Diff von Fc abhängig ist,
       # bei der VAR methode wissen wir es allerdings nicht und lassen die
       #Daten für sich sprechen
       summary(OLS1)
  # 
    #Determine the persisitence of the ode,
       #by determining the acf und p(partial)acf
       # acf beschreibt, ob es einen signifikanten zusammenhang zwischen
       #beobacteten Messeregbnissen zu unterschiedlichen Beobachtungszeitpkt
       #gibt. richtung 1 bedeutet signifikanter zusammenhang
       # Kreuzkorrelation könnte auch interessant sein
      acf(P19Bill, main="ACF for P19 Billing")
      pacf(P19Bill, main="PACF for P19 Billing")
      
      acf(P19Fc1, main="ACF for Forecast and Order der ersten Woche")
      pacf(P19Fc1, main="PACF for Forecast and Order der ersten Woche")

      # finding the optimal Lags
          # var braucht die anzahl der Autoregressive lags vorgegeben
          okun.bv <- cbind(P19Bill,P19Fc1,P19Fc2,P19Fc3,P19Fc4,P19Fc5,P19Fc6,P19Fc7,P19Fc8,P19Fc9,P19Fc10,P19Fc11,P19Fc12,P19Fc13)
          # bindet die beiden Variablen aneinander
          colnames(okun.bv) <- cbind("P19Billing","P19Fc1")
          # aendert den NAmen der Spalten

          lagselect <- VARselect(okun.bv,lag.max = 100,type = "const")
          # bei max 10 kommen 9 lags, bei max 100 kommen 54 und bei max 200 4lags
          # mit "const" wird angenommen, dass es keinen (saisonalen)
          # Trend in den Daten gibt
          lagselect$selection
          # zeigt und die Daten der selection criteria an, diesen wert sollten wir
          #im Var model als p nehmen

          #building Var Model
              ModelOkun1 <- VAR(okun.bv,p = 4, type = "const", season = NULL, exog = NULL)
              # bedeutet man geht von keinen Saisonalen effekten und keinen Exoten aus
              summary(ModelOkun1)
              # Auswertun:
                # roots of the char... beschreibt ob unser system stabil ist (wenn alle
                # Werte innerhalb des Unit circles sind)= keine strenuous? roots
                # im p wert der Tabelle kann abglesen werden, ob es ein significantes
                # lag gibt

           # hier würden jetzt einige tests kommen
            #forecast mit Var
            forecast <- predict(ModelOkun1,n.ahead = 13, ci = 0.95)
            forecast$fcst$P19Bill
            fanchart(forecast, names ="P19Billing")
            fanchart(forecast, names ="P19 Fc1")
          # wie viele quaters ahead forecast werden, confidence intervall von 95% und das in
          # einem fanchart wiedergeben

  
  
  #---- Forecasting Ende------------------------
  
  # Nächste Schritte: 
  # Daten Darstellen, vlt sollten wir hier differenzieren zwischen den wirklichen 
  # Rohdaten und den Structurierten quasi df1 anlegen mit reinen Rohdaten. Oder
  # es noch irgendwie in R programmieren und dann einfach damit weiter machen 

            