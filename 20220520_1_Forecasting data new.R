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
  #view(df)
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
  df$P19Bill<- as.numeric(df$P19Bill)
  df$P20Bill<- as.numeric(df$P20Bill)
  df$P22Bill<- as.numeric(df$P22Bill)
  df$P30Bill<- as.numeric(df$P30Bill)
  df$P39Bill<- as.numeric(df$P39Bill)
  df$P48Bill<- as.numeric(df$P48Bill)
  df$P50Bill<- as.numeric(df$P50Bill)
  df$P55Bill<- as.numeric(df$P55Bill)
  df$P57Bill<- as.numeric(df$P57Bill)
  df$P67Bill<- as.numeric(df$P67Bill)
  #df$...1<- as.Date(df$...1,"%Y/%U")
  str(df)
 
    #-------Einschub Daten auffüllen
    repl <- apply(df,2, function(x) mean(x, na.rm=TRUE))
    # wende eine funktion auf die Spalten(2) an die funktion ist der mittelwert der
    #Spalten in denen ein fehlender Wert ist
    for(i in 1:dim(df)[2]) df[is.na(df[,i]),i] <- repl[i]
    #die werte in der Spalte mit index i werden ersetzt, wenn in dem i kein Wert steht
    x1 <- df
    #x1<- x1[66:117,]
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
  
  xd <- rcorr(as.matrix(x1))
  
  
  # hier wäre es interessant genau die Produkte welche einen P wert < 0.05 haben
  # in den Forecast miteinzubeziehen 
  # Korrelationen zwischen P19, P20, P22, P30, P39, 55
  # p wert größer 0,05 bei P19 zu 48, 50, (57),(67)
  # @ vanessa vlt. könntest du hier herausfinden welche Faktoren (statistisch signif)
  # miteinander korrelieren, damit das Var modell nicht zu groß wird 
  # korrelationp <- cor.mtest
  # link: https://www.youtube.com/watch?v=RgZhBDqEIq8
  ?ts
  
  
# ------------------Forecasting-------------
  P19Bill<- ts(x1$P19Bill,start = c(201600),end = c(201652),frequency = 1)
  P20Bill<- ts(x1$P20Bill,start = c(201600),end = c(201652),frequency = 1)
  P22Bill<- ts(x1$P22Bill,start = c(201600),end = c(201652),frequency = 1)
  P30Bill<- ts(x1$P30Bill,start = c(201600),end = c(201652),frequency = 1)
  P39Bill<- ts(x1$P39Bill,start = c(201600),end = c(201652),frequency = 1)
  P48Bill<- ts(x1$P48Bill,start = c(201600),end = c(201652),frequency = 1)
  P50Bill<- ts(x1$P50Bill,start = c(201600),end = c(201652),frequency = 1)
  P55Bill<- ts(x1$P55Bill,start = c(201600),end = c(201652),frequency = 1)
  P57Bill<- ts(x1$P57Bill,start = c(201600),end = c(201652),frequency = 1)
  P67Bill<- ts(x1$P67Bill,start = c(201600),end = c(201652),frequency = 1)
  P19Fc1<-ts(x1$P19H1,start = c(201600),end = c(201652),frequency = 1)
  P19Fc2<-ts(x1$P19H2,start = c(201600),end = c(201652),frequency = 1)
  P19Fc3<-ts(x1$P19H3,start = c(201600),end = c(201652),frequency = 1)
  P19Fc4<-ts(x1$P19H4,start = c(201600),end = c(201652),frequency = 1)
  P19Fc5<-ts(x1$P19H5,start = c(201600),end = c(201652),frequency = 1)
  P19Fc6<-ts(x1$P19H6,start = c(201600),end = c(201652),frequency = 1)
  P19Fc7<-ts(x1$P19H7,start = c(201600),end = c(201652),frequency = 1)
  P19Fc8<-ts(x1$P19H8,start = c(201600),end = c(201652),frequency = 1)
  P19Fc9<-ts(x1$P19H9,start = c(201600),end = c(201652),frequency = 1)
  P19Fc10<-ts(x1$P19H10,start = c(201600),end = c(201652),frequency = 1)
  P19Fc11<-ts(x1$P19H11,start = c(201600),end = c(201652),frequency = 1)
  P19Fc12<-ts(x1$P19H12,start = c(201600),end = c(201652),frequency = 1)
  P19Fc13<-ts(x1$P19H13,start = c(201600),end = c(201652),frequency = 1)
  
  
  # plot the series
  #autoplot(cbind(P19Bill,P19Fc1,P19Fc2,P19Fc3,P19Fc4,P19Fc5,P19Fc6,P19Fc7,P19Fc8,P19Fc9,P19Fc10,P19Fc11,P19Fc12,P19Fc13))
  autoplot(cbind(P19Bill,P20Bill,P22Bill,P30Bill,P39Bill,P48Bill,P50Bill,P55Bill,P57Bill,P67Bill))
  # 
  # #OLS
  #OLS1 <- lm(P19Bill~P19Fc1)
       # Fc ist independent, Diff ist dependent
       # das heisst wir gehen davon aus, dass Diff von Fc abhängig ist,
       # bei der VAR methode wissen wir es allerdings nicht und lassen die
       #Daten für sich sprechen
       #summary(OLS1)
  # 
    #Determine the persisitence of the ode,
       #by determining the acf und p(partial)acf
       # acf beschreibt, ob es einen signifikanten zusammenhang zwischen
       #beobacteten Messeregbnissen zu unterschiedlichen Beobachtungszeitpkt
       #gibt. richtung 1 bedeutet signifikanter zusammenhang
       # Kreuzkorrelation könnte auch interessant sein
      #acf(P19Bill, main="ACF for P19 Billing")
      #pacf(P19Bill, main="PACF for P19 Billing")
      
      #acf(P19Fc1, main="ACF for Forecast and Order der ersten Woche")
      #pacf(P19Fc1, main="PACF for Forecast and Order der ersten Woche")

      # finding the optimal Lags
          # var braucht die anzahl der Autoregressive lags vorgegeben
          okun.bv <- cbind(P19Bill,P19Fc1,P19Fc2,P19Fc3,P19Fc4,P19Fc5,P19Fc6,P19Fc7,P19Fc8,P19Fc9,P19Fc10,P19Fc11,P19Fc12,P19Fc13)
          #okun.bv<- cbind(P19Bill,P20Bill,P22Bill,P30Bill,P39Bill,P48Bill,P50Bill,P55Bill,P57Bill,P67Bill)
          # bindet die beiden Variablen aneinander
          #colnames(okun.bv) <- cbind("P19Billing","P19Fc1")
          # aendert den NAmen der Spalten

          lagselect <- VARselect(okun.bv,type = "const")
          # bei max 10 kommen 9 lags, bei max 100 kommen 54 und bei max 200 4lags
          # mit "const" wird angenommen, dass es keinen (saisonalen)
          # Trend in den Daten gibt
          lagselect$selection
          # zeigt und die Daten der selection criteria an, diesen wert sollten wir
          #im Var model als p nehmen

          #building Var Model
              ModelOkun1 <- VAR(okun.bv,p = 1, type = "const", season = NULL, exog = NULL)
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
            forecast$fcst
            fanchart(forecast, names ="P19Billing")
            fanchart(forecast, names ="P19 Fc1")
          # wie viele quaters ahead forecast werden, confidence intervall von 95% und das in
          # einem fanchart wiedergeben

    #Multpile Regression
    #fit <- lm(P19Bill ~  P19H1 + P19H2 + P19H3 + P19H4 + P19H5 + P19H6 + P19H7 + P19H8 + P19H9 + P19H10 + P19H11 + P19H12 + P19H13, data=df)
    fit <- Arima(P19Bill, order = c(3,1,0))
            summary (fit)
    
    plot(df$Due_date,df$P19Bill)
    
    coefficients(fit) # model coefficients
    confint(fit, level=0.95) # CIs for model parameters 
    fitted(fit) # predicted values
    residuals(fit) # residuals
    anova(fit) # anova table 
    #install.packages("DAAG")
    library(DAAG)
    cv.lm(df, fit, m=3)
  
  #---- Forecasting Ende------------------------
  
  # Nächste Schritte: 
  # Daten Darstellen, vlt sollten wir hier differenzieren zwischen den wirklichen 
  # Rohdaten und den Structurierten quasi df1 anlegen mit reinen Rohdaten. Oder
  # es noch irgendwie in R programmieren und dann einfach damit weiter machen 

            