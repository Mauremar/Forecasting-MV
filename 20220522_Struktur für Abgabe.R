# zuerst den Umgang mit den Orginalen Rohdaten zeigen, um ein gefühl dafür zu 
# bekommen. 
# (Tests aus der .. Datei)
# 
# Dann evtl in zweitem R code mit der Matrix von Vanessa (VBA) weiter arbeiten 
# 

# OLS= ordinary least square regression
# 
# hier kommt dann das Model 
# 
# colinearität überprüfen 
# 
# Hier kommen Tests, ob unser Model valide ist (Gauss-Markov assumptions)
# 1. stability test 
# 2. heteroscedasticity test 
# 3. autokorrelationstest 
# 4. godness fit f test
# 5. cross validity test 
# --! Aufteilung in Test und Trainingsdatensatz 
# 6. Accuracy vergleichen 



# neue Version mit den neu aufbereiteter Matrix (VBA)
# Date: 24.05.2022
# Author: Marc Maurer & Vanessa Kleemann
# note: heute den ersten Forecast mit P19 und H1 geschafft


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
#install.packages ("mice") # zur Darstellung, welche Daten fehlen 
#install.packages("corplot")
#install.packages("gridExtra")
#install.packages("openxlsx")
#load the packages
# library(readxl)
# library(tidyverse)
# library(pacman)
# library(urca)
# library(vars)
# library(mFilter)
# library(tseries)
# library(forecast)
# #library(panelvar)
# library(mice)
# library(corplot)
# library(gridExtra)
# library(openxlsx)

#nur diese Packages sind in dem Code noetig, sonst kein stability test moegl.
     library(urca)
     library(vars)
     library(mFilter)
     library(tseries)
     library(forecast)
     library(tidyverse)


# Einlesen der Rohdaten 
# privat
setwd("C:/Users/marcm/OneDrive/One_Note/TUHH/Projektseminar/Datenanalyse R")
# Draeger
#setwd("C:/Users/mauremar/Desktop/Privat/Uni/R")
getwd()
df<-read_excel(file.choose())   #"Datenstruckturiert_backup .xlsm"
options(digits = 5)

# Struktur und Darstellung der Rohdaten 
  str(df) #shows the structure of the data
  view(df)
  summary(df)
  
  # Umgang mit Missing Data
  md.pattern(df)
  sum(is.na(df)) # gibt die Anzahl der Missing Data
  # Missing Data ersetzen
  # hierfür muuessen alle Spalten numerisch sein 
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
  colnames(df)[colnames(df)=="P63Bill"] <-"P67Bill"
  view(df)
  df$P67Bill<- as.numeric(df$P67Bill)
  #df$...1<- as.Date(df$...1,"%Y/%U")
  #options(max.print = 1000)
  str(df,list.len=ncol(df))  # befehl um die ganze Liste anzuzeigen
  
  
  #-------Einschub Daten auffuellen----
  repl <- apply(df,2, function(x) mean(x, na.rm=TRUE))
  # wende eine funktion auf die Spalten(2) an die Funktion ist der Mittelwert der
  #Spalten in denen ein fehlender Wert ist
  for(i in 1:dim(df)[2]) df[is.na(df[,i]),i] <- repl[i]
  #die werte in der Spalte mit index i werden ersetzt, wenn in dem i kein Wert steht
    #for(i in 1:dim(df)[2]) df[is.na(df[,i]),i] <- apply(df,2, function(x) mean(x[,i:i+10], na.rm=TRUE))
    #--! hier muss noch eine bessere Lsg gefunden werden, als der gesamte Mittelwert!
    #--! Idee mit Package "Dynlm"?
  
  
  view(df)
  x1<- df[14:222,]
  view(x1)
  #----------Einschub Ende--------

ggplot(data=x1)+ geom_point(mapping = aes(x=Due_date,y=P19Bill))

#------------ neu aufbereitete Matrix analysieren -------------

# um alle Korrelatioskoeffizienten in einer und alle  p values in einer
# zweiten Tabelle anzuzeigen:
p_load(Hmisc)

# die Daten müssen dafür in einer Matrix vorliegen und alle gleich sein:
  options(max.print = 1000)
  x1 %>%
  as.matrix() %>%
  rcorr()

xd <- rcorr(as.matrix(x1))
view(xd)
  #Hier kann man bereits feststellen ´, dass die Korrelation mit 
  # zunehmendem Forecast Richtung due_date zunimmt 


  #Abhängigkeiten feststellen 
    # kleinere Matrizen für bessere Uebersicht
    xdr <- subset(xd$r)
    xdp <- subset(xd$P)

      # Matrix nur mit Korrelationen für Billings
      xdrP<-subset(xdr,select = c(P19Bill,P20Bill,P22Bill,P30Bill,P39Bill,P48Bill,P50Bill,P55Bill,P57Bill,P67Bill))
      view(xdrP)
      # Matrix nur mit P-Werten für Billings
      xdpP<-subset(xdp,select = c(P19Bill,P20Bill,P22Bill,P30Bill,P39Bill,P48Bill,P50Bill,P55Bill,P57Bill,P67Bill))
      view(xdpP)
      
      # Matrix nur mit Korrelationen für die 5 aktuellsten Forecasts
      xdrFc<-subset(xdr,select = c(P19H1,P19H2,P19H3,P19H4,P19H5,P20H1,P20H2,P20H3,P20H4,P20H5,P22H1,P22H2,P22H3,P22H4,P22H5,P30H1,P30H2,P30H3,P30H4,P30H5,P39H1,P39H2,P39H3,P39H4,P39H5,P48H1,P48H2,P48H3,P48H4,P48H5,P50H1,P50H2,P50H3,P50H4,P50H5,P55H1,P55H2,P55H3,P55H4,P55H5,P57H1,P57H2,P57H3,P57H4,P57H5,P67H1,P67H2,P67H3,P67H4,P67H5))
      view(xdrFc)
      # ersten 5 Fc scheint ok zu sein 
      # Trend zu erkennen, dass mit zunehmenden Alter des Fc die werte schlechter mit Billing korrelieren 
                                                 
      #noetige Umwandlung für Ausgabe in Excel    
      xdrP_all<-as.data.frame.matrix(xdrP)
      view(xdrP_all)
      write.xlsx(xdrP_all,"Korrelation1 zw. Produkten.xlsx",overwrite = TRUE)
      # dadurch wurde festgestellt, welche beiden anderen Produkte jeweils mit
      # einbezogen werden sollten 
    
      xdpP_all<-as.data.frame.matrix(xdpP)
      round(xdpP_all,digits = 5)
      view(xdpP_all)
      write.xlsx(xdpP_all,"Signifikanz zw. Produkten.xlsx",overwrite = TRUE)
      
      # --!hier noch recherchieren, was eine gute Anzahl an FC Werten wäre und welcher 
      # Korrelationskoeffizient als "hoch gilt"
      
      # weitere mMoeglichkeiten zum testen, wenn Zeit  
        #korrelationp <- cor.mtest
        # link: https://www.youtube.com/watch?v=RgZhBDqEIq8
      
# Nachweis, dass alle markierten Zusammenhänge signifikant sind
# in den Pdfs wurden die Produktabhängigkeiten ueberprüft und in einer Exel
# "Abhängigkeiten der Billings voneinander" zusammengefasst   

#----------------Ende Matrizenanalyse----------------            


# ------------------Forecasting-------------
  #Quellen: 
      #
      # noch einfuegen
      
  #erstellen der ts Elemente  
    P19Bill<- ts(x1$P19Bill,frequency = 1)
    P20Bill<- ts(x1$P20Bill,frequency = 1)
    P22Bill<- ts(x1$P22Bill,frequency = 1)
    P30Bill<- ts(x1$P30Bill,frequency = 1)
    P39Bill<- ts(x1$P39Bill,frequency = 1)
    P48Bill<- ts(x1$P48Bill,frequency = 1)
    P50Bill<- ts(x1$P50Bill,frequency = 1)
    P55Bill<- ts(x1$P55Bill,frequency = 1)
    P57Bill<- ts(x1$P57Bill,frequency = 1)
    P67Bill<- ts(x1$P67Bill,frequency = 1)
    
    P19Fc1<-ts(x1$P19H1,frequency = 1)
    P19Fc2<-ts(x1$P19H2,frequency = 1)
    P19Fc3<-ts(x1$P19H3,frequency = 1)
    P19Fc4<-ts(x1$P19H4,frequency = 1)
    P19Fc5<-ts(x1$P19H5,frequency = 1)

    P20Fc1<-ts(x1$P20H1,frequency = 1)
    P20Fc2<-ts(x1$P20H2,frequency = 1)
    P20Fc3<-ts(x1$P20H3,frequency = 1)
    P20Fc4<-ts(x1$P20H4,frequency = 1)
    P20Fc5<-ts(x1$P20H5,frequency = 1)

    P22Fc1<-ts(x1$P22H1,frequency = 1)
    P22Fc2<-ts(x1$P22H2,frequency = 1)
    P22Fc3<-ts(x1$P22H3,frequency = 1)
    P22Fc4<-ts(x1$P22H4,frequency = 1)
    P22Fc5<-ts(x1$P22H5,frequency = 1)

    P30Fc1<-ts(x1$P30H1,frequency = 1)
    P30Fc2<-ts(x1$P30H2,frequency = 1)
    P30Fc3<-ts(x1$P30H3,frequency = 1)
    P30Fc4<-ts(x1$P30H4,frequency = 1)
    P30Fc5<-ts(x1$P30H5,frequency = 1)

    P39Fc1<-ts(x1$P39H1,frequency = 1)
    P39Fc2<-ts(x1$P39H2,frequency = 1)
    P39Fc3<-ts(x1$P39H3,frequency = 1)
    P39Fc4<-ts(x1$P39H4,frequency = 1)
    P39Fc5<-ts(x1$P39H5,frequency = 1)

    P48Fc1<-ts(x1$P48H1,frequency = 1)
    P48Fc2<-ts(x1$P48H2,frequency = 1)
    P48Fc3<-ts(x1$P48H3,frequency = 1)
    P48Fc4<-ts(x1$P48H4,frequency = 1)
    P48Fc5<-ts(x1$P48H5,frequency = 1)
    
    P50Fc1<-ts(x1$P50H1,frequency = 1)
    P50Fc2<-ts(x1$P50H2,frequency = 1)
    P50Fc3<-ts(x1$P50H3,frequency = 1)
    P50Fc4<-ts(x1$P50H4,frequency = 1)
    P50Fc5<-ts(x1$P50H5,frequency = 1)
    
    P55Fc1<-ts(x1$P55H1,frequency = 1)
    P55Fc2<-ts(x1$P55H2,frequency = 1)
    P55Fc3<-ts(x1$P55H3,frequency = 1)
    P55Fc4<-ts(x1$P55H4,frequency = 1)
    P55Fc5<-ts(x1$P55H5,frequency = 1)
    
    P57Fc1<-ts(x1$P57H1,frequency = 1)
    P57Fc2<-ts(x1$P57H2,frequency = 1)
    P57Fc3<-ts(x1$P57H3,frequency = 1)
    P57Fc4<-ts(x1$P57H4,frequency = 1)
    P57Fc5<-ts(x1$P57H5,frequency = 1)
    
    P67Fc1<-ts(x1$P67H1,frequency = 1)
    P67Fc2<-ts(x1$P67H2,frequency = 1)
    P67Fc3<-ts(x1$P67H3,frequency = 1)
    P67Fc4<-ts(x1$P67H4,frequency = 1)
    P67Fc5<-ts(x1$P67H5,frequency = 1)
    
    # plot the series example for Product19
    autoplot(cbind(P19Bill,P19Fc1,P19Fc2,P19Fc3,P19Fc4,P19Fc5))
    autoplot(cbind(P19Bill,P19Fc1,P19Fc2,P19Fc3,P19Fc4,P19Fc5,P22Bill,P30Bill))
    
    
  # finding the optimal Lags
    VarP19 <- cbind(P19Bill,P19Fc1,P19Fc2,P19Fc3,P19Fc4,P19Fc5,P22Bill,P30Bill)
    # bindet die beiden Variablen aneinander
  
    LagP19 <- VARselect(VarP19,lag.max = 100,type = "const")
    LagP19$selection
    # zeigt und die Daten der selection criteria an, diesen wert sollten wir
    #im Var model als p nehmen
    # haben keine wieteren Infos ueber Daten, daher die Standardeinstellungen
    
    ModelP19 <- VAR(VarP19,p = 14, type = "const", season = NULL, exog = NULL)
    # mit "const" wird angenommen, dass es keinen (saisonalen)
    # Trend in den Daten gibt
    # dies konnte auch beim accurancy test gezeigt werden?!

    summary(ModelP19)
    # Auswertung:
    # roots of the char... beschreibt ob unser system stabil ist (wenn alle
    # Werte innerhalb des Unit circles sind)= keine strenuous? roots
    # im p wert der Tabelle kann abglesen werden, ob es ein significantes
    # lag gibt
    
    
    #---! hier muessen wir die ganzen Tests einfuegen 
    
    
    #forecast mit Var
    forecast2018Q1P19 <- predict(ModelP19,n.ahead = 13, ci = 0.95)
    forecast2018Q1P19
    forecast2018Q1P19$fcst$P19Bill
    fanchart(forecast2018Q1P19, names ="P19Billing")
    fanchart(forecast2018Q1P19, names ="P19 Fc1")
    
 
    
#--------------Tests zur Validierung des Models
    #Quellen 
      #https://www.youtube.com/watch?v=qyGlB4cqZ9Q&list=PLEuzmtv9IuT88IrQDHqz100twn85HxWfV&index=2
    
    
   
  #serial correlation
    Serial19 <- serial.test(ModelP19, lags.pt = 14, type = "PT.asymptotic")
    #asympthotic geht davon aus, dass wir einen normalen typ haben
    Serial19
    # ist der Test nicht signifikant (also ueber 0,05), so ist er bestdanden
    # Bestanden ist gut, weil wir keine serial korrelation in Var haben wollen
    # --! wir bestehen den Test nicht! --> Ursachen? welche Folgen?
    # bedeutet, dass Ts noch kein weißes Rauschen ist--> also starke Muster noch
    #erkennbar, Informationen die genutzt werden koennten sind also noch nicht im 
    #Modell enthalten --> wuerde weitere aAnalyse benoetigen, Kombination mit anderen
    #Fc Modellen dankbar
    
    
  # heteroscedastisity 
    # wollen wir auch nicht im Modell haben, da es in time series zu "arch
    # effects" kommt. Es ist ein Test für volatilität
    Arch19 <- arch.test(ModelP19, lags.multi = 14, multivariate.only = TRUE)
    Arch19
    # ist der Wert über 0.05, Test bestanden, suffert nicht von hetero...
    # unser Modell leidet nicht unter heteroscedastisity
  
  # Test for Normal Distribution of Residuals 
    # we want them to be normally distributed, dafür 3 verschiedene Tests
    Norm19 <- normality.test(ModelP19, multivariate.only = TRUE)
    Norm19
    # test sollte ebenfalls über 0.05 sein; ist jedoch nicht soo schlimm
    # wenn der Test nicht bestanden wird
    # unser Model besteht nicht, also sind die Residuals nicht normalverteilt
    
  # Testing for structural breaks in the residuals
    # wir wollen keine Breaks in ihnen haben, Test für Stabilitaet
    str(ModelP19)
    Stability19 <- stability(ModelP19,type= "OLS-CUSUM")
    plot(Stability19)
    # hopefully no points which exeeds the both confidence intervals (red)
    # --> besteht den Test
    
    
  #Autokorrelationsfunktion 
    # acf beschreibt, ob es einen signifikanten zusammenhang zwischen
    #beobacteten Messeregbnissen zu unterschiedlichen Beobachtungszeitpkt
    #gibt. richtung 1 bedeutet signifikanter zusammenhang
    acf(P19Bill, main="ACF for P19 Billing")
    acf(P22Bill, main="ACF for P22 Billing")
    

    
    
#------- Crossvalidation/ Accurency test
  #Aufteilung der Datensatze
    # Forecasts und Test immer fuer ein Quater durchfuehren, 
    # da lag 14 sollten wir mit Testdaten, die laenger sind als ein Quater 
    #Testen
    
    #Trainingsdaten 
      J2016 <- df[66:117,]
      # # Jahresweise Aufteilung 
      # J2016 <- df[66:117,]
      # J2015 <- df[118:169,]
      # J2014 <- df[170:222,]
      # #--! fuer weitere Crossvalidation sollten wir auch andere Zeitpunkte
      # # auswaehlen (nicht immer den Anfang des Jahres, Gefahr eines syst. Fehlers)
      # # Beachte: nicht zu weit in Vergangenheit, da aktuellste Daten meist besser
      # # die Unternehmensituation wiederspiegeln
      
      J2016P19Bill<- ts(J2016$P19Bill,frequency = 1)
      J2016P22Bill<- ts(J2016$P22Bill,frequency = 1)
      J2016P30Bill<- ts(J2016$P30Bill,frequency = 1)
      J2016P19Fc1<-ts(J2016$P19H1,frequency = 1)
      J2016P19Fc2<-ts(J2016$P19H2,frequency = 1)
      J2016P19Fc3<-ts(J2016$P19H3,frequency = 1)
      J2016P19Fc4<-ts(J2016$P19H4,frequency = 1)
      J2016P19Fc5<-ts(J2016$P19H5,frequency = 1)
      autoplot(cbind(P19Bill,P19Fc1,P19Fc2,P19Fc3,P19Fc4,P19Fc5,P22Bill,P30Bill))
      
      VarJ2016P19 <- cbind(J2016P19Bill,J2016P19Fc1,J2016P19Fc2,J2016P19Fc3,J2016P19Fc4,J2016P19Fc5,J2016P22Bill,J2016P30Bill)
      LagJ2016P19 <- VARselect(VarJ2016P19,lag.max = 20,type = "const")
      LagJ2016P19$selection # p=4 wird empfohlen 
      ModelJ2016P19 <- VAR(VarJ2016P19,p = 4, type = "const", season = NULL, exog = NULL)
      summary(ModelJ2016P19)
      
      fc2017Q1P19 <- predict(ModelJ2016P19,n.ahead = 12, ci = 0.95)
      fc2017Q1P19Bill <- fc2017Q1P19$fcst$J2016P19Bill
      fc2017Q1P19BillValue <- fc2017Q1P19Bill[,1]
      
      #mit season=True
      ModelJ2016P19Season <- VAR(VarJ2016P19,p = 4, type = "const", season = TRUE, exog = NULL)
      summary(ModelJ2016P19)
      fc2017Q1P19Season <- predict(ModelJ2016P19Season,n.ahead = 12, ci = 0.95)
      fc2017Q1P19BillSeason <- fc2017Q1P19Season$fcst$J2016
      fc2017Q1P19BillSeasonValue <- fc2017Q1P19Bill[,1]
      
    #Testdaten 
      J2017Q1 <- df[54:65,]
      
      #Prod19
        J2017Q1P19 <-J2017Q1$P19Bill
        J2017Q1P19
       
   #Accurancy
      accuracy(fc2017Q1P19BillValue,J2017Q1P19)
      accuracy(J2017Q1$P19H1,J2017Q1P19)  
      accuracy(fc2017Q1P19BillSeasonValue,J2017Q1P19)
      # bedeutet unser Fc ist schlechter als der aktuellste, weiterhin kein 
      #Unterschied zw. season oder nicht
      
      #Darstellung
      plot(fc2017Q1P19BillValue,
           main="Forecasts fuer Q1 2017")
      lines(J2017Q1$P19H1,col=2)
      lines(fc2017Q1P19BillSeasonValue, col=3)
      lines(J2017Q1P19)
      legend("topright",lty=1, col=c(4,2,3),
             legend = c("Standardvar", "Fc1","SeasonVar"))
      
      #MSE (Quelle:https://www.youtube.com/watch?v=4z_2qnAhTvk)
      eVar=J2017Q1P19-fc2017Q1P19BillValue
      MSE_Var <-mean(eVar^2) %>%
          sqrt()
     
      e_Fc1=J2017Q1P19-J2017Q1$P19H1
      MSE_Fc1 <-mean(eVar^2) %>%
        sqrt()
      MSE_Var
      MSE_Fc1
      # Gleicher MSE Wert 82212
      
#------- Crossvalidation/ Accurency test--
  
    
    #--! Weitere to dos: 
    # --!Uebertrag der Forecasts in excel 
    
    #nice to have
    #--> Quartals / Jahresbericht Infineon nachschlagen und vergleichen 
    #--! Beschriftung der Graphen anpassen (x-Achse mit den Jahren/ Monaten?,
    #yAchse in richtigem Zahlenformat)
    