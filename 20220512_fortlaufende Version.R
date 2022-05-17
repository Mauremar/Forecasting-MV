#Version 20220516_7 
#Fortlaufende Version
# Versuch eine fortlaufende Version zu erstellen 

#hallo

#Notes
# um die Daten besser anzeigen zu koennen koennten wir
# das due Date aendern, frage ist in welches Format
#

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

# privat
setwd("C:/Users/marcm/OneDrive/One_Note/TUHH/Projektseminar/Datenanalyse R")
# Draeger
#setwd("C:/Users/mauremar/Desktop/Privat/Uni/R")
getwd()
df<-read_excel(file.choose())
str(df) #shows the structure of the data
view(df)

#Daten vorfiltern ----------------------------------------------------
  

# #Fc_horizont herausnehmen
# df_lean=subset(df,select = c(-Fc_horizon))
# df_lean

# # filter, erase "nan"
# x1<-filter(df,Billing!="nan")
# view(x1)
x1<- na.omit(df) # löscht alle daten, die keinen Inhalt haben
view(x1)
## mit df_lean
#x1<-filter(df_lean,Billing!="nan")

# filter auf ein bestimmes Product
Productx<-filter(x1,Sp_number =="Product_19")
Productx

# filter nur ein Jahr darstellen Woche 01 bis 52 z.B. 201401...201452, 201501
Jahrx<-subset(Productx,Due_date > 201400 & Due_date < 201453)
Jahrx
  
  
  # # hier könnte die implementierung folgen 
  # newData <- matrix(nrow = 156,ncol = 141)
  # view(newData)
  # newData[,1]<- 2
  # view(newData)
  # newData[,1] <
  # 

# Adding the difference between forecast and billing //
x1$diff <- as.numeric(unlist(x1[,5])) - as.numeric(unlist(x1[,6]))
## mit df_lean
#x1$diff <- as.numeric(unlist(x1[,4])) - as.numeric(unlist(x1[,5]))
x1
view(x1)

# 
#------------Einschub Forecasting
ggplot(data=x1)+ geom_point(mapping = aes(x=Due_date,y=Billing))
# declare time series variables


    #-----------pVar Versuch
    # view(x1)
    # varone <-pvargmm(
    #   dependent_vars = c("1","2","3","4","5","6","7","8","9","10","11","12","13"),
    #   lags = 1,
    #   exog_vars = c("Billing"),
    #   transformation = "fd",
    #   data = x1,
    #   panel_identifier = c("Sp_number", "Due_date"),
    #   steps = c("twostep"),
    #   system_instruments = TRUE,
    #   max_instr_dependent_vars = 99,
    #   min_instr_dependent_vars = 2L,
    #   collapse = FALSE
    # )
    # 
    # summary(varone)
    # 
    
    
    #-------------pVar ende


# Diff<-ts(x1$diff,start = c(201400),frequency = 12)
# Diff
# # bedeutet der startpunkt ist im dritten Monat von 1999 und die daten
# # gibt es vierteljählich (=4), wäres es monatlich wäre es 12
# Fc<- ts(x1$Fc_and_order,start = c(201400),frequency = 12)
# # plot the series
# autoplot(cbind(Diff,Fc))
# 
# #OLS
# OLS1 <- lm(Diff~Fc)
#      # Fc ist independent, Diff ist dependent
#      # das heisst wir gehen davon aus, dass Diff von Fc abhängig ist,
#      # bei der VAR methode wissen wir es allerdings nicht und lassen die
#      #Daten für sich sprechen
#      summary(OLS1)
# 
#   #Determine the persisitence of the ode,
#      #by determining the acf und p(partial)acf
#      # acf beschreibt, ob es einen signifikanten zusammenhang zwischen
#      #beobacteten Messeregbnissen zu unterschiedlichen Beobachtungszeitpkt
#      #gibt. richtung 1 bedeutet signifikanter zusammenhang
#      # Kreuzkorrelation könnte auch interessant sein
#     acf(Diff, main="ACF for Difference")
#     pacf(Diff, main="PACF for Difference")
#     #!acf ist nur aussagekräftig, wenn ich nicht 13 gleiche Billings habe!
#     # müsste also zwei unterschiedlich große Matritzen erstellen?
#     acf(Fc, main="ACF for Forecast and Order")
#     pacf(Fc, main="PACF for Forecast and Order")
# 
#     # finding the optimal Lags
#         # var braucht die anzahl der Autoregressive lags vorgegeben
#         okun.bv <- cbind(Diff, Fc)
#         # bindet die beiden Variablen aneinander
#         colnames(okun.bv) <- cbind("Difference","Fc Estimate")
#         # aendert den NAmen der Spalten
# 
#         lagselect <- VARselect(okun.bv,lag.max = 100,type = "const")
#         #!!! Warnung wenn man 1000 lags maximal eintellt berechnet R über 25 min!
#          # es soll die optimale anzahl an lags gefunden werden und es darf max
#         #10 lags geben. mit "const" wird angenommen, dass es keinen (saisonalen)
#         # Trend in den Daten gibt
#         lagselect$selection
#         # zeigt und die Daten der selection criteria an, diesen wert sollten wir
#         #im Var model als p nehmen
# 
#         #building Var Model
#             ModelOkun1 <- VAR(okun.bv,p = 2, type = "const", season = NULL, exog = NULL)
#             # bedeutet man geht von keinen Saisonalen effekten und keinen Exoten aus
#             summary(ModelOkun1)
#             # Auswertun:
#               # roots of the char... beschreibt ob unser system stabil ist (wenn alle
#               # Werte innerhalb des Unit circles sind)= keine strenuous? roots
#               # im p wert der Tabelle kann abglesen werden, ob es ein significantes
#               # lag gibt
#             
#         # hier würden jetzt einige tests kommen 
#             #forecast mit Var
#                   forecast <- predict(ModelOkun1,n.ahead = 4, ci = 0.95)
#                   fanchart(forecast, names ="Difference")
#                   fanchart(forecast, names ="Fc Estimate")
#                   # wie viele quaters ahead forecast werden, confidence intervall von 95% und das in
#                   # einem fanchart wiedergeben

# müssen überlegen, wie wir die Produkte mit inbeziehen
# Vorschlag, immer Matrix nach due Dates Producten und deren einzelner FOrecast,
# und dessen differenz (wird sehr breit....)
# Besprechen mit Vanessa und Sebastian in der nächsten Stunde

#--Einschub Forecast Ende




# #plots
# plot(x1$Fc_horizon, x1$diff, col=2)# Alle DIfferenzen von allen Produkten
# hist(x1$diff, breaks=1000)          # Wie die Differenzen verteilt sin
# 
# #billing über due date für ein Product
# plot(Jahrx$Due_date, Jahrx$Billing, col=3)
# 
# # One variable, by groups // funktioniert nur, wenn in Sp_number auch etwas steht
# boxplot(x1$diff ~ Sp_number,
#         data = x1,
#         horizontal = T)
# 
# #mit ggplot2 "Histogram all Products" // auch möglich mit diff dann x1 statt df
# df %>%
#   ggplot(aes(x = Fc_and_order,
#              fill = Sp_number)) +
#   geom_density(alpha = 0.5) +
#   theme(legend.position = "right")
# 
# 
# #erstellt mehrere Scatterplots, nützlich, um Muster zu erkennen (Erklärung siehe One note)
# # x1 %>%
# #   select(Due_date:diff) %>%
# #   plot()
# 
# 
# 
# 
# #p_load(psych)
# # describe ist more powerful als summary
# # describe(x1$Fc_and_order which(x1[,"Sp_number"]=="Product_19") )
# # Funktioniert beides noch nicht
# 
# # versuche pearson corralation coefficient herauszufinden:
# str((x1))
# x2<-x1
# 
# x2$Billing <-as.numeric(unlist(x2[,6]))  # dient dazu den type of billing auf numeric zu setzen,
# # sorgt gleichzeitig jedoch, das Sp_number sich auch verändert
# x2$Sp_number <-as.numeric(unlist(x2[,1]))
# str(x1)
# str(x2)
# cor(x2) # hierfür muss x2 komplett numerisch sein
# # bedeutet das ergebnis, dass eine Korrelation zwischen FC and Order und Billing besteht?!
# # müsste noch " Product" durch eine Zahl ersetzen ersetzen, was würde mir das bringen?
# # # auf 2 NK gerundes //klappt noch nicht
# # UCBAdmissions %>%
# # x2 %>%
# #   + cor()%>%
# #   +round(2)
# 
# 
# # für eine Detailiertere Correlationsanalyse zwischen 2 Variablen use this:
# cor.test(x2$Fc_and_order,x2$Billing)
# cor.test(x2$Fc_date,x2$diff)
# 
# # um alle Korrelatioskoeffizienten in einer und alle  p values in einer
# # zweiten Tabelle anzuzeigen:
# p_load(Hmisc)
# 
# # die Daten müssen dafür in einer Matrix vorliegen und alle gleich sein:
# x2 %>%
#   as.matrix() %>%
#   rcorr()


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


# #erster Var versuch
# #load required packages for running VAR
#      # install.packages("urca")
#      # install.packages("vars")
#      # install.packages("mFilter")
#      # install.packages("tseries")
#      # install.packages("forecast")
#      # install.packages("tidyverse")
#      
#      library(urca)
#      library(vars)
#      library(mFilter)
#      library(tseries)
#      library(forecast)
#      library(tidyverse)
# 
# load the data
# okun<-read.csv(file.choose())
#     # setwd("C:/Users/mauremar/Desktop/Privat/Uni/R")
#     # getwd()
#     # okun<-read.csv("20220508_VarTut_SampleVAR.csv")
# view(okun)
#   #a sample graph
#      ggplot(data=okun)+ geom_point(mapping = aes(x=unem,y=real_gdp_growth))
#   # declare time series variables
#      gdp<-ts(okun$real_gdp_growth,start = c(1999,3),frequency = 4)
#      # bedeutet der startpunkt ist im dritten Monat von 1999 und die daten
#      # gibt es vierteljählich (=4), wäres es monatlich wäre es 12
#      unem<- ts(okun$unem,start = c(1999,3),frequency = 4)
#   # plot the series
#      autoplot(cbind(gdp,unem))
# 
#   #OLS
#      OLS1 <- lm(gdp~unem)
#      # unem ist independent, gdp ist dependent
#      # das heisst wir gehen davon aus, dass gdp von unem abhängig ist,
#      # bei der VAR methode wissen wir es allerdings nicht und lassen die
#      #Daten für sich sprechen
#      summary(OLS1)
# 
#   #Determine the persisitence of the ode,
#      #by determining the acf und p(partial)acf
#      # acf beschreibt, ob es einen signifikanten zusammenhang zwischen
#      #beobacteten Messeeregbnissen zu unterschiedlichen Beobachtungszeitpkt
#      #gibt. richtung 1 bedeutet signifikanter zusammenhang
#      # Kreuzkorrelation könnte auch interessant sein
#     acf(gdp, main="ACF for Real GDP Growth")
#     pacf(gdp, main="PACF for Real GDP Growth")
# 
#     acf(unem, main="ACF for unemployment")
#     pacf(unem, main="PACF for unemployment")
# 
#   # finding the optimal Lags
#     # var braucht die anzahl der Autoregressive lags vorgegeben
#     okun.bv <- cbind(gdp, unem)
#     # bindet die beiden Variablen aneinander
#     colnames(okun.bv) <- cbind("GDP","Unemployment")
#     # aendert den NAmen der Spalten
# 
#     lagselect <- VARselect(okun.bv,lag.max = 10,type = "const")
#     # es soll die optimale anzahl an lags gefunden werden und es darf max
#     #10 lags geben. mit "const" wird angenommen, dass es keinen (saisonalen)
#     # Trend in den Daten gibt
#     lagselect$selection
#     # zeigt und die Daten der selection criteria an, diesen wert sollten wir
#     #im Var model als p nehmen
# 
#   # building Var Model
#     ModelOkun1 <- VAR(okun.bv,p = 4, type = "const", season = NULL, exog = NULL)
#     # bedeutet man geht von keinen Saisonalen effekten und keinen Exoten aus
#     summary(ModelOkun1)
#     # Auswertun:
#       # roots of the char... beschreibt ob unser system stabil ist (wenn alle
#       # Werte innerhalb des Unit circles sind)= keine strenuous? roots
#       # im p wert der Tabelle kann abglesen werden, ob es ein significantes
#       # lag gibt
# 
# # Tests für das Var Model
#     #serial correlation
#     Serial1 <- serial.test(ModelOkun1, lags.pt = 12, type = "PT.asymptotic")
#     #asympthotic geht davon aus, dass wir einen normalen typ haben
#     Serial1
#     # ist der Test nicht signifikant (also ueber 0,05), so ist er bestdanden
#     # Bestanden ist gut, weil wir keine serial korrelation in Var haben wollen
# 
#     # heteroscedastisity
#     # wollen wir auch nicht im Modell haben, da es in time series zu "arch
#     # effects" kommt. Es ist ein Test für volatilität
#     Arch1 <- arch.test(ModelOkun1, lags.multi = 12, multivariate.only = TRUE)
#     Arch1
#     # ist der Wert über 0.05, Test bestanden, suffert nicht von hetero...
# 
#     # Normal diestribution of the residuals
#       # we want them to be normally distributed, dafür 3 verschiedene Tests
#     Norm1 <- normality.test(ModelOkun1, multivariate.only = TRUE)
#     Norm1
#       # test sollte ebenfalls über 0.05 sein; ist jedoch nicht soo schlimm
#       # wenn der Test nicht bestanden wird
# 
#     # Testing for structural breaks in the residuals
#       # wir wollen keine Breaks in ihnen haben, Test für Stabilitaet
#       Stability1 <- stability(ModelOkun1,type = "OLS-CUSUM")
#       plot(Stability1)
#       # hopefully no points which exeeds the both confidence intervals (red)
# 
# #   # Granger Causality:
#       GrangerGDP <- causality(ModelOkun1, cause ="GDP")
#       GrangerGDP
# 
#       GrangerUneployment <- causality(ModelOkun1, cause ="Unemployment")
#       GrangerUneployment
#       # Granger causality beschreibt wie und ob kausalitäten bestehen (ob das Model
#       # die Ursache für GDP ist). Geht von der Nullhypothese aus, dass der GDP nicht
#       # granger cause unemployment (p value größer 0.05 __> H0 kann nicht verworfen werden)
# 
#     #Impulseresponefunction
#       GDPirf <- irf(ModelOkun1, impulse = "Unemployment", response =  "GDP", n.ahead = 20, boot = TRUE)
#       plot(GDPirf, ylab ="GDP", main = "Shock from unemployment")
#       # wie das System wirkt wenn unemployment geschockt wird und welche Auwirkungen es
#       # auf GDP hat. Wir wollen 20 Werte in der Zukunft sehen
#       # im plot werden die Konfidenzintervalle dargestellt
# 
#       Unemploymentirf <- irf(ModelOkun1, impulse = "GDP", response =  "Unemployment", n.ahead = 20, boot = TRUE)
#       plot(Unemploymentirf, ylab ="Unemployment", main = "Shock from GDP")
# 
#     # varianz decomposition
#       FEVD1 <- fevd(ModelOkun1, n.ahead = 10)
#       plot(FEVD1)
#       # wie viel die Variablen beeinflusst werden, durch schocks. Im graphen sieht man,
#       # dass bei einem schok von gdp hauptsächlich gdp beeinflusst wird
# 
#     # forecast mit Var
#       forecast <- predict(ModelOkun1,n.ahead = 4, ci = 0.95)
#       fanchart(forecast, names ="GDP")
#       view(forecast)
#       fanchart(forecast, names ="Unemployment")
#       # wie viele quaters ahead forecast werden, confidence intervall von 95% und das in
#       # einem fanchart wiedergeben


# structured var video Philipine dataset---------------
    #   library(urca)
    #   library(vars)
    #   library(mFilter)
    #   library(tseries)
    #   library(TSstudio)
    #   library(forecast)
    #   library(tidyverse)
    #   #Loading the Dataset
    #   macro <- read_csv(file.choose())
    #   head(macro)
    #   #Creating thee Time Series Objectives
    #   y <- ts(macro$`Output Gap`, start = c(2000,1,1), frequency = 4)
    #   pi <- ts(macro$CPI, start = c(2000,1,1), frequency = 4)
    #   r <- ts(macro$RRP, start = c(2000,1,1), frequency = 4)
    #   #Time Series Plots
    #   ts_plot(y, title = "Output Gap", Xtitle = "Time", Ytitle = "Output Gap")
    #   ts_plot(pi, title = "Inflation Rate", Xtitle = "Time", Ytitle = "Inflation Rate")
    #   ts_plot(r, title = "Overnight Reverse Repurchase Rate", Xtitle = "Time", Ytitle = "RRP")
    #   #Setting the Restrictions
    #   amat <- diag(3)
    #   amat[2,1] <- NA
    #   amat[3,1] <- NA
    #   amat[3,2] <- NA
    #   amat
    #     # Var restrictions 
    #     # es wird eine idemtity matrix erstell (3x3), da drei variablen 
    #     # row 2 one wird zu NA verändert 
    #     # .... so dass das untere dreieck der Matrix frei ist 
    #     # output gap (erste spalte kann dabei die beiden anderen beeinflussen, 
    #     # Spalte 2 hat nur einfluss auf die dirtte und die dritte Spalte = interest
    #     # rate hat keinen Einfluss auf die anderen Spalten)
    #   
    #   #Buidling the Model
    #   sv <- cbind(y, pi, r)
    #   colnames(sv) <- cbind("OutputGap", "Inflation", "RRP")
    #   lagselect <- VARselect(sv, lag.max = 8, type = "both")
    #   lagselect$selection
    #   lagselect$criteria
    #   
    #   Model1 <- VAR(sv, p = 5, season = NULL, exog = NULL, type = "const")
    #   SVARMod1 <- SVAR(Model1, Amat = amat, Bmat = NULL, hessian = TRUE, estmethod =
    #                      c("scoring", "direct"))
    #   SVARMod1
    #     # estimating the model 
    #     # Standart var
    #     # S var: amat erlaubt die matrix of restictions einzusetzen 
    #     # Das Ergebniss zeigt und die Koeffizienten der einzelnen variablen
    #     # 
    #     
    # #Impulse Response Functions
    #   SVARog <- irf(SVARMod1, impulse = "OutputGap", response = "OutputGap")
    #   SVARog
    #   plot(SVARog)
    #   SVARinf <- irf(SVARMod1, impulse = "OutputGap", response = "Inflation")
    #   SVARinf
    #   plot(SVARinf)
    #   SVARrrp <- irf(SVARMod1, impulse = "Inflation", response = "RRP")
    #   SVARrrp
    #   plot(SVARrrp)
    #   #Forecast Error Variance Decomposition
    #   SVARfevd <- fevd(SVARMod1, n.ahead = 10)
    #   SVARfevd
    #   plot(SVARfevd)
  #-------------------------------------------SVAR ende    
      
# Panel Var in R 
      # gmm = general message of moments 
      # lag =1 da yearly data 
      # es gibt exog variables
      # transformation (es gibt zwei typen, meisten genutzt ist first difference)
      # fod kann mann auch noch nutzen !vergleichen, welches bessere Resultate 
      #liefert
      # ID ist cross section identifier? und year der time identifyer
      # twosteps iteration wird in der literatur empfohlen, der rest sind default
      # weitere möglihkeiten "onestep" oder "mstep"(=iterated efficient GMM estimator)
      #Einstellungen
    #install.packages("panelvar")
    # library(panelvar)
    # citation("panelvar")
    #   


#------------------Notizen----------------------
#-------Matrix in R--------------
# lege eine Matrix mit 140 Spalten an 
# Schema MatrixNeu: due Date|Fc für Fc P1 erste Woche|....|Fc P1 zweite Woche| Billing P1| Fc P2 erste Woche.....|
-
  # Schaue das Due Date der Rohdaten an und schreibe dieses + die Produktbezeichnung aus 
  # der Spalte "Sp_number" + das Billing in jeweils eine Spalte (aber gleiche Reihe) der neuen Matrix (MatrixNeu).
  # Nimm dann aus der Reihe des due Dates den Fc-and order Wert und trage diesen in die 
  #entsprechende Spalte der neuen Matrix ein.
  # Gehe in die Nächste Reihe der Rohdaten und überprüfe, ob die Produktbezeichnung
  #und das Due_date noch dem der vorherigen Reihe der neuen Matrix entsprechen. 
  # Ist dass der Fall, schreibe den entsprechenden Wert aus Fc_and Order in die 
  #richtige Spalte der neuen Matrix
  # gehe in die nächste Reihe der Rohdaten, vorgang wiederholen solange bis 
  #Produktbezeichnung oder Due date anders sind als die der vorangegangenen Reihe 
  #der neuen Matrix
#--> Vanessa mit Excel
#----------

#------------Fragen----------------
# wie kombiniere ich nur ein Due Date mit einem Billing für 13 Forecasts
# gegeben der Fall mit pvar, was ist unsere exogonous variable? und dependant
