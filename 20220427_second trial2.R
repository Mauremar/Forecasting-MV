#Version 20220427_1 Marc


#Notes
# um die Daten besser anzeigen zu k?nnen k?nnten wir 
# das due Date ?ndern, frage ist in welches Format
#
#install.packages("readxl")
library(readxl)
#install.packages("tidyverse")
library(tidyverse)

# privat 
  #setwd("C:/Users/marcm/OneDrive/One_Note/TUHH/Projektseminar/Datenanalyse R")
# Draeger
  setwd("C:/Users/mauremar/Desktop/Privat/Uni/R")
#getwd()
df<-read_excel("Daten_ForecastingChallenge - Q42017.xlsx")
str(df) #shows the structure of the data

# filter, erase "nan"
idx <- which(df[,"Billing"]!="nan")
x1 <- df[idx,]

# difference between forecast and billing
x1$diff <- as.numeric(unlist(x1[,5])) - as.numeric(unlist(x1[,6]))

plot(x1$Fc_horizon, x1$diff, col=2)# Alle DIfferenzen von allen Produkten 
hist(x1$diff, breaks=1000)          # Wie die Differenzen verteilt sin 



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



#ggplot2
#library(ggplot2)
# ggplot(data=filter19,aes(x = Due_date, y = Fc_and_order, group=1))+
#   geom_line(aes(y=Fc_and_order,color="0",size=2))+
#   geom_point(aes(y=Fc_and_order,color="3",size=1))

# ggplot(data=filter19Kw,aes(x = Fc_date, y = Fc_and_order, group=1))+
#   geom_line(aes(y=Fc_and_order,col="red",size=1))+
#   geom_point(aes(y=Fc_and_order,col="grey3",size=1))


