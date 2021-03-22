library(dplyr)
library(lubridate)
library(ggplot2)
covid_navarra<-Casos_nav_2203

#Casos en Cendea, Pamp, Zizur

covid_study<-filter(covid_navarra, DesMun=="CIZUR MENOR"|DesMun=="ASTRÁIN" 
       |DesMun=="ERIETE"|
         DesMun=="GAZÓLAZ"|
         DesMun=="GUENDOLÁIN"|
         DesMun=="LARRAYA"|
         DesMun=="MURU ASTRÁIN"|
         DesMun=="PATERNÁIN"|
         DesMun=="ERIETE"|
         DesMun=="SAGUES"|
         DesMun=="UNDIANO"|
         DesMun=="ZARIQUIEGUI"|
         CodMun==747|
         CodMun==250)


covid_study_dates<-(covid_study$Fecha)%>% ymd()

#Casos Pamplona

covid_pamplona<-filter(covid_navarra, CodMun==747)

#Casos estudio entre Nov y Marzo

covid_study_Nov_marz<-covid_study%>%filter(Fecha >= as.Date('2020-11-06') & Fecha <= as.Date('2021-03-21'))


covid_pamp_Nov_feb<-covid_pamplona%>%filter(Fecha >= as.Date('2020-11-06') & Fecha <= as.Date('2021-02-02'))

#Formato tiempo

time<-(covid_study$Fecha) %>% ymd()
str(covid_study)

time_nov_mar<-(covid_study_Nov_marz$Fecha) %>% ymd()

time_pamp<-(covid_pamp_Nov_feb$Fecha) %>% ymd()

#Gráfico

cases_ts<-ggplot(data = covid_study_Nov_marz,
               aes(x = time_nov_mar,
                   y = NuevosCasos)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Date',
       y = 'covid cases')+
  scale_x_date(date_breaks = "1 month")


cases_ts

ggsave("covid_study.jpg")

#Pamplona

cases_ts_pamp<-ggplot(data = covid_pamp_Nov_feb,
                 aes(x = time_pamp,
                     y = NuevosCasos)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Date',
       y = 'covid cases')+
  scale_x_date(date_breaks = "1 month")


cases_ts_pamp

#Export CSV

write.csv(covid_study_Nov_marz,"C:\\Users\\juanc\\Desktop\\Seminarios\\Clases\\Cizur\\IRB Pamplona\\CHUM ammends\\Docs January 6th 2021\\BMJ pub\\covid_study_220321.csv", row.names = FALSE)

write.csv(covid_cendea_Nov_feb,"C:\\Users\\juanc\\Desktop\\Seminarios\\Clases\\Cizur\\IRB Pamplona\\CHUM ammends\\Docs January 6th 2021\\BMJ pub\\cendea_nov_feb21.csv", row.names = FALSE)

write.csv(covid_pamp_Nov_feb,"C:\\Users\\juanc\\Desktop\\Seminarios\\Clases\\Cizur\\IRB Pamplona\\CHUM ammends\\Docs January 6th 2021\\BMJ pub\\pamp_nov_feb21.csv", row.names = FALSE)



