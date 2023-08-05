#Cargar paquetes----
library(tidyverse)
library(openxlsx)
library(magrittr)
library(dplyr)
library(tibble)
library(ggplot2)
library(forecast)
library(patchwork)
library(readxl)

#Cargar achivos
empresas<-read.xlsx('Proyecto/balances_2014.xlsx',sheet='v2014_activo1')

#Cambiamos los nombres de las variables
empresas<-empresas %>% rename(Empresa=nombre_cia,Status=situacion,Tipo_de_empresa=tipo,Pais=pais,Provincia=provincia
                              ,Canton=canton,Ciudad=ciudad,Actividad_economica=ciiu4_nivel1,Subactividad=ciiu4_nivel6)

#Cambiamos a tibble
empresas<-tibble::as_tibble(empresas)
glimpse(empresas)

#Obtenemos las columnas de solvencia y liquidez
empresas_1<-empresas %>% 
  mutate(Liquidez_corriente=v345/v539, Endeudamiento_Del_activo=v599/v499, Endeudamiento_patrimonial=v599/v698, 
         Endeudamiento_del_activo_fijo=v698/v498,Apalancamiento=v499/v698) %>% 
  select(ruc,Empresa,Status,Tipo_de_empresa, Pais, Provincia, Canton, Ciudad, Actividad_economica,
         Subactividad, Liquidez_corriente, Endeudamiento_Del_activo, Endeudamiento_patrimonial, 
         Endeudamiento_del_activo_fijo, Apalancamiento) %>% view

#crear una tabla resumiendo el num total de empresas por actividad economica y por actividad economica en cada canton. 
#Debe ser un dt aparte

empresas_segun_AE<-empresas_1 %>% group_by(Actividad_economica,Canton) %>% summarise(NN = n()) %>% 
  arrange(desc(NN)) %>% view()


#primero nos deshacemos de los NA porque no ayudaran a nuestro analisis y como son aprox 1655 datos los eliminamos

empresas_2<-empresas_1 %>% drop_na() %>% view()


#¿El endeudamiento del activo fue mayor en empresas micro + pequeñas vs. grandes?

p1 <- empresas_2 %>%
  left_join(empresas, by = "ruc") %>%
  select(ruc, tamanio, Endeudamiento_del_activo_fijo)

p1 <- p1 %>%
  filter(Endeudamiento_del_activo_fijo !=  "Inf") 

p1 <- p1 %>%
  filter(Endeudamiento_del_activo_fijo !=  "-Inf") 

p12 <- aggregate(Endeudamiento_del_activo_fijo ~ tamanio, data = p1, FUN = sum) %>% view


#¿La liquidez por tipo de compañía es diferente entre aquellas empresas que tienen más de
#60 trabajadores directos y que cuenta con 100 a 800 trabajadores administrativos?

p2 <- merge(empresas_2, empresas, by = "ruc", all = TRUE)
p2 <- p2 %>%
  filter(Liquidez_corriente !=  "Inf") 

p2 <- p2 %>%
  filter(Liquidez_corriente !=  "-Inf") 

p2 <- p2 %>%
  select(ruc, Liquidez_corriente,trab_direc, trab_admin,Tipo_de_empresa.y)
p2$trabajadores <- ifelse(p2$trab_direc > 62 & p2$trab_admin >= 100 & p2$trab_admin <= 800, "1","0" )

p22 <- aggregate(Liquidez_corriente ~ trabajadores+Tipo_de_empresa.y, data = p2, FUN = sum) %>%  view


#Describe el top 10 de empresas con mayor apalancamiento.

top10_apalancamiento <- empresas_2 %>%
  arrange(Apalancamiento)
  
head(top10_apalancamiento,10) %>% view

#Crea una tabla resumiendo el número total de empresas por actividad económica y por actividad
#económica por cada cantón. La tabla simplemente debe aparecercomo un data frame o tibble en tu script. 


frecuencia <- empresas_2 %>% count(Actividad_economica)
frecuencia= as.data.frame(frecuencia)

p3<- as.data.frame(table(empresas_2$Canton, empresas_2$Actividad_economica)) %>% view


#Gráficamente muestra el comparativo de los indicadores financieros de liquidez y
#solvencia por Status y provincia. 

#Para liquidez: Tenemos solo liquidez corriente

p4 <- empresas_2 %>% filter(Liquidez_corriente !=  "Inf" & Liquidez_corriente !="-Inf") 

g1<-ggplot(p4, aes(x = Provincia, fill = Liquidez_corriente)) +geom_bar()+labs(title='Liquidez por provincia')
g2<-ggplot(p4, aes(x = Status, fill = Liquidez_corriente)) +geom_bar()+labs(title='Liquidez por status')


#Para solvencia:Tenemos end. del activo, end. patrimonial, end. activo fijo, apalancamiento

p5<-empresas_2 %>% filter(Endeudamiento_Del_activo !=  "Inf" & Endeudamiento_Del_activo !="-Inf")
p6<-empresas_2 %>% filter(Endeudamiento_patrimonial !=  "Inf" & Endeudamiento_patrimonial !="-Inf") 
p7<-empresas_2 %>% filter(Endeudamiento_del_activo_fijo !=  "Inf" & Endeudamiento_del_activo_fijo !="-Inf") 
p8<-empresas_2 %>% filter(Apalancamiento !=  "Inf" & Apalancamiento !="-Inf") 


g3<-ggplot(p5, aes(x = Provincia, fill= Endeudamiento_Del_activo)) +geom_bar()+labs(title='Endeudamiento del activo en provincias')
g4<-ggplot(p5, aes(x = Status, fill = Endeudamiento_Del_activo)) +geom_bar()+labs(title='Endeudamiento del activo por status')
g5<-ggplot(p6, aes(x = Provincia, fill = Endeudamiento_patrimonial)) +geom_bar()+labs(title='Endeudamiento patrimonial por provincia')
g6<-ggplot(p6, aes(x = Status, fill = Endeudamiento_patrimonial)) +geom_bar()+labs(title='Endeudamiento patrimonial por status')
g7<-ggplot(p7, aes(x = Provincia, fill = Endeudamiento_del_activo_fijo)) +geom_bar()+labs(title='Endeudamiento del activo fijo por provincia')
g8<-ggplot(p7, aes(x = Status,  fill= Endeudamiento_del_activo_fijo)) +geom_bar()+labs(title='Endeudamiento del activo fijo por status')
g9<-ggplot(p8, aes(x = Provincia, fill = Apalancamiento)) +geom_bar()+labs(title='Apalancamiento por provincia')
g10<-ggplot(p8, aes(x = Status, fill = Apalancamiento)) +geom_bar()+labs(title='Apalancamiento por status')


#Gráficamente muestra el comparativo de los indicadores financieros de liquidez y
#solvencia por tipo de empresa 

#Para liquidez: Tenemos solo liquidez corriente
g11<-ggplot(p4, aes(x = Tipo_de_empresa, y = Liquidez_corriente)) +geom_point()+labs(title='Liquidez por tipo de empresa')

#Para solvencia:Tenemos end. del activo, end. patrimonial, end. activo fijo, apalancamiento

g12<-ggplot(p5, aes(x = Tipo_de_empresa, fill = Endeudamiento_Del_activo)) +geom_bar()+labs(title='Endeudamiento del activo por Tipo de empresa')
g13<-ggplot(p6, aes(x = Tipo_de_empresa, fill = Endeudamiento_patrimonial)) +geom_bar()+labs(title='Endeudamiento patrimonial por Tipo de empresa')
g14<-ggplot(p7, aes(x = Tipo_de_empresa,  fill= Endeudamiento_del_activo_fijo)) +geom_bar()+labs(title='Endeudamiento del activo fijo por Tipo de empresa')
g15<-ggplot(p8, aes(x = Tipo_de_empresa, fill = Apalancamiento)) +geom_bar()+labs(title='Apalancamiento por Tipo de empresa')



