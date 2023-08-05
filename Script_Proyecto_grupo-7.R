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


setwd("C:/Users/cavila/Desktop")


#Cargar achivos
empresas<-read.xlsx('balances_2014.xlsx',sheet='v2014_activo1')

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


#mostrar graficamente los indicadores de liquidez y solvencia por status y provincia

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


p12 <- aggregate(Endeudamiento_del_activo_fijo ~ tamanio, data = p1, FUN = sum)


p12


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




p22 <- aggregate(Liquidez_corriente ~ trabajadores+Tipo_de_empresa.y, data = p2, FUN = sum)


p22



ggplot(empresas_2)+geom_point(aes(Status, Liquidez_corriente))+ 
  geom_smooth(aes(Status, Liquidez_corriente))+facet_wrap(~Pais)

ggplot(empresas_2) +
  geom_histogram(mapping = aes(x = Provincia, fill =Empresa))

ggplot(empresas_2, aes(x=Liquidez_corriente))+
  geom_bar()+
  facet_wrap(~Tipo_de_empresa)+
  labs(title='Grafico liqu vs solvencia')

#Describe el top 10 de empresas con mayor apalancamiento.


top10_apalancamiento <- empresas_2 %>%
  arrange(Apalancamiento)

head(top10_apalancamiento,10)

#Crea una tabla resumiendo el número total de empresas por actividad económica y por actividad económica por cada cantón. La tabla simplemente debe aparecercomo un data frame o tibble en tu script. 


frecuencia <- empresas_2 %>%
  count(Actividad_economica)
frecuencia= as.data.frame(frecuencia)

p3= as.data.frame(table(empresas_2$Canton, empresas_2$Actividad_economica))






#Gráficamente muestra el comparativo de los indicadores financieros de liquidez y
#solvencia por Status y provincia. 
p4 <- empresas_2 %>%
  filter(Liquidez_corriente !=  "Inf") 

p4 <- empresas_2 %>%
  filter(Liquidez_corriente !=  "-Inf") 


g1=ggplot(p4, aes(x = Provincia, y = Liquidez_corriente)) +geom_point()
g1


g2=ggplot(p4, aes(x = Status, y = Liquidez_corriente)) +geom_point()
g2

g3=ggplot(p4, aes(x = Tipo_de_empresa, y = Liquidez_corriente)) +geom_point()
g3
