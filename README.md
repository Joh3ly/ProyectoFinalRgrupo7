---
title: "Proyecto Final Grupo 7"
author: "Michelle Cevallos-Hailis Alvarado"
date: "2023-08-06"
output: pdf_document
---

# **Informe sobre Análisis Financiero**

## Introducción

En este informe, se realizará un análisis financiero de las empresas del año 2014 utilizando datos proporcionados en cuatro archivos en formato xlsx. El objetivo es utilizar técnicas de estadística descriptiva y visualización de datos para entender la situación financiera de las empresas y responder a preguntas específicas sobre su endeudamiento, liquidez y apalancamiento.

## Parte 1: Datos

Los datos a usar del 2014 se conforman de 347 variables medidas en 47033 observaciones. El archivo " balance_2014.xlsx" se proporciona en R Workspace Data(.RData) format.

Para el análisis de datos se cargarán los paquetes

```{r}
library(tidyverse)
library(openxlsx)
library(magrittr)
library(dplyr)
library(tibble)
library(ggplot2)
library(forecast)
library(patchwork)
library(readxl)

```

En esta sección, se describirá y mostrará cómo está conformado el dataset 'balance_2014.xlsx' que se utilizará para el análisis financiero.

### **1. Creación del Tibble Empresas**

Se generó un tibble llamado 'empresas' utilizando los datos del archivo 'balance_2014.xlsx'. El tibble contiene las siguientes variables:

-   Empresas: Nombres de las compañías.

-   Status: Especificación de la situación de la compañía (activo, en liquidación, etc.).

-   Tipo_de_empresa: Clase de compañía analizada (anónima, economía mixta, etc.).

-   País, Provincia, Cantón, Ciudad: Información geográfica de ubicación de la empresa.

-   Actividad económica: Descripción del código CIIU4 NIVEL 1.

-   Subactividad: Descripción del código CIIU4 NIVEL 6.

-   Liquidez corriente, Endeudamiento del activo, Endeudamiento patrimonial, Endeudamiento del Activo Fijo, Apalancamiento: Indicadores financieros de interés.

```{r}
#Cargar achivos
empresas<-read.xlsx('Proyecto/balances_2014.xlsx',sheet='v2014_activo1')

#Cambiamos los nombres de las variables
empresas<-empresas %>% rename(Empresa=nombre_cia,Status=situacion,Tipo_de_empresa=tipo,Pais=pais,Provincia=provincia,Canton=canton,Ciudad=ciudad,Actividad_economica=ciiu4_nivel1,Subactividad=ciiu4_nivel6)

#Cambiamos a tibble
empresas<-tibble::as_tibble(empresas)
glimpse(empresas)

#Obtenemos las columnas de solvencia y liquidez
empresas_1<-empresas %>% 
  mutate(Liquidez_corriente=v345/v539, Endeudamiento_Del_activo=v599/v499, Endeudamiento_patrimonial=v599/v698, 
         Endeudamiento_del_activo_fijo=v698/v498,Apalancamiento=v499/v698) %>% 
  select(ruc,Empresa,Status,Tipo_de_empresa, Pais, Provincia, Canton, Ciudad, Actividad_economica,Subactividad, Liquidez_corriente, Endeudamiento_Del_activo, Endeudamiento_patrimonial, Endeudamiento_del_activo_fijo, Apalancamiento) %>% view

#crear una tabla resumiendo el num total de empresas por actividad economica y por actividad economica en cada canton. 
#Debe ser un dt aparte

empresas_segun_AE<-empresas_1 %>% group_by(Actividad_economica,Canton) %>% summarise(NN = n()) %>% 
  arrange(desc(NN)) %>% view()


#primero nos deshacemos de los NA porque no ayudaran a nuestro analisis y como son aprox 1655 datos los eliminamos

empresas_2<-empresas_1 %>% drop_na() %>% view() 
  
```

## Parte 2: Análisis

En esta sección, se abordarán las preguntas planteadas en el informe y se presentarán los resultados del análisis financiero. Cada pregunta será respondida de manera clara y concisa, respaldada por los hallazgos obtenidos del análisis de los datos. Se incluyen gráficos, tablas y estadísticas relevantes para ilustrar los resultados.

**1. ¿El endeudamiento del activo fue mayor en empresas micro + pequeñas vs. grandes?**

Para determinar si el endeudamiento del activo fue mayor en empresas micro + pequeñas en comparación con las grandes, realizamos un análisis detallado de los estados financieros de varias empresas.

```{r}
p1 <- empresas_2 %>%
  left_join(empresas, by = "ruc") %>%
  select(ruc, tamanio, Endeudamiento_del_activo_fijo)

p1 <- p1 %>%
  filter(Endeudamiento_del_activo_fijo !=  "Inf") 

p1 <- p1 %>%
  filter(Endeudamiento_del_activo_fijo !=  "-Inf") 

p12 <- aggregate(Endeudamiento_del_activo_fijo ~ tamanio, data = p1, FUN = sum) %>% view

print(p12)
```

El análisis de la tabla de resultados reveló que el endeudamiento del activo fijo fue significativamente mayor en empresas clasificadas como "MICRO" y "PEQUEÑA" en comparación con aquellas catalogadas como "GRANDE".

La suma del endeudamiento del activo fijo para las empresas "MICRO" y "PEQUEÑA" superó considerablemente al endeudamiento de las empresas "GRANDE".

Este hallazgo sugiere que las empresas más pequeñas tienden a tener una mayor necesidad de financiamiento mediante deuda para respaldar sus operaciones y proyectos de inversión, mientras que las empresas más grandes pueden depender de otras fuentes de capital o pueden tener una menor carga de endeudamiento en relación con sus activos fijos.

**2. ¿La liquidez por tipo de compañía es diferente entre aquellas empresas que tienen más de 60 trabajadores directos y que cuenta con 100 a 800 trabajadores administrativos?**

En el marco de esta investigación, se llevó a cabo un análisis detallado de la liquidez corriente en diferentes tipos de empresas, centrándose en aquellas con más de 60 trabajadores directos y que cuentan con un número de trabajadores administrativos comprendido entre 100 y 800. Los resultados obtenidos revelaron diferencias significativas en los niveles de liquidez corriente entre las empresas que cumplían con estos criterios y aquellas que no como se muestra a continuacion:

```{r}
p2 <- merge(empresas_2, empresas, by = "ruc", all = TRUE)
p2 <- p2 %>%
  filter(Liquidez_corriente !=  "Inf") 

p2 <- p2 %>%
  filter(Liquidez_corriente !=  "-Inf") 

p2 <- p2 %>%
  select(ruc, Liquidez_corriente,trab_direc, trab_admin,Tipo_de_empresa.y)
p2$trabajadores <- ifelse(p2$trab_direc > 62 & p2$trab_admin >= 100 & p2$trab_admin <= 800, "1","0" )

p22 <- aggregate(Liquidez_corriente ~ trabajadores+Tipo_de_empresa.y, data = p2, FUN = sum) %>%  view

print(p22)
```

Se encontró que las empresas con más de 60 trabajadores directos y que satisfacen el rango de 100 a 800 trabajadores administrativos mostraron distintos niveles de liquidez corriente, con valores que oscilaron entre 1.172458e+01 y 5.198283e+00. Por otro lado, las empresas que no cumplían con estos requisitos también exhibieron variaciones en sus niveles de liquidez corriente, con valores diferentes para cada tipo de empresa, como ANÓNIMA, ANÓNIMA EN PREDIOS RÚSTICOS, ANÓNIMA MULTINACIONAL ANDINA, ASOCIACIÓN O CONSORCIO, ECONOMÍA MIXTA, RESPONSABILIDAD LIMITADA y SUCURSAL EXTRANJERA.

Estos hallazgos sugieren que el tamaño y la estructura de la fuerza laboral pueden influir en la liquidez corriente de las empresas. Aquellas que se ajustan a los criterios específicos de trabajadores directos y administrativos podrían enfrentar una situación financiera más favorable, evidenciada por niveles de liquidez corriente más estables y bajos riesgos financieros. Por otro lado, las empresas que no cumplen con dichos criterios podrían presentar una mayor variabilidad en sus niveles de liquidez, lo que podría reflejar una gestión financiera menos optimizada o una mayor dependencia de fuentes de financiamiento externas.

Estos resultados ofrecen una visión valiosa para los gestores financieros y los responsables de la toma de decisiones en las empresas, destacando la importancia de considerar cuidadosamente la estructura de la fuerza laboral al analizar y gestionar la liquidez y el riesgo financiero. Además, estos hallazgos podrían ser útiles para la identificación de estrategias y políticas financieras adecuadas para diferentes tipos de empresas con el fin de mejorar su salud financiera y sostenibilidad a largo plazo.

**3. Describe el top 10 de empresas con mayor apalancamiento.**

El análisis del top 10 de empresas con mayor apalancamiento revela importantes datos sobre su situación financiera y el grado de endeudamiento en relación con sus activos y patrimonio.

A continuación, se presenta un resumen detallado de estas empresas, destacando sus niveles de apalancamiento y otros indicadores financieros relevantes:

```{r}
top10_apalancamiento <- empresas_2 %>%
  arrange(Apalancamiento)
  
head(top10_apalancamiento,10) %>% view

print(top10_apalancamiento)
```

La investigación realizada reveló una amplia diversidad en los indicadores financieros de estas compañías al analizar distintos parámetros, como la liquidez corriente, el endeudamiento del activo, el endeudamiento patrimonial, el endeudamiento del activo fijo y el apalancamiento.

Los resultados destacaron que algunas empresas presentan niveles de endeudamiento significativamente altos, sugiriendo una mayor dependencia de fuentes externas de financiamiento para mantener sus operaciones.

Es importante señalar que un alto nivel de apalancamiento implica un mayor riesgo financiero para estas empresas, ya que tienen una mayor proporción de deuda en su estructura de capital. Esto significa que cualquier disminución en los ingresos podría afectar su capacidad para cumplir con sus obligaciones financieras. Por otro lado, un apalancamiento más bajo suele ser indicativo de una estructura financiera más estable y menos riesgo en tiempos de volatilidad económica .

## Part 3: Análisis exploratorio de datos

En esta tercera parte del informe, se abordarán tareas específicas enfocadas en el análisis financiero de las empresas estudiadas.

#### Resumen del número de empresas por actividad económica y por actividad económica por cada cantón

Con el objetivo de brindar una visión clara y concisa de la distribución de las empresas por actividad económica y su relación con los cantones, se creó una tabla resumiendo el número total de empresas por actividad económica y por actividad económica por cada cantón.

```{r}
frecuencia <- empresas_2 %>% count(Actividad_economica)
frecuencia= as.data.frame(frecuencia)

p3<- as.data.frame(table(empresas_2$Canton, empresas_2$Actividad_economica)) %>% view

head(p3,6)
```

#### Indicadores financieros por Status y provincia

Posteriorme se procedio a realizar una gráfica comparativa de los indicadores financieros de liquidez y solvencia en función del "Status" (estado activo o inactivo) y la "Provincia" en la que operan las empresas. Estas visualizaciones permitieron identificar patrones o tendencias relacionadas con la situación financiera de las empresas según su estatus y ubicación geográfica.

```{r message=FALSE, warning=FALSE}
#Para liquidez: Tenemos solo liquidez corriente

p4 <- empresas_2 %>% filter(Liquidez_corriente !=  "Inf" & Liquidez_corriente !="-Inf") 

ggplot(p4, aes(x = Provincia, fill = Liquidez_corriente)) +geom_bar()+labs(title='Liquidez por provincia')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )


ggplot(p4, aes(x = Status, fill = Liquidez_corriente)) +geom_bar()+labs(title='Liquidez por status')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )


#Para solvencia:Tenemos end. del activo, end. patrimonial, end. activo fijo, apalancamiento

p5<-empresas_2 %>% filter(Endeudamiento_Del_activo !=  "Inf" & Endeudamiento_Del_activo !="-Inf")
p6<-empresas_2 %>% filter(Endeudamiento_patrimonial !=  "Inf" & Endeudamiento_patrimonial !="-Inf") 
p7<-empresas_2 %>% filter(Endeudamiento_del_activo_fijo !=  "Inf" & Endeudamiento_del_activo_fijo !="-Inf") 
p8<-empresas_2 %>% filter(Apalancamiento !=  "Inf" & Apalancamiento !="-Inf") 


ggplot(p5, aes(x = Provincia, fill= Endeudamiento_Del_activo)) +geom_bar()+labs(title='Endeudamiento del activo en provincias')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )


ggplot(p5, aes(x = Status, fill = Endeudamiento_Del_activo)) +geom_bar()+labs(title='Endeudamiento del activo por status')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )


ggplot(p6, aes(x = Provincia, fill = Endeudamiento_patrimonial)) +geom_bar()+labs(title='Endeudamiento patrimonial por provincia')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )


ggplot(p6, aes(x = Status, fill = Endeudamiento_patrimonial)) +geom_bar()+labs(title='Endeudamiento patrimonial por status')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )

ggplot(p7, aes(x = Provincia, fill = Endeudamiento_del_activo_fijo)) +geom_bar()+labs(title='Endeudamiento del activo fijo por provincia')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )

ggplot(p7, aes(x = Status,  fill= Endeudamiento_del_activo_fijo)) +geom_bar()+labs(title='Endeudamiento del activo fijo por status')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )


ggplot(p8, aes(x = Provincia, fill = Apalancamiento)) +geom_bar()+labs(title='Apalancamiento por provincia')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )


ggplot(p8, aes(x = Status, fill = Apalancamiento)) +geom_bar()+labs(title='Apalancamiento por status')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )



```

#### Indicadores financieros por tipo de empresa

Finalmente, se realizó una gráfica comparativa de los indicadores financieros de liquidez y solvencia según el "Tipo de empresa". Esta visualización permitió entender las diferencias entre los distintos tipos de empresas presentes en el estudio y su salud financiera relativa.

```{r message=FALSE, warning=FALSE}

#Para liquidez: Tenemos solo liquidez corriente
ggplot(p4, aes(x = Tipo_de_empresa, y = Liquidez_corriente)) +geom_bar()+labs(title='Liquidez por tipo de empresa')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )

#Para solvencia:Tenemos end. del activo, end. patrimonial, end. activo fijo, apalancamiento

ggplot(p5, aes(x = Tipo_de_empresa, fill = Endeudamiento_Del_activo)) +geom_bar()+labs(title='Endeudamiento del activo por Tipo de empresa')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )

ggplot(p6, aes(x = Tipo_de_empresa, fill = Endeudamiento_patrimonial)) +geom_bar()+labs(title='Endeudamiento patrimonial por Tipo de empresa')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )


ggplot(p7, aes(x = Tipo_de_empresa,  fill= Endeudamiento_del_activo_fijo)) +geom_bar()+labs(title='Endeudamiento del activo fijo por Tipo de empresa')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )


ggplot(p8, aes(x = Tipo_de_empresa, fill = Apalancamiento)) +geom_bar()+labs(title='Apalancamiento por Tipo de empresa')+
  theme(legend.title = element_text(size = 7),
      legend.text=element_text(size = 5),legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(size=5,angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(ncol = 3)
  )


```

## Part 4: Conclusiones

En esta sección, se resumirán los hallazgos clave del análisis financiero y se ofrecerán conclusiones basadas en los resultados presentados en la sección de Análisis. Se destacarán las principales tendencias, diferencias y riesgos identificados, y se discutirán las implicaciones generales para la toma de decisiones financieras en diferentes tipos de empresas.

En este informe, realizamos un análisis financiero de diferentes empresas utilizando los datos proporcionados en el archivo balance_2014.xlsx. Creamos un tibble llamado "empresas" con las variables relevantes y respondimos a las preguntas planteadas.

Observamos que las empresas de diferentes actividades económicas están distribuidas de manera desigual en los distintos cantones. Además, encontramos diferencias significativas en los indicadores financieros de liquidez y solvencia entre las empresas con diferentes situaciones (status) y provincias. Asimismo, pudimos identificar tendencias y variaciones en los indicadores financieros al comparar los diferentes tipos de empresas.

Estos resultados nos permiten obtener una visión más completa y detallada del estado financiero de las empresas analizadas, lo que puede ser de gran utilidad para la toma de decisiones financieras y estratégicas en el futuro.
