
###################
# Asier Diaz Fernandez
# TFM (NFT's Nftshowroom)
#################

#------Cargamos librerias

library(data.table)
library(inspectdf)
library(dplyr)
library(ranger)
library(tidyverse)
library(caret)
library(devtools)
library(ggmosaic)
library(corrplot)
library(rpart)
library(ggfortify)
library(rattle)
library(lubridate)
library(plyr)
library(DataExplorer)
library(patchwork)
library(stringr)
library(forcats)
library(fastDummies)
library(stringdist)
library(recipes)


#Cargamos archivos 

setwd("C:/Users/asier/OneDrive/Documentos/Master/Modulos/M�dulo 16 Trabajo fin de master/Datasets/NFTs/dataset")


set.seed(42)
datTotal <- as.data.frame(fread("dataset.csv", nThread = 4 ))

#Determinacion de las variables numericas
datTotal$price <- as.numeric(as.character(datTotal$price))
datTotal$likes <- as.numeric(as.character(datTotal$likes))
datTotal$tokens <- as.numeric(as.character(datTotal$tokens))
datTotal$V16 <- NULL

#Depuracion de datos
names(datTotal)

#Dimension de nuestro DataSet
dim(datTotal) 

#Consulta de la estructura del df
str(datTotal) 

#Verificacion de variables y tipos de valor de los datos
glimpse(datTotal) 


#Comprobacion de las variables con valores missing

sapply(datTotal, function(x) sum(is.na(x)))

datTotal <- na.omit(datTotal)

#Comprobacion de si hay filas duplicadas en el dataset y las eliminamos

duplicated(select(datTotal, -cid)) %>% sum()

datTotal_Final <- datTotal[!duplicated(select(datTotal, -cid)),] 

# Checking del nuevo set de datos
duplicated(select(datTotal_Final, -cid)) %>% sum()

dim(datTotal_Final)


#Exploracion de datos

# Exploracion de variables numericas y categoricas

# Tabla de frecuencias absolutas y relativas de la variable respuesta
table(datTotal_Final$price)
prop.table(table(datTotal_Final$price)) %>% round(digits = 2)

#Varianza 0

datTotal_Final %>% nearZeroVar(saveMetrics = TRUE)

#Elminacion de las variables con una varianza proxima a 0

datTotal_Final$symbol <- NULL
datTotal_Final$royalty <- NULL

#Analisis en particular a las variables price y nsfw
quantile(datTotal_Final$price, c(c(0.05), 1:9/10 , c(0.95, 0.99, 0.999)))
max(datTotal_Final$price)

#En el percentil 95 el precio es de 900. Sin embargo, el maximo es de 
#2744850. Este es un outlier (junto con otros que puede que haya) que ensucian
#los datos y no nos permiten ver bien las graficas, tambien pueden distorsionar
#los resultados de una regresion lineal, etc.

#Cuantos valores hay por encima de 10000 (p 99)
datTotal_Final %>% dplyr::filter(price > 10000) %>% nrow(.)

#Cuantos por encima de 900 (p 95)
datTotal_Final %>% dplyr::filter(price > 900) %>% nrow(.)

datTotal_Final2 <- datTotal_Final %>% dplyr::filter(price <= 900)

#Como se puede observar,la mayoria de los NFTs tienen un precio inferior a 200. Precisamente, el 80%
#de los NFTs tienen un precio inferior a 200.
plot(density(datTotal_Final2$price))
hist(datTotal_Final2$price)

#Aunque NSFW tiene muy pocos valores = True, se procede al analisis 
#en busqueda de algun patron (la hipotesis es que los NSFW son mas caros)
#No obstante, el boxplot muestra un solapamiento entre las cajas, indicando que
#no hay una diferencia significativa entre los precios cuando el NTF es NSFW y cuando no.

prop.table(table(datTotal_Final2$nsfw)) %>% round(digits = 2)
boxplot(price~nsfw,data=datTotal_Final2)

#Se elimina la variable NSFW
datTotal_Final2$nsfw <- NULL


#Funcion para obtener el numero de niveles tienen las variables categoricas

for (i in 1:ncol(datTotal_Final2)) {
  
  tmp_clas <- class(datTotal_Final2[,i])
  tmp_lg <- length(unique(datTotal_Final2[,i]))
  
  if (tmp_clas == 'character') {
    print( c(names(datTotal_Final2)[i], tmp_lg))
  } 
}

#Estos conteos de categorias revelan que title, name, art_series, cid y path
#no son aprovechables directamente. en especial cid, dado que es un identificador unico.
#title, name y art series quizas s� si se intentan obtener tematicas a partir del texto.


#Seran los NTFs mas caros en funcion del tipo de formato?
boxplot(price~type,data=datTotal_Final2)

#De nuevo, no hay grandes diferencias. En todo caso, para video si que 
#se ve que que la caja esta desplazada hacia arriba, indicando que quizas los videos
#son un poco mas caros
ggplot(datTotal_Final2, aes(x=price, fill=type)) + geom_density(alpha=.3)
prop.table(table(datTotal_Final2$type)) %>% round(digits = 2)


#La variable likes seguramente sea mas interesante categorizarla en si tienen
# 0 likes, 1, 2, o mas de 2.
prop.table(table(datTotal_Final2$likes)) %>% round(digits = 2)

boxplot(price~likes,data=datTotal_Final2)

datTotal_Final2$likes <- dplyr::case_when(datTotal_Final2$likes > 2 ~ 3,
                                          TRUE ~ datTotal_Final2$likes
                                         )

#Year pasa a ser un factor.

datTotal_Final2$year <- as.numeric(datTotal_Final2$year)

datTotal_Final2$year <- dplyr::case_when(datTotal_Final2$year %in% 1990:2021 ~ datTotal_Final2$year,
                                         TRUE ~ -1)

datTotal_Final2$year <- as.factor(datTotal_Final2$year)

# Rights, tiene solo los valores 1 y 3, así que tambien
datTotal_Final2$rights <- as.factor(datTotal_Final2$rights)

# Tokens, lo mismo que likes (> 10 -> 11)
datTotal_Final2$tokens <- dplyr::case_when(datTotal_Final2$tokens > 10 ~ 11,
                                          TRUE ~ datTotal_Final2$tokens
)

# En este escenario, no tendriamos ninguna variable numerica dado que estaran 
# categorizadas.

datTotal_Final2$type <- as.factor(datTotal_Final2$type)


#Indicamos Variables objetivo y las apartamos de nuestro analisis
#Las variables predictoras a elegir seran las siguientes: type, likes, tokens, year y rights

#Seria interesante hacer una prueba con creators, para comprobar de alguna manera si el precio depende mas
#del creador (cada creador tiene un sesgo a poner un precio) que del resto de variables

y <- log(datTotal_Final2$price)
input <- datTotal_Final2 %>% dplyr::select(type, likes, tokens, year, rights)
summary(input)


# Cargo las librerias que me van a hacer falta
library(questionr)
library(corrplot)
library(caret)
library(ggplot2)
library(lmSupport)


#Obtengo la particion
set.seed(123456)
trainIndex <- createDataPartition(y, p=0.8, list=FALSE)
x_train <- input[trainIndex,]
x_test <- input[-trainIndex,]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]


#Construyo un modelo preliminar con todas las variables
modelo1 <- lm(y_train ~ year+tokens+likes+type+rights, data=data_train)
summary(modelo1)

# modelEffectSizes(modelo1)
barplot(sort(modelEffectSizes(modelo1)$Effects[-1,4],decreasing =T),las=2,main="Importancia de las variables (R2)")

#Checkeando las asunciones de la regresion lineal:
#Que los residuos no tengan ningun patron (residuos vs valores reales). Lo ideal seria que la linea roja fuese horizontal 
# centrada en el 0.
plot(modelo1, 1)

#Asuncion de que los residuos tienen una distribucion normal.
#En este QQ plot, los residuos tienen una distribucion normal o casi normal si los puntos estan cerca de la linea discontinua.
plot(modelo1, 2)

#Asuncion de homogeneidad de varianza de los residuos. Se deberia ver como los puntos estan distribuidos de manera homogenea,
#con una dispersion homogenea a lo largo del plot. Ademas, la linea roja deberia ser horizontal, indicando la tendencia de los residuos
#comparado con los valores reales (fitted values).
plot(modelo1, 3)

#Deteccion de puntos palanca. Estos puntos son outliers que hacen que nuestro hiperplano fiteado en la regresion se desvie mucho.
# Los 3 mas outliers estan marcados con su indice en el grafico. Sin embargo, puede haber mas.
plot(modelo1, 5)

# Selecting the predictor variables
input <- datTotal_Final2 %>% dplyr::select(type, likes, tokens, year, rights)

# First, a model with all the types

# Checking correlations in continuous variables (likes, tokens and price)
cor(data.frame(input$likes, input$tokens, y))



# Splitting into train (80%), val (10%) and test (10%)
trainIndex <- createDataPartition(y, p=0.8, list=FALSE)
x_train <- input[trainIndex,]
x_test <- input[-trainIndex,]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

valIndex <- createDataPartition(y_test, p=0.5, list=FALSE)
x_val <- x_test[valIndex,]
x_test <- x_test[-valIndex,]
y_val <- y_test[valIndex]
y_test <- y_test[-valIndex]

x_and_y <- datTotal_Final2 %>% dplyr::select(type, likes, tokens, year, rights, price)
x_and_y$price <- log(x_and_y$price)
x_and_y_train <- x_and_y[trainIndex,]

# Después de ver el árbol de decisión más abajo, se intentan varias interacciones
model <- lm(price ~ year + tokens + likes + type + rights + year:tokens + year:likes + tokens:likes, data=x_and_y_train)
summary(model)

x_and_y_train$pred <- predict(model, newdata=x_and_y_train)
x_and_y_train$diff <- x_and_y_train$price - x_and_y_train$pred
x_and_y_train$abs_diff <- abs(x_and_y_train$price - x_and_y_train$pred)
x_and_y_train$diff_rel <- abs(x_and_y_train$price - x_and_y_train$pred) * 100 / x_and_y_train$price
summary(x_and_y_train %>% dplyr::select(diff, abs_diff, diff_rel))

# Checking multicollinearity (lt 1 -> no multicolinearity)
# library(car)
# car::vif(model)


# Quick and dirty decision tree to spot possible groups
install.packages("party")
library(party)

x_and_y <- datTotal_Final2 %>% dplyr::select(type, likes, tokens, year, rights, price)
x_and_y$price <- log(x_and_y$price)
x_and_y_train <- x_and_y[trainIndex,]
tree <- ctree(price ~ year + tokens + type + rights + likes, x_and_y_train)
plot(tree)

x_and_y_train$pred <- predict(tree, newdata=x_and_y_train)
x_and_y_train$diff <- x_and_y_train$price - x_and_y_train$pred
x_and_y_train$abs_diff <- abs(x_and_y_train$price - x_and_y_train$pred)
x_and_y_train$diff_rel <- abs(x_and_y_train$price - x_and_y_train$pred) * 100 / x_and_y_train$price
summary(x_and_y_train %>% dplyr::select(diff, abs_diff, diff_rel))

# Me pregunto por cuál variable hacer varios modelos. El tipo aparece en el árbol,
# pero la más discriminatoria son los años. Sin embargo, hacer un modelo por año no
# es muy práctico (muchos modelos). Se me ocurre agrupar los años, aunque el criterio a usar
# no lo tengo muy claro (hay años con muchas observaciones, otros con pocas, otros con mediana similar 
# pero dispersión distinta...). Opto por utilizar lmtrees, que son árboles que en sus nodos ajustan modelos 
# de regresión lineal.

# https://rdrr.io/cran/partykit/man/lmtree.html
# install.packages("partykit") #This is very slow...
# library(partykit)
# 
# lm_tree <- lmtree(price ~ type + likes + tokens + year + rights,
#                   data = x_and_y_train, minsize = nrow(x_and_y_train) * 0.01,
#                   cores=1
#                   )

# https://www.r-bloggers.com/2015/03/ensemble-learning-with-cubist-model/
# https://www.dropbox.com/s/2vf3swfbk48lfdc/RulesRulesRules.pdf?dl=0
# https://www.rulequest.com/cubist-win.html
install.packages("Cubist")
library(Cubist)

modelRules <- cubist(x = x_train, y = y_train, committees=1)
summary(modelRules)

# El modelo de regresión tal cual tiene un error medio absoluto de 0.89, esto lo baja a 0.82,
# habría que ver la dispersión del error que también es interesante, pero no nos arregla mucho.
# El del árbol de decisión simple da un error medio absoluto de 0.83.

# Ideas: partir los precios en categorías (bajo, medio, alto, extremo) y predecir estas categorías?
# Además la interpretabilidad sería mejor, porque nos quitamos de en medio el logaritmo sobre price


# Partiendo los precios en categorías
# prop.table(table(x_and_y$price_chunk)) %>% round(digits = 2)

plot(density(datTotal_Final2$price))
plot(density(datTotal_Final2$price[datTotal_Final2$price < 300]))
plot(density(datTotal_Final2$price[datTotal_Final2$price < 100]))

# 0 - 50
# 50 - 100
# 100 - 300
# 300 - inf

x_and_y <- datTotal_Final2 %>% dplyr::select(type, likes, tokens, year, rights, price)

x_and_y$price <- x_and_y$price
# x_and_y$price_chunk <- dplyr::if_else(x_and_y$price <= 50, 0, 
#                                       dplyr::if_else(x_and_y$price <= 100, 1,
#                                                      dplyr::if_else(x_and_y$price <= 300, 2,
#                                                                     3
#                                                      )
#                                       )
# )
# x_and_y$price_chunk <- dplyr::if_else(x_and_y$price <= 100, 0,
#                                       dplyr::if_else(x_and_y$price <= 170, 1, 2
#                                       )
# )
x_and_y$price_chunk <- dplyr::if_else(x_and_y$price <= 29, 0,
                                      dplyr::if_else(x_and_y$price <= 50, 1,
                                                     dplyr::if_else(x_and_y$price <= 125, 2,
                                                                    3
                                                     )
                                      )
)

prop.table(table(x_and_y$price_chunk)) %>% round(digits = 2)

x_and_y$price_chunk <- as.factor(x_and_y$price_chunk)
x_and_y_train <- x_and_y[trainIndex,]

require(nnet)
mn_model <- multinom(price_chunk ~ type + likes + tokens + year + rights, 
                     data=x_and_y_train)
summary(mn_model)

# https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
x_and_y_train$pred_prob <- predict(mn_model, newdata = x_and_y_train, "probs")
x_and_y_train$pred <- predict(mn_model, newdata = x_and_y_train)

x_and_y_train %>%dplyr::select(price_chunk, pred_prob, pred)

confusionMatrix(x_and_y_train$price_chunk, x_and_y_train$pred)

# Probamos un random forest
require(randomForest)

rf <- randomForest(price_chunk ~ type + likes + tokens + year + rights, 
                   data=x_and_y_train, ntree=100)
x_and_y_train$pred_prob <- predict(rf, newdata = x_and_y_train, "prob")
x_and_y_train$pred <- predict(rf, newdata = x_and_y_train)

# x_and_y_train %>%dplyr::select(price_chunk, pred_prob, pred)

confusionMatrix(x_and_y_train$price_chunk, x_and_y_train$pred)

# Decision Tree

tree <- ctree(price_chunk ~ year + tokens + type + rights + likes, x_and_y_train)
# plot(tree)

x_and_y_train$pred_prob <- predict(rf, newdata = x_and_y_train, "prob")
x_and_y_train$pred <- predict(rf, newdata = x_and_y_train)

confusionMatrix(x_and_y_train$price_chunk, x_and_y_train$pred)

# Conclusión: Por capacidad explicativa y accuracy, es mejor predecir los precios
# partidos en categorías (según están partidos, las 4 clases están balanceadas)
# y sin transformación de log(price) porque dificultaría la explicabilidad del modelo.

# En general, todos los modelos intentados en TFM.R y aquí se pueden desarrollar
# como que primero se intentó un modelo de regresión lineal típico (con los checkeos
# de outliers, residuos con distribución normal, etc...), se siguió mejorando
# el modelo lineal con interacciones ayudándonos de un árbol de decisión,
# y luego se intentó a categorizar los precios y probar un modelo de regresión multinomial, un random forest y 
# un árbol de decisión. El random forest y el árbol de decisión son los 2 que mejor 
# resultados dan en train (habría que ver en val y test) y por capacidad de explicativa se elige el decision tree.


