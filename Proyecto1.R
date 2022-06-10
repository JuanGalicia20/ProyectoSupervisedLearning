
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggforce)
library(hrbrthemes)
library(schoolmath)
library(corrplot)
library(weights)
library(caret)
library(car)
library(GGally)
library(corpcor)
library(mctest)
library(e1071)
library(rpart)
library(rpart.plot)
library(Metrics)

setwd("C:/Users/juana/Documents/UVG/3 año/5to semestre/df/Proyecto1")

df<-read.csv("train.csv", stringsAsFactors=TRUE)

#_------------------------------------------------
factors_vec <- c('MSSubClass',
                 'OverallQual',
                 'OverallCond',
                 'MSSsubClass')

factors_df <- names(df2)[(names(df2) %in% factors_vec)]

for(i in factors_df){
  df2[,i] <- as.factor(df2[,i])
}

num_df <- dplyr::select_if(df, is.numeric)
num_df <- num_df[,-1]

discrete_vals <- c('YearBuilt',
                   'YearRemodAdd',
                   'BsmtFullBath',
                   'BsmtHalfBath',
                   'FullBath',
                   'HalfBath',
                   'Bedroom',
                   'Kitchen',
                   'TotRmsAbvGrd',
                   'Fireplaces',
                   'GarageYrBlt',
                   'GarageCars',
                   'MoSold',
                   'YrSold')

discrete_names <- names(df)[(names(df) %in% discrete_vals)]
discrete_df <- num_df[, discrete_names]

continuos_names <- names(num_df)[!(names(num_df) %in% discrete_vals)]
continuos_df <- num_df[, continuos_names]

#Relacion entre discretas y continuas
df_cor <- cor(continuos_df, use = 'complete.obs')
corrplot(df_cor, method = "number")

discrete_df <- mutate(discrete_df, SalePrice = (continuos_df[,'SalePrice']))
df_cor_discrete <- cor(discrete_df, use = 'complete.obs')
corrplot(df_cor_discrete, method = "number")

#Relacion entre Cualitativas
df2 = read.csv('train.csv')

factors_names <- names(df2)[!(names(df2) %in% names(num_df))]
factors_df <- df2[, factors_names]
factors_df <- factors_df[,-1]

Columnas = colnames(factors_df)
Tipos = sapply(factors_df, class)

nombres_DF = as.data.frame(cbind(Columnas, Tipos)) 

variables_para_dummificar = nombres_DF


for(i in 1:nrow(variables_para_dummificar)) {
  
  columna_ref = as.character(variables_para_dummificar[i, 1])
  temporal = select(df2, columna_ref)
  colnames(temporal) = "REF"
  temporal$REF = as.factor(temporal$REF)
  temporal_dummy = dummify(temporal$REF)
  nombres_dummy = colnames(temporal_dummy)
  nombres_dummy = paste(columna_ref, nombres_dummy, sep = "-")
  colnames(temporal_dummy) = nombres_dummy
  archivo = if(i == 1){ archivo = temporal_dummy} else {archivo = cbind(archivo, temporal_dummy)}
  
}

archivo = as.data.frame(archivo)
archivo <- mutate(archivo, SalePrice = (continuos_df[,'SalePrice']))


related_var <- c('MSZoning-C (all)', 'LotShape-IR2', 'LotShape-IR3', 'LandContour-Bnk', 'LotConfig-CulDSac', 'LotConfig-FR2',
                 'Neighborhood-Edwards', 'Neighborhood-Gilbert', 'Neighborhood-IDOTRR', 'Neighborhood-Mitchel', 'Neighborhood-NAmes',
                 'Neighborhood-NoRidge', 'Neighborhood-OldTown', 'Neighborhood-Sawyer', 'Neighborhood-StoneBr','Condition2-PosN',
                 'HouseStyle-1.5Fin', 'HouseStyle-2.5Fin','HouseStyle-2.5Unf','HouseStyle-2Story', 'OverallQual-1', 'OverallQual-2',
                 'OverallQual-3','OverallQual-4', 'OverallQual-5', 'OverallQual-6','OverallQual-7','OverallQual-8','OverallQual-9',
                 'OverallCond-3','OverallCond-4','OverallCond-5','OverallCond-6','RoofMatl-ClyTile', 'RoofMatl-CompShg',
                 'RoofMatl-Membran','RoofMatl-Metal','RoofMatl-Roll','RoofMatl-Tar&Grv', 'RoofMatl-WdShake','Exterior1st-BrkFace',
                 'Exterior1st-CemntBd', 'BsmtFinType1-ALQ', 'BsmtFinType1-BLQ','BsmtFinType1-GLQ','BsmtFinType1-Rec','HeatingQC-Ex',
                 'KitchenQual-Ex','KitchenQual-Gd', 'Functional-Maj1','Functional-Maj2','FireplaceQu-Gd','FireplaceQu-TA',
                 'GarageType-Attchd','GarageType-BuiltIn','GarageType-Detchd','PoolQC-Ex', 'PoolQC-Gd','SaleType-ConLD',
                 'SaleType-New','SaleCondition-Alloca')

related_num_var <- c('YearBuilt','YearRemodAdd', 'FullBath', 'TotRmsAbvGrd', 'GarageCars'
                     , 'MasVnrAreal', 'TotalBsmtSF', 'X1stFlrSF', 'GrLivArea', 'GarageArea')


related_names <- names(archivo)[(names(archivo) %in% related_var)]
related_names_num <- names(num_df)[(names(num_df) %in% related_num_var)]
related_df <- cbind(archivo[, related_names], num_df[, related_names_num])
related_df <- mutate(related_df, SalePrice = (continuos_df[,'SalePrice']))


related_var_2 <- c('MSZoning-C (all)', 'LotShape-IR2', 'LotShape-IR3', 'LandContour-Bnk', 'LotConfig-CulDSac', 'LotConfig-FR2',
                   'Neighborhood-Edwards', 'Neighborhood-Gilbert', 'Neighborhood-IDOTRR', 'Neighborhood-Mitchel', 'Neighborhood-NAmes',
                   'Neighborhood-NoRidge', 'Neighborhood-OldTown', 'Neighborhood-Sawyer', 'Neighborhood-StoneBr','Condition2-PosN',
                   'HouseStyle-1.5Fin', 'HouseStyle-2.5Fin','HouseStyle-2.5Unf','HouseStyle-2Story', 'OverallQual-1', 'OverallQual-2',
                   'OverallQual-3','OverallQual-4','OverallQual-8','OverallQual-9',
                   'OverallCond-3','OverallCond-4','OverallCond-5','OverallCond-6','RoofMatl-ClyTile', 'RoofMatl-CompShg',
                   'RoofMatl-Membran','RoofMatl-Metal','RoofMatl-Roll','RoofMatl-Tar&Grv', 'RoofMatl-WdShake','Exterior1st-BrkFace',
                   'Exterior1st-CemntBd', 'BsmtFinType1-ALQ', 'BsmtFinType1-BLQ','BsmtFinType1-GLQ','BsmtFinType1-Rec','HeatingQC-Ex',
                   'KitchenQual-Ex','KitchenQual-Gd', 'Functional-Maj1','Functional-Maj2','FireplaceQu-Gd','FireplaceQu-TA',
                   'GarageType-Attchd','GarageType-BuiltIn','GarageType-Detchd','PoolQC-Ex', 'PoolQC-Gd','SaleType-ConLD',
                   'SaleType-New','SaleCondition-Alloca')

related_num_var_2 <- c('YearBuilt','YearRemodAdd', 'FullBath', 'TotRmsAbvGrd', 'GarageCars'
                       , 'MasVnrAreal', 'TotalBsmtSF', 'GrLivArea', 'GarageArea')


related_names2 <- names(archivo)[(names(archivo) %in% related_var_2)]
related_names_num2 <- names(num_df)[(names(num_df) %in% related_num_var_2)]
related_df2 <- cbind(archivo[, related_names2], num_df[, related_names_num2])
related_df2 <- mutate(related_df2, SalePrice = (continuos_df[,'SalePrice']))

num_data <- dplyr::select_if(df, is.numeric)
num_data <- num_data[,-1]
continuos_names <- names(num_data)[!(names(num_data) %in% discrete_vals)]
continuos_data <- num_data[, continuos_names]

related_var_2 <- c('MSZoning-C (all)', 'LotShape-IR2', 'LotShape-IR3', 'LotConfig-CulDSac',
                   'Neighborhood-Edwards', 'Neighborhood-Mitchel', 'Neighborhood-NAmes',
                   'Neighborhood-NoRidge', 'Neighborhood-OldTown', 'Neighborhood-Sawyer', 'Neighborhood-StoneBr','Condition2-PosN',
                   'HouseStyle-2.5Fin','OverallQual-8','OverallQual-9',
                   'OverallCond-3','OverallCond-4','OverallCond-5','OverallCond-6','RoofMatl-ClyTile', 'RoofMatl-CompShg',
                   'RoofMatl-Roll','RoofMatl-Tar&Grv', 'RoofMatl-WdShake','Exterior1st-BrkFace',
                   'BsmtFinType1-ALQ', 'BsmtFinType1-BLQ','BsmtFinType1-GLQ','BsmtFinType1-Rec','HeatingQC-Ex',
                   'KitchenQual-Ex', 'Functional-Maj1','FireplaceQu-Gd',
                   'PoolQC-Ex', 'PoolQC-Gd','SaleType-New')

related_num_var_2 <- c('YearBuilt','YearRemodAdd', 'TotalBsmtSF', 'GrLivArea', 'GarageArea')

related_names2 <- names(archivo)[(names(archivo) %in% related_var_2)]
related_names_num2 <- names(num_data)[(names(num_data) %in% related_num_var_2)]
related_data2 <- cbind(archivo[, related_names2], num_data[, related_names_num2])
related_data2 <- mutate(related_data2, SalePrice = (continuos_data[,'SalePrice']))

#------------------------------------------------------------------------

df<-df%>%
  mutate(rango = ifelse(SalePrice <=100000, "< 100,000", ifelse(SalePrice <= 200000, "< 200,000", ifelse(SalePrice <= 300000, "< 300,000", ifelse(SalePrice <= 400000, "< 400,000", ifelse(SalePrice <= 500000, "< 500,000", ifelse(SalePrice <= 600000, "< 600,000", ifelse(SalePrice <= 700000, "< 700,000", ifelse(SalePrice <= 800000, "< 800,000", "Muy cara")))))))))


table(df$rango, df$SaleCondition)
table(df$rango, df$HouseStyle)
table(df$rango, df$SaleType)
table(df$rango, df$ExterCond)
table(df$rango, df$Foundation)

#A
df%>%
  group_by(HouseStyle)%>%
  ggplot(aes(x=HouseStyle, y=SalePrice))+
  geom_col()+
  ggtitle("Media de precio de casas por tipo")+
  theme_economist()

#B
df%>%
  group_by(BedroomAbvGr)%>%
  ggplot(aes(x=BedroomAbvGr, y=SalePrice))+
  geom_col()+
  ggtitle("Número de cuartos visualizado por el precio de venta")+
  theme_economist()

#C
df%>%
  group_by(Neighborhood)%>%
  ggplot(aes(x=Neighborhood, y=SalePrice))+
  geom_col()+
  coord_flip()+
  theme_economist()+
  ggtitle("Precio de venta por ubicación")

#D
df%>%
  ggplot(aes(x=Neighborhood, y=SalePrice))+
  geom_boxplot()+
  coord_flip()+
  theme_economist()+
  ggtitle("Distribución de los precios de casas por ubicación")


#E
df%>%
  ggplot(aes(x=Neighborhood, y=LotArea))+
  geom_boxplot()+
  coord_flip()+
  theme_economist()+
  ggtitle("Distribución de cantidad casas por\n Tamaño de la propiedad")


#F
df%>%
  ggplot(aes(x=SalePrice))+
  facet_wrap(~HouseStyle)+
  geom_histogram(bins = 20)+
  theme_economist()+
  ggtitle("Distribución de precios mediante\n el numero de niveles de la casa")+
  theme(panel.spacing = unit(2, "lines"))



#G
df%>%
  ggplot(aes(x=YearBuilt, y=SalePrice))+
  geom_point()+
  geom_smooth(method="lm", formula = y~x, se= F)+
  theme_economist()+
  ggtitle("Efecto del año de construccion\n sobre el precio de la casa")



#H
df%>%
  ggplot(aes(x=YearBuilt, y=OverallCond))+
  geom_point()+
  geom_smooth(method="lm", formula = y~x, se= F)+
  theme_economist()+
  ggtitle("Efecto del año de construcción\n sobre su OverallCondition")


  


#modelo regression 1
modelo1<-lm(SalePrice ~ .-SalePrice, data=related_data2)
modelo1
summary(modelo1)

related_data2$prediccion<-predict(modelo1, related_data2)
ggplot(related_data2, aes(x=prediccion, y=SalePrice))+
  geom_point()+
  geom_abline(color="red")

related_data2$residuals<-related_data2$prediccion-related_data2$SalePrice
res1=residuals(related_data2)
rms1<-sqrt(mean(res1^2))
rms1
sd_df1 <- sd(related_data2$SalePrice)
sd_df1

ifelse(rms1<sd_df1, "Muy buen modelo", "Podria mejorar")



test<-read.csv("test.csv", stringsAsFactors=TRUE)
test$prediccion<-predict(modelo1, test)
ggplot(test, aes(x=prediccion, y=SalePrice))+
  geom_point()+
  geom_abline(color="red")

related_data2$residuals<-related_data2$prediccion-related_data2$SalePrice
res1=residuals(related_data2)
rms1<-sqrt(mean(res1^2))
rms1
sd_df1 <- sd(related_data2$SalePrice)
sd_df1

ifelse(rms1<sd_df1, "Muy buen modelo", "Podria mejorar")












