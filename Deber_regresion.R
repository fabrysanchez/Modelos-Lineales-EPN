#Deber Fabricio Sánchez

dir()
library(readxl)
ls("package:readxl")
excel_sheets("data_rls_uti.xlsx")
data<-read_excel("data_rls_uti.xlsx",sheet =1,col_names = TRUE, na="")
str(data)
nrow(data)
regresion<-lm(Utilidad ~ Ventas,data)
summary(regresion)
anova<-aov(regresion)
summary(anova)
#Fractil de 0.025, g.l.
qt(0.975,df=38)
#intervalos de confianza
confint(regresion,level=0.95)
(a<-mean(data[,"Utilidad"]))
(b<-mean(data[,"Ventas"]))
names(regresion)
res1<-regresion[["residuals"]]
(predicciones<-regresion[["fitted.values"]])
data2<-data.frame(data,predicciones,res1)
hist(res1,20)
mean(res1)
data3<-data.frame(res1)
qqnorm(res1)
qqline(res1,col="red")
plot(res1,predicciones)
plot(data[,1],data[,2])

#Centralizados 
utilidad1<-data[,"Utilidad"]-a
ventas1<-data[,"Ventas"]-b
regresion2<-lm(utilidad1 ~ ventas1)
summary(regresion2)
anova1<-aov(regresion2)
summary(anova2)
confint(regresion2,level=0.95)
res2<-regresion2[["residuals"]]
(predicciones1<-regresion2[["fitted.values"]])
data4<-data.frame(utilidad1,ventas1,predicciones1,res2)
hist(res2,20)
mean(res2)
data5<-data.frame(res2)
qqnorm(res2)
qqline(res2,col="blue")
plot(res2,predicciones1)
plot(utilidad1,ventas1)

