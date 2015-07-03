data1<-read_excel("data1_rls_uti.xlsx",sheet =1,col_names = TRUE, na="")
View(data1)
media1<-mean (data1[,1])
media2<-mean (data1[,2])
data2<-data.frame( )
for(i in 1:nrow(data1))
 {
   data2[i,1]<-data1[i,1]-media1
 }
  for(j in 1:nrow(data1))
     {
       data2[j,2]<-data1[j,2]-media2
     }
names(data2)<-names(data1)
regresion<-lm(Utilidad ~ Ventas,data2)
summary(regresion)
anova<-aov(regresion)
summary(anova)
#Calculo el fractil de 0.025 con 38 grados de libertad
qt(0.975,df=38 )
#INTERVALOS DE CONFIANZA
confint(regresion,level=0.95)
a<-mean(data2[,1])
b<-mean(data2[,2])
names(regresion)
res<-regresion[["residuals"]]
predicciones<-regresion[["fitted.values"]]
predicciones
data2<-data.frame(data2,predicciones,res)
View(data2)
mean(res)
data3<-data.frame(res)
View(data3)
windows()
par(mfrow=c(2,2))
hist(res,20)
qqnorm(res)
qqline(res,col="red")
plot(res,predicciones)
plot(data2[,1],data2[,2])
