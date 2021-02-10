# comparación de grupos

## Presion arterial AMPA y MAPA 
####    medias
t.test(Analisis1$DifTAD ~ Analisis1$grupo, var.equal = TRUE)
####   Variaciones  
t.test(AMPA_MEDIAS$CVTAD ~ AMPA_MEDIAS$grupo, var.equal = TRUE)
t.test(AMPA_MEDIAS$Media.TAD ~ AMPA_MEDIAS$grupo, var.equal = TRUE)

#### para comparar las varianzas.
library(dplyr)
bot    <- Analisis1 %>% filter(grupo == "Bot") %>% pull(DifTAS)
papel    <- Analisis1 %>% filter(grupo == "papel") %>% pull(DifTAS)

length(na.omit(bot))
length(na.omit(papel))
library(car)
leveneTest(Analisis1$DifTAD , Analisis1$grupo)                                                                                                                                                                                     

library(ggplot2)
ggplot(Analisis1, aes(x=grupo, y=DifTAS)) + geom_boxplot() +
        xlab("Group") + ylab("Diurnal Systolic BP. Difference of means (mmHg)") + 
        scale_x_discrete(labels = c('Intervention/Bot','Control/Paper'))
# boxplot (Analisis1$DifTAS ~ Analisis1$grupo)     
## Checklist
t.test(ChecklistCompleto$diferencia ~ ChecklistCompleto$grupo, var.equal = FALSE) 

t.test(ChecklistFiltrado$diferencia ~ ChecklistFiltrado$grupo, var.equal = TRUE) 

####    Mediante regresión para ajustar

summary(lm(diferencia ~ grupo + Edad + Situacion.laboral + sexo, data = ChecklistFiltrado))
summary(lm(diferencia ~ grupo + Situacion.laboral + grupo*Situacion.laboral, data = ChecklistFiltrado))

## Bland Altman plot

#Carga el paquete 
library(BlandAltmanLeh)
#install.packages("BlandAltmanLeh")
# ejemplo


## plot simple
BA <- bland.altman.plot(Analisis1$mediTASdiurna, Analisis1$Promedio.de.TAS, graph.sys="ggplot2", conf.int=.95)
print (BA + xlab("Media PAS por dos métodos ") + ylab("Diferencia PAS MAPA-AMPA")  + ggtitle("Diferencias en TAS diurna MAPA - AMPA"))


	
## plot con diferencia entre grupos
## ejemplo
A <- c(-0.358, 0.788, 1.23, -0.338, -0.789, -0.255, 0.645, 0.506, 
0.774, -0.511, -0.517, -0.391, 0.681, -2.037, 2.019, -0.447, 
0.122, -0.412, 1.273, -2.165)
B <- c(0.121, 1.322, 1.929, -0.339, -0.515, -0.029, 1.322, 0.951, 
0.799, -0.306, -0.158, 0.144, 1.132, -0.675, 2.534, -0.398, 0.537, 
0.173, 1.508, -1.955)
sex <- c( 1,1,1,1,2,2,2,1,1,1,2,2,2,2,2,1,1,2,1,2)

ba.stats <- bland.altman.stats(Analisis1$mediTASdiurna, Analisis1$Promedio.de.TAS)

plot(ba.stats$means, ba.stats$diffs, col=sex, 
     sub=paste("critical difference is", round(ba.stats$critical.diff,4)),
     main="make your own graph easily", ylim=c(-1.5,1.5), pch=18-sex)
abline(h = ba.stats$lines, lty=c(2,3,2), col=c("lightblue","blue","lightblue"), 
       lwd=c(3,2,3))
legend(x = "topright", legend = c("male","female"), fill = 1:2)
ba.stats <- bland.altman.stats(Analisis1$mediTASdiurna, Analisis1$Promedio.de.TAS)

# caso de Tensiobot
ba.stats <- bland.altman.stats(Analisis1$mediTADdiurna, Analisis1$Promedio.de.TAD)
plot(ba.stats$means, ba.stats$diffs, col=Analisis1$grupo, 
     xlab="Media PAD por dos métodos ",
	ylab="Diferencia PAD MAPA-AMPA",      main="Diferencias en TAD diurna MAPA - AMPA", ylim=c(-30,30), pch=18)
abline(h = ba.stats$lines, lty=c(2,3,2), col=c("lightblue","blue","lightblue"), 
       lwd=c(3,2,3))
legend(x = "topright", legend = c("Bot","papel"), fill = 1:2)


plot(ba.stats$means, ba.stats$diffs, col=Analisis1$grupo,      
	ylab="Diferencia PAS MAPA-AMPA", 
     main="Diferencias en TAS diurna MAPA - AMPA", 
abline(h = ba.stats$lines, lty=c(2,3,2), col=c("lightblue","blue","lightblue"), 
       lwd=c(3,2,3))
legend(x = "topright", legend = c("Bot","papel"), fill = 1:2)

## Coeficiente de correlación intraclase
CoefCI$id.tensiobot <- factor(CoefCI$id.tensiobot)
anova(lm(CoefCI$TAD ~ CoefCI$id.tensiobot ))

CoefCIpapel <- subset(CoefCI, CoefCI$grupo=="papel")
CoefCIbot <- subset(CoefCI, CoefCI$grupo=="Bot")
summary(CoefCIpapel)
summary(CoefCIbot)
library(ICC)
ICCest(CoefCI$id.tensiobot, CoefCI$TAS, alpha = 0.05, CI.type = "THD")
ICCest(CoefCI$id.tensiobot, CoefCI$TAD, alpha = 0.05, CI.type = "THD")
ICCest(CoefCIpapel$id.tensiobot, CoefCIpapel$TAS, alpha = 0.05, CI.type = "THD")
ICCest(CoefCIpapel$id.tensiobot, CoefCIpapel$TAD, alpha = 0.05, CI.type = "THD")
ICCest(CoefCIbot$id.tensiobot, CoefCIbot$TAS, alpha = 0.05, CI.type = "THD")
ICCest(CoefCIbot$id.tensiobot, CoefCIbot$TAD, alpha = 0.05, CI.type = "THD")

