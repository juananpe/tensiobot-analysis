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

length(na.omit(papel))
length(na.omit(papel))
library(car)
leveneTest(Analisis1$DifTAD , Analisis1$grupo)                                                                                                                                                                                     

boxplot (Analisis1$DifTAS ~ Analisis1$grupo)     
## Checklist
t.test(ChecklistCompleto$diferencia ~ ChecklistCompleto$grupo, var.equal = FALSE) 

t.test(ChecklistFiltrado$diferencia ~ ChecklistFiltrado$grupo, var.equal = TRUE) 

####    Mediante regresión para ajustar

summary(lm(diferencia ~ grupo + Edad + Situacion.laboral + sexo, data = ChecklistFiltrado))
summary(lm(diferencia ~ grupo + Situacion.laboral + grupo*Situacion.laboral, data = ChecklistFiltrado))


## bland altman function



baplotdata <- function(m1, m2,group, title) {
  ## new data frame sin NA's
  datosprov <- data.frame(m1,m2, group)
  ## discard NA's
  datosprov <- subset(datosprov, !is.na(datosprov[1]+datosprov[2]))
  
  # m11 and m12 are the measurements of the first group
  # m21 and m22 are the measurements of the second group
  
  # group 1
  m11 <- subset(datosprov[1], datosprov[3]== "Bot")
  m12 <- subset(datosprov[2], datosprov[3]== "Bot")
  diffs1 <-  as.numeric(unlist(m11- m12))
  means1 <- as.numeric(unlist((m11 + m12) / 2))
  mdiff1 <- mean(diffs1)
  sddiff1 <- sd(diffs1)
  # Compute the figure limits group 1
  ylimh1 <- mdiff1 + 3 * sddiff1
  yliml1 <- mdiff1 - 3 * sddiff1
  
  
  # grupo 2
  m21 <- subset(datosprov[1], datosprov[3]== "papel")
  m22 <- subset(datosprov[2], datosprov[3]== "papel")
  diffs2 <-  as.numeric(unlist(m21- m22))
  means2 <- as.numeric(unlist((m21 + m22) / 2))
  
  mdiff2 <- mean(diffs2)
  sddiff2 <- sd(diffs2)
  # print provisional results
  print (mdiff1)
  print (mdiff2)
  print (sddiff1)
  print (sddiff2)
  
  
  # Compute the figure limits grupo 2
  ylimh2 <- mdiff2 + 3 * sddiff2
  yliml2 <- mdiff2 - 3 * sddiff2
  # Plot data first group
  plot(diffs1 ~ means1, pch = 15, main= title , xlab = "Average values", 
       ylab = "Differences", ylim = c(yliml2, ylimh2))    
  # Center line first group
  abline(h = mdiff1) 
  # Standard deviations lines
  abline(h = mdiff1 + 1.96 * sddiff1, lty = 2)
  abline(h = mdiff1 - 1.96 * sddiff1, lty = 2)
  # Center line second group
  abline(h = mdiff2, col=2) 
  # Standard deviations lines
  abline(h = mdiff2 + 1.96 * sddiff2, col=2, lty = 2)
  abline(h = mdiff2 - 1.96 * sddiff2, col=2, lty = 2)        
  # Plot data second group 
  par(new=TRUE)
  plot(diffs2 ~ means2, col=2, pch=15,
       ylim = c(yliml2, ylimh2), axes = FALSE, xlab = "", ylab = "") 
  legend(x = "topright", legend = c("Bot","Paper"), fill = 1:2)
  
}

baplotdata(Analisis1$mediTADdiurna, Analisis1$Promedio.de.TAD,Analisis1$grupo,"Bland Altman plot, DBP (ABPM-HBPM) mmHg" )


#Carga el paquete 
library(BlandAltmanLeh)


## Bland Altman plot usando package BlandAltmanLeh

## plot con diferencia entre grupos
## datos ejemplo
A <- c(-0.358, 0.788, 1.23, -0.338, -0.789, -0.255, 0.645, 0.506, 
       0.774, -0.511, -0.517, -0.391, 0.681, -2.037, 2.019, -0.447, 
       0.122, -0.412, 1.273, -2.165)
B <- c(0.121, 1.322, 1.929, -0.339, -0.515, -0.029, 1.322, 0.951, 
       0.799, -0.306, -0.158, 0.144, 1.132, -0.675, 2.534, -0.398, 0.537, 
       0.173, 1.508, -1.955)
sex <- c( 1,1,1,1,2,2,2,1,1,1,2,2,2,2,2,1,1,2,1,2)


# caso de Tensiobot
#primero crear una variable numérica para el grupo
Analisis1$grupoN <- 1
Analisis1$grupoN[Analisis1$grupo == "Bot"] <- 2
Analisis1$grupoN

## plot BA

ba.stats <- bland.altman.stats(Analisis1$mediTASdiurna, Analisis1$Promedio.de.TAS)
plot(ba.stats$means, ba.stats$diffs, col=sex, 
     sub=paste("critical difference is", round(ba.stats$critical.diff,4)),
     main="make your own graph easily", ylim=c(-1.5,1.5), pch=18-sex)
abline(h = ba.stats$lines, lty=c(2,3,2), col=c("lightblue","blue","lightblue"), 
       lwd=c(3,2,3))
legend(x = "topright", legend = c("male","female"), fill = 1:2)
ba.stats <- bland.altman.stats(Analisis1$mediTASdiurna, Analisis1$Promedio.de.TAS)


ba.stats <- bland.altman.stats(Analisis1$mediTASdiurna, Analisis1$Promedio.de.TAS)
plot(ba.stats$means, ba.stats$diffs, col=Analisis1$grupoN+2, 
     xlab="Mean SBP both methods ",
     ylab="Difference SBP ABMP-HBPM",      main="Bland Altman plot SBP", ylim=c(-30,30), pch=18-Analisis1$grupoN+2)
abline(h = ba.stats$lines, lty=c(2,3,2), col=c("lightblue","blue","lightblue"), 
       lwd=c(3,2,3))
legend(x = "topright", legend = c("Paper","Bot"), fill = 3:4)

ba.stats <- bland.altman.stats(Analisis1$mediTADdiurna, Analisis1$Promedio.de.TAD)
plot(ba.stats$means, ba.stats$diffs, col=Analisis1$grupoN, 
     xlab="Mean DBP both methods ",
     ylab="Difference DBP ABMP-HBPM",      main="Bland Altman plot DBP", ylim=c(-30,30), pch=18-Analisis1$grupoN)
abline(h = ba.stats$lines, lty=c(2,3,2), col=c("lightblue","blue","lightblue"), 
       lwd=c(3,2,3))
legend(x = "topright", legend = c("Paper","Bot"), fill = 1:2)

plot(ba.stats$means, ba.stats$diffs, col=Analisis1$grupo,      
     ylab="Diferencia PAS MAPA-AMPA", 
     main="Diferencias en TAS diurna MAPA - AMPA", 
     abline(h = ba.stats$lines, lty=c(2,3,2), col=c("lightblue","blue","lightblue"), 
            lwd=c(3,2,3))
     legend(x = "topright", legend = c("Paper","Bot"), fill = 1:2)
     
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
     
     