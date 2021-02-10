attach(Analisis1)
#comparar los pacientes que han realizado AMPA con Bot o con papel
table (HTA_MAPA, HTA_AMPA)
round(prop.table (table (AMPA_HECHO , grupo),margin=2)*100, digits=1)
summary(table (AMPA_HECHO , grupo))

## Relación entre MAPA y AMPA.
summary(table (HTA_MAPA, HTA_AMPA))
library(irr)
kappa2(cbind (HTA_MAPA, HTA_AMPA))
medidasBot <- subset(Analisis1, Analisis1$grupo=="Bot")
medidasPapel <- subset(Analisis1, Analisis1$grupo=="papel")

kappa2(cbind (medidasBot$HTA_MAPA, medidasBot$HTA_AMPA))
kappa2(cbind (medidasPapel$HTA_MAPA, medidasPapel$HTA_AMPA))
