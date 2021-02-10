setwd("/opt/tensiobot/analisis/")

library("xlsx")
path <- "./bbddcompleta.xlsx"
pacientes <-read.xlsx(path, "pacientes", header=TRUE, colClasses=NA)
summary(pacientes)
CheckList <-read.xlsx(path, "CheckList", header=TRUE, colClasses=NA)
summary(CheckList)
MAPA <-read.xlsx(path, "MAPA", header=TRUE, colClasses=NA)
summary(MAPA)
AMPA <-read.xlsx(path, "AMPA", header=TRUE, colClasses=NA)
summary(AMPA)
Satisfaccion <-read.xlsx(path, "Satisfaccion", header=TRUE, colClasses=NA)
summary(Satisfaccion)
Farmacos <-read.xlsx(path, "Farmacos", header=TRUE, colClasses=NA)
summary(Farmacos)
AMPA_MEDIAS <-read.xlsx(path, "AMPA_medias MAPA", header=TRUE, colClasses=NA)
summary(AMPA_MEDIAS)
Analisis1 <- read.xlsx(path, "PacMAPA_AMPA", header=TRUE, colClasses=NA)
summary(Analisis1)
FarmacosPaciente <- read.xlsx(path, "FarmacosPaciente", header=TRUE, colClasses=NA)
summary(FarmacosPaciente)

#ChecklistCompleto
ChecklistCompleto <- read.xlsx(path, "ChecklistCompleto", header=TRUE, colClasses=NA)
summary(ChecklistCompleto)
CoefCI <- read.xlsx(path, "CoefCI", header=TRUE, colClasses=NA)
summary(CoefCI)
## Seleccionar casos con pretest <100

ChecklistFiltrado <- subset(ChecklistCompleto, pre <100)

summary(ChecklistFiltrado)

