library(paqueteMET)   # activar paqueteMET
library(dplyr)
data("CarreraLuz22")  # cargar la dataset

data = CarreraLuz22

glimpse(data)

table(data$sex)

library(tidyverse)

table(data$categoria) %>% prop.table()*100 

ptable <- table(data$categoria) %>% 
  prop.table()*100 

ptable %>%
  round(2)

summarytools::freq(data$categoria, cumul = T)

unique(data$origen)
summarytools::freq(data$origen, cumul = T)

class(data)
rNms <- c("Bogota",
          "Bogotá",               
          "Bogota D.c",
          "Cali",
          "Cali Valle",
          "Florida",
          "Florida Va",
          "Jamundi",
          "Jamundí",
          "Popayan",
          "Popayán",
          "Tulua",
          "Tuluá")
pNms <- c("Bogotá",
          "Bogotá",               
          "Bogotá",
          "Cali",
          "Cali",
          "Florida",
          "Florida",
          "Jamundí",
          "Jamundí",
          "Popayán",
          "Popayán",
          "Tuluá",
          "Tuluá")
pData <- data


for (i in 1:length(rNms)) {
  pData["origen"][pData["origen"]==rNms[i]] <- pNms[i]
}

for (i in unique(pData$origen)) {
  nCat <- length(pData["origen"][pData["origen"]==i])
  if (nCat<5) {pData["origen"][pData["origen"]==i]<-"Otros"}
}
unique(pData$origen)
table(pData$origen)

summarytools::freq(pData$nacionalidad, cumul = T)

pData["time.H"] = pData["timerun"]/(60*60)
pData["vel.kmh"] = 10/pData["time.H"]

hist(pData$vel.kmh)

plot(density(pData$vel.kmh))

ggplot(pData, aes(vel.kmh)) +
  geom_histogram(aes(y=..density..))+
  facet_grid(vars(sex),vars(categoria))
