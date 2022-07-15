###Autore: Denis Giusti
###Matricola: 157179

# ANALISI DI PROPRIETÀ IN AFFITTO

########################
# PACCHETTI E LIBRERIE #
########################

## Pacchetti
install.packages("tidyverse")
install.packages("modelr")
install.packages("graphics")
library(tidyverse)
library(modelr)
library(graphics)

## Link libreria
https://www.kaggle.com/datasets/karthikbhandary2/property-rentals

## Libreria
library(readr)
df <- read_csv("~/Desktop/property_rentals.csv", 
               col_types = cols(bathrooms = col_number(), 
                                bedrooms = col_number(), minimum_nights = col_number(), 
                                price = col_number()))


#######################
# INIZIO COSE SENSATE #
#######################

## Primo sguardo
View(df)
summary(df)
problems(df) #Verifico eventuali problemi

newdf <- na.omit(df) #Nuovo dataset tolte le mancanze
View(newdf)
summary(newdf)

# Inizio dell'analisi

## Primi grafici, valutazione primarie ad occhio di prezzi e minimo di notti
###Istogramma prezzo con evidenziata media dei prezzi
hist(newdf$price, breaks = 100, main = "Prezzo", xlab = "prezzo", ylab = "frequenza")
abline(v = mean(newdf$price), col = "red", lwd = 1)
###Istogramma notti minime con evidenziata media notti minime
hist(newdf$minimum_nights, breaks = 100, main = "Notti minime", xlab = "notti", ylab = "frequenza")
abline(v = mean(newdf$minimum_nights), col = "red", lwd = 1)

## Valutazione prezzi e minimo di notti più dettagliata

###Prezzo minore $29, prezzo maggiore $9999, visto con "summary"
median(newdf$price) # Mediana == $155
sort(table(newdf$price)) # Moda == $150
mean(newdf$price) # Media $228.4914

#Risulta quindi
Mediana | Moda | Media
--- | --- | ---
$155 | $150 | $228.5

summary(newdf)
###Minore numero di notti minime 1 notte, maggiore numero di notti minime1125, visto con "summary"
median(newdf$minimum_nights) #### Mediana == 4 notti
sort(table(newdf$minimum_nights)) #### Moda == 30 notti
mean(newdf$minimum_nights) ### Media == 16.03665 notti

#Risulta quindi:
Mediana | Moda | Media
--- | --- | ---
4 notti | 30 notti | 16,04 notti


## Posizione media degli immobili
###Non essendoci stato indicato il luogo dove sono presenti gli immobili procediamo a determinarlo.
###Ci vengono fornite le cordinate di latitudine e longitudine, essendo tutte molto simili,
###procedo con la media delle cordinate per determinare a grandi linee la locazione degli immobili.

mean(newdf$latitude) # Media == 37.76533
mean(newdf$longitude) # Media == -122.4313

###Media della longitudine = 37.76533
###Media della latitudine = -122.4313

#Cerco il punto sulla mappa
library(leaflet)
leaflet() %>%
  setView(-122.4312, 37.76543, zoom = 9) %>% 
  addTiles() %>%
  addMarkers(-122.4312, 37.76543, popup = "Maungawhau")

###L'immagine risultante ci mostra che ci troviamo nella città di San Francisco, più precisamente
###in California negli Stati Uniti.

## Tipologie di immobili presenti
sort(table(newdf$property_type))

###Risultano 15 tipologie di immobili con i seguenti numeri:
  
Immobile | Quantità
--- | ---
Other | 1
Cottage | 5
Bungalow | 6
Boutique hotel | 10
Guesthouse | 12
Hostel | 16
Bed and breakfast | 20
Serviced apartment |20
Hotel | 41
Townhouse | 47
Loft | 50
Guest suite | 177
Condominium | 255
House | 637
Apartment | 913

## Prezzo medio per tipologia di immobile
prezzo_medio_immobile <- newdf %>% group_by(property_type) %>% summarise(price = mean(price))
View(prezzo_medio_immobile)

## Grafico prezzo medio per tipologia di immobile
Prezzi <- c(64, 73, 135, 139, 153, 164,183, 198, 208, 210, 248, 261, 265, 290, 331)
Immobili <- c("Bed and breakfast", "Hostel", "Other", "Guesthouse", "Guest suite", "Cottage", "Hotel", "Bungalow", "Apartment", "Boutique hotel", "Townhouse", "House", "Loft", "Condominium", "Serviced apartment")
grafico <- data.frame(Immobili, Prezzi)
plot(grafico$Prezzi, type="l", lty="solid", col="red", axes=FALSE, ann=FALSE)
title(main="Prezzo medio per immobile", col.main="blue", font.main="4", ylab="Prezzi in dollari")
box()
axis(1, at=1:15, lab=grafico$Immobili, las = 2)
axis(2, las=1, at=seq(min(Prezzi), max(Prezzi), 20))

## Valori medi di prezzo, camere e bagni immobili
###Valore medio prezzo, camere e bagni. Ordino per prezzo crescente.
dati_medi <- arrange(newdf %>% group_by(property_type) %>%summarise_at(c("price", "bedrooms", "bathrooms"), mean), price)

#I dati ottenuti risultano:
  
Immobile | Prezzo | Camere | Bagni
--- | --- | --- | ---
Bed and breakfast | $64.10 | 1 | 6.6
Hostel | $72.56 | 1 | 2.2
Other | $135.00 | 0 | 1
Guesthouse | $139.17 | 1 | 1
Guest suite | $152.82 | 1 | 1
Cottage | $163.80 | 1 | 1
Hotel | $183.39 | 1 | 1
Bungalow | $197.50 | 2 | 1
Apartment | $208.29 | 1 | 1
Boutique hotel | $209.70 | 1 | 1
Townhouse | $247.77 | 2 | 1
House | $261.32 | 2 | 2
Loft | $265.34 | 1 | 1
Condominium | $289.70 | 2 | 1
Serviced apartment | $331.45 | 1 | 1


