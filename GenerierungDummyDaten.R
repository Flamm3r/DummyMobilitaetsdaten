#size of the dummy dataset
num <- 100

#Variablendeklaration
pers_id <- c(1:num)               #pers_id: Eindeutige Personennummer
hh_nr <- NA                       #hh_nr:   Eindeutige Haushaltsnummer
pers_nr <- NA                     #pers_nr: Personennummer je Haushalt
hh_bdl <- NA                      #hh_bdl:  Kennzahl für das Wohnbundesland
hh_wohngem <- NA
hh_raumtyp <- NA                  #hh_raumtyp: Raumtyp des Wohnortes (z.B.: Großstadt, Stadt, Umland, Peripherie)
weg_nr <- NA                      #weg_nr:     Laufnummer der Wege pro Person
weg_startzeit <- NA
weg_startgem <- NA
weg_startbez <- NA
weg_startbdl <- NA
weg_startraumtyp <- NA
weg_quellzweck <- NA
weg_zielzweck <- NA
weg_zweck <- NA
weg_hauptvm <- NA
weg_zielzeit <- NA
weg_zielbez <- NA
weg_zielbdl <- NA
weg_zielraumtyp <- NA
weg_dauer <- NA
weg_laenge <- NA
weg_hochrechnungsfaktor <- NA

#Dataframedeklartion
df_wege <- data.frame(pers_id = pers_id, hh_nr = hh_nr, pers_nr = pers_nr, hh_bdl = hh_bdl, hh_wohngem = hh_wohngem, hh_raumtyp = hh_raumtyp, weg_nr = weg_nr, 
                        weg_startzeit = weg_startzeit, weg_startgem = weg_startgem, weg_startbez = weg_startbez, 
                        weg_startbdl = weg_startbdl, weg_startraumtyp = weg_startraumtyp, weg_quellzweck = weg_quellzweck, 
                        weg_zielzweck = weg_zielzweck, weg_zweck = weg_zweck, weg_hauptvm = weg_hauptvm, weg_zielzeit = weg_zielzeit, 
                        weg_zielbez = weg_zielbez, weg_zielbdl = weg_zielbdl, weg_zielraumtyp = weg_zielraumtyp, weg_dauer = weg_dauer, 
                        weg_laenge = weg_laenge, weg_hochrechnungsfaktor = weg_hochrechnungsfaktor)

#hh_nr generieren
for(i in 1:num){
  
  if(i == 1){
    j <- 1
    k <- 1
    rand <- sample(5:20,1)  #Anzahl an Wegen (Zeilen) je Haushalt
  }
  
  if(j <= rand){
    j <- j + 1 
    df_wege$hh_nr[i] <- k
  }else{
    j <- 1
    k <- k+1
    rand <- sample(5:20,1)  
    df_wege$hh_nr[i] <- k
  }

}

#pers_nr generieren
for(i in 1:max(df_wege$hh_nr)){
  k <- 1
  num_hhrows <- nrow(df_wege[df_wege$hh_nr == i,])  #Anzahl der Reihen je Haushalt
  rand <- sample(1:4,1)   #Anzahl an Personen je Haushalt
  
  for(j in 1:num_hhrows){
    
    if(j == 1 ){
      l <- 1
    }
    
    if(l < (num_hhrows/rand)){
      df_wege[df_wege$hh_nr == i,]$pers_nr[j] <- k
      l <- l+1
    }else{
      l <- 1
      k <- k+1
      df_wege[df_wege$hh_nr == i,]$pers_nr[j] <- k
    }
  }
}

#hh_wohnbdl generieren
for(i in 1:max(df_wege$hh_nr)){
  rand <- sample(1:9,1) #Zufällige Kennzahl für eines von neun Bundesländern
  df_wege[df_wege$hh_nr == i,]$hh_bdl <- rand
}

#hh_wohnraumtyp generieren
for(i in 1:max(df_wege$hh_nr)){
  rand <- sample(1:4,1) #Zufällige Kennzahl für einen Raumtyp (z.B.: Großstadt, Stadt, Umland, Peripherie)
  df_wege[df_wege$hh_nr == i,]$hh_raumtyp <- rand
}

#weg_nr generieren


