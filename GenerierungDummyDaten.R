#################################
### size of the dummy dataset ###
#################################
num <- 100                        #Anzahl der Datenzeilen

############################
### Variablendeklaration ###
############################
pers_id <- c(1:num)               #pers_id: Eindeutige Personennummer (dient initial zur erstellung des leeren Datensatzes in der gewünschten Größe)
hh_nr <- NA                       #hh_nr:   Eindeutige Haushaltsnummer
pers_nr <- NA                     #pers_nr: Personennummer je Haushalt
hh_bdl <- NA                      #hh_bdl:  Kennzahl für das Wohnbundesland
hh_wohngem <- NA
hh_raumtyp <- NA                  #hh_raumtyp:    Raumtyp des Wohnortes (z.B.: Großstadt, Stadt, Umland, Peripherie)
weg_nr <- NA                      #weg_nr:        Laufnummer der Wege pro Person
weg_startzeit <- NA               #weg_startzeit: Startzeit des Weges
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

###########################
### Dataframedeklartion ###
###########################
df_wege <- data.frame(pers_id = pers_id, hh_nr = hh_nr, pers_nr = pers_nr, hh_bdl = hh_bdl, hh_wohngem = hh_wohngem, hh_raumtyp = hh_raumtyp, weg_nr = weg_nr, 
                        weg_startzeit = weg_startzeit, weg_startgem = weg_startgem, weg_startbez = weg_startbez, 
                        weg_startbdl = weg_startbdl, weg_startraumtyp = weg_startraumtyp, weg_quellzweck = weg_quellzweck, 
                        weg_zielzweck = weg_zielzweck, weg_zweck = weg_zweck, weg_hauptvm = weg_hauptvm, weg_zielzeit = weg_zielzeit, 
                        weg_zielbez = weg_zielbez, weg_zielbdl = weg_zielbdl, weg_zielraumtyp = weg_zielraumtyp, weg_dauer = weg_dauer, 
                        weg_laenge = weg_laenge, weg_hochrechnungsfaktor = weg_hochrechnungsfaktor)

########################
### hh_nr generieren ###
########################

for(i in 1:nrow(df_wege)){   #Durchläuft den gesamten Datensatz       
  
  if(i == 1){                #Beim ersten Durchlauf der Schleife
    rand <- sample(5:20,1)     #Anzahl an Wegen (Zeilen) je Haushalt
    j <- 1                     #Zaehlvariable für die Zeilen je Haushalt
    k <- 1                     #Zaehlvariable für die Haushaltsnummer
  }
  
  if(j <= rand){            #Überprüft ob es noch immer der selbe Haushalt ist (dann wid k nicht inkrementiert)
    j <- j + 1                 #Zählt die Zeilen innerhalb eines Haushaltes mit 
    df_wege$hh_nr[i] <- k      #Weist der entsprechenden Datenzeile die aktuelle Haushaltsnummer zu
  }else{                    #Wird die Anzahl an Zeilen des aktuellen Haushalts überschritten/wird ein neuer Haushalt erreich
    j <- 1                     #Zählvariable für die Zeilen des akteullen Haushalt wird auf 1 zurückgesetzt
    k <- k+1                   #Haushaltsnummer wird um +1 inkrementiert
    rand <- sample(5:20,1)     #neue Anzahl an Zeilen des genenerierten Haushalts
    df_wege$hh_nr[i] <- k      #weist der Zeile die aktuelle/zugehörige Haushaltsnummer zu
  }

}

##########################
### pers_nr generieren ###
##########################
for(i in 1:max(df_wege$hh_nr)){     #Duchläuft die Anzahl an Haushalten im Datensatz (Aktueller Haushalt entspricht i)
  k <- 1                                #
  num_hhrows <- nrow(df_wege[df_wege$hh_nr == i,])  #Anzahl der Reihen je Haushalt
  rand <- sample(1:4,1)                 #Anzahl an Personen je Haushalt
  
  for(j in 1:num_hhrows){               #Durchläuft die Zeilen (j) des aktuellen Haushalts (i)
    if(j == 1 ){                            #in der ersten Zeile des Haushalts wird die Zählvariable l auf 1 gesetzt
      l <- 1
    }
    
    if(l < (num_hhrows/rand)){                        #Teilt die Reihen pro Haushalt mit der zufälligen Anzahl an darin lebenden Personen gleichmäßig auf
      df_wege[df_wege$hh_nr == i,]$pers_nr[j] <- k        #weist der aktuellen Zeile eine Personennummer (k) zu
      l <- l+1                                            #Zählvariable wird erhöht um die Zeilen pro Person zu kontrollieren
    }else{                                            #Alle einer Person im HH zugewiesenen Zeilen sind erschöpft aber es gibt noch weitere Personen im Haushalt
      l <- 1                                              #Zählvariable für die Zeilen pro Person im HH wird auf 1 zurückgesetzt
      k <- k+1                                            #Personennummer je Haushalt wird um 1 inkrementiert
      df_wege[df_wege$hh_nr == i,]$pers_nr[j] <- k        #Inkrementierte Personennummer wird der aktuellen Zeile zugewiesen
    }
  }
}

##########################
### pers_id generieren ###
##########################
for(i in 1:nrow(df_wege)){                      #Durchläuft alle Zeilen des Datensatzes
  if(i == 1){                                   #Bei der ersten Datenzeile
    pers_id_akt <- 1                                #pers_id wird mit 1 initialisiert
    df_wege$pers_id[i] <- pers_id_akt               #pers_id wird zugewiesen
  }else{                                        #Alle Zeilen außer der ersten
    if(df_wege$pers_nr[i] == df_wege$pers_nr[i-1]){ #Wenn die pers_nr in der aktuellen Zeile die selbe ist wie in der Zeile davor (selbe Person)
      df_wege$pers_id[i] <- pers_id_akt                 #pers_id wird nicht inkrementiert (keine neue Person) und der Datenzeile zugewiesen
    }else{                                          #Wenn die pers_nr in der aktuellen Zeile nicht der pers_nr der vorherigen Zeile entspricht (neue Person)
      pers_id_akt <- pers_id_akt + 1                    #pers_id wird inkrementiert, da es sich um eine neue Person handelt
      df_wege$pers_id[i] <- pers_id_akt                 #Inkrementierte (neue) pers_id wird der aktuellen Datenzeile zugewisen
    }
  }
}


#############################
### hh_wohnbdl generieren ###
#############################
for(i in 1:max(df_wege$hh_nr)){                 #Durchläuft alle Haushalte im Datensatz
  rand <- sample(1:9,1)                             #Zufällige Kennzahl für eines von neun Bundesländern
  df_wege[df_wege$hh_nr == i,]$hh_bdl <- rand       #Weist allen Datenzeilen des aktuellen Haushalts (i) die zufällige Bundesländerkennzahl zu
}

#################################
### hh_wohnraumtyp generieren ###
#################################
for(i in 1:max(df_wege$hh_nr)){                 #Durchläuft alle Haushalte im Datensatz
  rand <- sample(1:4,1)                             #Zufällige Kennzahl für einen Raumtyp (z.B.: Großstadt, Stadt, Umland, Peripherie)
  df_wege[df_wege$hh_nr == i,]$hh_raumtyp <- rand   #Weist allen Datenzeilen des aktuellen Haushalts (i) den zufälligen Wohnraumtyp zu
}

#########################
### weg_nr generieren ###
#########################
weg_nr_pers <- 1                         #Initialisierung für die Wegenummer pro Person
for(i in 1:nrow(df_wege)){               #Durchlaeuft alle Zeilen des Datensatzes
  if(i == 1){                               #Erster Schleifendurchlauf
    #last_row <- 1                          #Zählvariable die im weiteren Durchlauf i-1 entspricht
    df_wege$weg_nr[i] <- 1                  #Setzt den ersten Weg im Datensatz auf Wegnr 1
  }else{                                  #betrifft alle Schleifendurchläufe außer den ersten
    last_pers_nr <- df_wege$pers_nr[i-1]    #Personennummer in der vorherigen Zeile
    act_pers_nr <- df_wege$pers_nr[i]       #Personennummer der aktuellen Zeil
    if(last_pers_nr == act_pers_nr){      #Wenn die Personenummer in einer neuen Zeile gleich bleibt steigt die Wegenummer
      weg_nr_pers <- weg_nr_pers + 1        #Wegenummer pro Person wird erhöht
      df_wege$weg_nr[i] <- weg_nr_pers
    }else{                                #Personennummer neuer Zeile ändert sich (es wird eine neue Person behandelt)
      weg_nr_pers <- 1                      #Wegnummer pro Person wird wieder auf 1 gesetzt
      df_wege$weg_nr[i] <- weg_nr_pers
    }
  }
}

################################
### weg_startzeit generieren ###
################################

#Funktion die eine zufällige Startzeit innerhalb der angegebenen Paramenter (Stunden) zurückgibt
funct_weg_startzeit <- function(startzeit_h_von, startzeit_h_bis){                                  
  weg_startzeit_h   <- toString(sample(startzeit_h_von:startzeit_h_bis,1))                #generiert eine Starzeit im Format "h" oder hh"
  weg_startzeit_min <- toString(sample(0:59,1))                                           #generiert die zugehörigen Minuten im Format "m" oder "mm"
  if(nchar(weg_startzeit_h)<2){
    weg_startzeit_h <- paste0("0", weg_startzeit_h)                                       #Formt die Minuten auf das Format "hh" um, fals nur ein einstellige Stundenanzahl generiert wurde
  }
  if(nchar(weg_startzeit_min)<2){                                                         #Formt die Minuten auf das Format "mm" um, fals nur ein einstellige Minutenanzahl generiert wurde
    weg_startzeit_min <- paste0("0", weg_startzeit_min)
  }
  weg_startzeit <- paste(weg_startzeit_h, weg_startzeit_min, sep = ":")                   #Verbindet die generierten Zeiten zu dem Formet "hh:mm"
  return(weg_startzeit)
}

#Schleife zum befüllen des Datensatzes
for(i in 1:nrow(df_wege)){                                  #Durchläuft alle Zeilen des Datensatzes
  akt_weg_nr <- df_wege$weg_nr[i]                             #Variable für die Wegenummer der aktuellen Datenzeile
  if(akt_weg_nr == 1){                                        #Wenn es sich um den ersten Weg einer Person handelt
    df_wege$weg_startzeit[i] <- funct_weg_startzeit(4,9)          #Beim ersten Weg wird eine zufällige Starzeit zwischen 04:00 und 09:00 Uhr generiert
  }else{                                                      #Bei allen weiteren wegen einer PErson wird ein zufällige Starzeit zwisch 09:00 bis 23:00 Uhr erstellt
    df_wege$weg_startzeit[i] <- funct_weg_startzeit(9,23)
  }
}
df_wege$weg_startzeit <- strptime(df_wege$weg_startzeit, '%H:%M')   #Wandelt die Startzeit vom String in ein Time-Format um

#Schleife zum sortieren der Beginnzeiten je Person
for(i in 1:max(df_wege$pers_id)){                         #Durchläuft jede im Datensatz vorhandene Person
  df_wege[df_wege$pers_id == i,]$weg_startzeit <- sort(df_wege[df_wege$pers_id == i,]$weg_startzeit)  #Sortiert die Beginnzeiten der Wege pro Person in aufsteigender Reihenfolge
}
