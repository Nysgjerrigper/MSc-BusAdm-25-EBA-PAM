# Hente og slå sammen full tibble for sesong 22/23, 23/24 og 24/25.
rm(list = ls())

library(tidyverse)
library(stringi)

# Hent data. Sesong 22 og 23 er tilgjengelig fra vastav sin github
s22 <- read_csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/refs/heads/master/data/2022-23/gws/merged_gw.csv", 
locale = locale(encoding = "UTF-8"))
s23 <- read_csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/refs/heads/master/data/2023-24/gws/merged_gw.csv",
locale = locale(encoding = "UTF-8"))

# Ettersom sesong 24 introduserte assistent manager, må vi skaffe dataen ukentlig fra vaastav, og fjerne de
# ekstra kollonnene som assistent mangaerene introduserer.
# Funksjon for å hente ukentlig data for sesong 24/25
hent_ukentlig <- function(gw) {
  url <- paste0("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2024-25/gws/gw", gw, ".csv")
  data <- read_csv(url, locale = locale(encoding = "UTF-8"))
  return(data)
}

# Hent data for gameweeks 1 til 28 (15.03.2025)
for (i in 1:28) {
  # Hent data for hver gameweek
  gw_data <- hent_ukentlig(i)
  
  # Lagre dataen i separate dataframes
  assign(str_c("gameweek", i), gw_data, envir = .GlobalEnv)
}

# Finn og fjern ekstra kollonner
# Lagre kolonnenavn fra første sesong som referanse
colnames(s22);colnames(gameweek28)
kolonner <- s22 |> select(1:40) |> colnames() # fjern GW, ettersom ukentlig ikke har GW

# Loop gjennom alle gameweeks og fjern ekstra kolonner
for (i in 1:28) {
    # Hent data for hver gameweek
    gw_data <- get(str_c("gameweek", i))
    
    # Behold bare kolonnene som finnes i gameweek1
    behold <- gw_data |> select(all_of(kolonner[1:40]))
    
    # Lagre den rensed dataen tilbake til gameweek-variabelen
    assign(str_c("gameweek", i), behold, envir = .GlobalEnv)
}

# Sjekk
sum(colnames(gameweek1) == colnames(gameweek28)) # 40

# Sett sammen alle gameweeks til en dataframe
sesong_2425 <- tibble()

# Løkke for å sette sammen alle gameweeks
for (i in 1:28) {
  # Trekk ut df navn
  df_name <- str_c("gameweek", i)
  df <- get(df_name)
  
  # Legg til GW kolonnen fra tidligere sesonger
  if (!"GW" %in% colnames(df)) {
    df <- df |> mutate(GW = i)
  }
  
  # Legg sammen til df 
  sesong_2425 <- bind_rows(sesong_2425, df)
}

glimpse(sesong_2425)

# Gjøre om alt fra utf 8 til latin aasci
sesong2223_latinsk <- s22 |> mutate(name = stri_trans_general(name, "latin-ascii"))
sesong2324_latinsk <- s23 |> mutate(name = stri_trans_general(name, "latin-ascii"))
sesong2425_latinsk <- sesong_2425 |> mutate(name = stri_trans_general(name, "latin-ascii"))

sesong2223_latinsk |> select(GW) |> unique() |> print(n = 50) # Dronning Elisabeth II døde denne sesongen, derav ingen gw 7
sesong2324_latinsk |> select(GW) |> unique() |> print(n = 50)
sesong2425_latinsk |> select(GW) |> unique() |> print(n = 50)

## Fikse navn ----------------------------------------------------
# Siden det er noen navnefeil så må vi fikse det
riktigenavn <- c(
  "Mitoma Kaoru" = "Kaoru Mitoma",
  "Tomiyasu Takehiro" = "Takehiro Tomiyasu",
  "Endo Wataru" = "Wataru Endo",
  "Kim Ji-Soo" = "Ji-Soo Kim",
  "Olu Aina" = "Ola Aina",
  "Dominic Solanke-Mitchell" = "Dominic Solanke",
  "Kaine Kesler-Hayden" = "Kaine Kesler Hayden",
  "Adama Traore Diarra" = "Adama Traore",
  "Omari Giraud-Hutchinson" = "Omari Hutchinson",
  "Joe Gomez" = "Joseph Gomez",
  "Rodrigo 'Rodri' Hernandez" = "Rodrigo Hernandez",
  "Yehor Yarmoliuk" = "Yegor Yarmoliuk",
  "Michale Olakigbe" = "Michael Olakigbe",
  "Djordje Petrovic" = "Dorde Petrovic",
  "Joshua King" = "Josh King",
  "Ben Brereton" = "Ben Brereton Diaz",
  "Jaden Philogene" = "Jaden Philogene-Bidace",
  "Carlos Alcaraz" = "Carlos Alcaraz Duran",
  "Luis Sinisterra" = "Luis Sinisterra Lucumi",
  "Joe Aribo" = "Joe Ayodele-Aribo",
  "Joseph Hodge" = "Joe Hodge",
  "Tom Cannon" = "Thomas Cannon",
  "Yerson Mosquera" = "Yerson Mosquera Valdelamar",
  "Max Kinsey" = "Max Kinsey-Wellings",
  "Alexandre Moreno Lopera" = "Alex Moreno Lopera",
  "Joe Whitworth" = "Joseph Whitworth"
)

Sesong_22_23 <- sesong2223_latinsk |>
    mutate(name = str_replace_all(name, riktigenavn))
Sesong_23_24 <- sesong2324_latinsk |>
    mutate(name = str_replace_all(name, riktigenavn))
Sesong_24_25 <- sesong2425_latinsk |>
    mutate(name = str_replace_all(name, riktigenavn))

# Sjekk
dim(Sesong_22_23) 
dim(Sesong_23_24) 
dim(Sesong_24_25) 

sum(is.na(Sesong_22_23) |> sum()) + (is.na(Sesong_23_24) |> sum()) + (is.na(Sesong_24_25) |> sum()) 

## Slå sammen alle sesongene og lagre data ------------------------------------------------------
allesesesonger <- bind_rows(s22 = Sesong_22_23, s23 = Sesong_23_24, s24 = Sesong_24_25, .id = "season")

# Lagre data
# Sesongvis
write_csv(Sesong_22_23, file = "Sesong 22 til 23.csv")
write_csv(Sesong_23_24, file = "Sesong 23 til 24.csv")
write_csv(Sesong_24_25, file = "Sesong 24 til 25.csv")
# Samlet sett
write_csv(allesesesonger, file = "Alle tre sesonger(22-24).csv")

# Alternativt datasett

Alt_22_23 <- Sesong_22_23
Alt_23_24 <- Sesong_23_24 |> mutate(GW = GW + 38)
Alt_24_25 <- Sesong_24_25 |> mutate(GW = GW + 76)

# Slå sammen radene til en dataframe
alternativsammensatt <- bind_rows(Alt_22_23, Alt_23_24, Alt_24_25)

# Lagre filen
write_csv(alternativsammensatt, "Differensiert gw alle tre sesonger(22-24).csv")
cat("Lagret filen\n")
