# Hente og slå sammen full tibble for sesong 22/23, 23/24 og 24/25.
rm(list = ls(all = TRUE))

library(tidyverse)
library(stringi)

# Henting av data =====

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

# Hent data for gameweeks 1 til 29 (28.03.2025)
for (i in 1:29) {
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
for (i in 1:29) {
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
for (i in 1:29) {
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

## Fikse navn ====
## Siden det er noen navnefeil så må vi fikse det
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

# Ben Davies
Sesong_22_23 <- Sesong_22_23 %>%
  mutate(name = if_else(name == "Ben Davies" & team == "Liverpool",
                        "Ben Davies Liverpool", name))
Sesong_23_24 <- Sesong_23_24 %>%
  mutate(name = if_else(name == "Ben Davies" & team == "Liverpool",
                        "Ben Davies Liverpool", name))
Sesong_24_25 <- Sesong_24_25 %>%
  mutate(name = if_else(name == "Ben Davies" & team == "Liverpool",
                        "Ben Davies Liverpool", name))

#View(Sesong_24_25)

# Sjekk
dim(Sesong_22_23) 
dim(Sesong_23_24) 
dim(Sesong_24_25) 

sum(is.na(Sesong_22_23) |> sum()) + (is.na(Sesong_23_24) |> sum()) + (is.na(Sesong_24_25) |> sum()) 

# Fiks lagnavn
lag2022 <- c(
  "Arsenal", "Aston Villa", "Bournemouth", "Brentford", "Brighton", 
  "Chelsea", "Crystal Palace", "Everton", "Fulham", "Leeds", 
  "Leicester", "Liverpool", "Man City", "Man United", "Newcastle", 
  "Nottingham Forest", "Southampton", "Tottenham", "West Ham", "Wolves"
)
Sesong_22_23 <- Sesong_22_23 %>%
  mutate(opponent_team = lag2022[opponent_team])

Sesong_22_23 <- Sesong_22_23 %>%
  mutate(team = recode(team,
                       "Spurs" = "Tottenham",
                       "Nott'm Forest" = "Nottingham Forest",
                       "Man Utd" = "Man United",
                       "Leicester City" = "Leicester"))

lag2023 <- c(
  "Arsenal", "Aston Villa", "Bournemouth", "Brentford", "Brighton", 
  "Burnley", "Chelsea", "Crystal Palace", "Everton", "Fulham", 
  "Liverpool", "Luton", "Man City", "Man United", "Newcastle", 
  "Nottingham Forest", "Sheffield Utd", "Tottenham", "West Ham", "Wolves"
)
Sesong_23_24 <- Sesong_23_24 %>%
  mutate(opponent_team = lag2023[opponent_team])
Sesong_23_24 <- Sesong_23_24 %>%
  mutate(team = recode(team,
                       "Spurs" = "Tottenham",
                       "Nott'm Forest" = "Nottingham Forest",
                       "Man Utd" = "Man United"
  ))

lag2024 <- c(
  "Arsenal", "Aston Villa", "Bournemouth", "Brentford", "Brighton", 
  "Chelsea", "Crystal Palace", "Everton", "Fulham", "Ipswich", 
  "Leicester", "Liverpool", "Man City", "Man United", "Newcastle", 
  "Nottingham Forest", "Southampton", "Tottenham", "West Ham", "Wolves"
)
Sesong_24_25 <- Sesong_24_25 %>%
  mutate(opponent_team = as.numeric(opponent_team)) %>%
  mutate(opponent_team = lag2024[opponent_team])
Sesong_24_25 <- Sesong_24_25 %>%
  mutate(team = recode(team,
                       "Spurs" = "Tottenham",
                       "Man Utd" = "Man United",
                       "Nott'm Forest" = "Nottingham Forest"
  ))


# Fjern Element Kolonner, denne inneholder ID til spillere men varierer fra sesong til sesong

Sesong_22_23 <- Sesong_22_23 |> select(-element)
Sesong_23_24 <- Sesong_23_24 |> select(-element)
Sesong_24_25 <- Sesong_24_25 |> select(-element)

# Sjekk
dim(Sesong_22_23);dim(Sesong_23_24);dim(Sesong_24_25) # 40

# Fjern Assistent Managere fra datasettet.

assistentmanagere <- c("Mikel Arteta", "Enzo Maresca", "Arne Slot", "Pep Guardiola", 
                        "Eddie Howe","Andoni Iraola","Fabian Hurzeler",
                        "Marco Silva", "Nuno Espirito Santo", "Ange Postecoglou",
                         "Unai Emery", "Thomas Frank", "Oliver Glasner",
                         "Ruben Amorim", "Vitor Pereira", "David Moyes",
                         "Kieran McKenna", "Ruud van Nistelrooy", "Ivan Juric",
                         "Graham Potter") # Managerliste

# Fjern AM
Sesong_24_25 <- Sesong_24_25 |> filter(!name %in% assistentmanagere)

# Sjekk om noen assistentmanagere fortsatt finnes i datasettet
Sesong_24_25 |> filter(name %in% assistentmanagere)


# Sammenslåing og Lagring ====

## Sammenslåing og numerifisering ====

### Egen kolonne med Sesong ID og Spiller ID ====
sesongogspiller_id <- bind_rows("Sesong 22/23" = Sesong_22_23, "Sesong 23/24" = Sesong_23_24, "Sesong 24/25" = Sesong_24_25, .id = "season")

# Numerifisering

sesong_ogspiller_id <- sesong_ogspiller_id |>
  mutate(player_id = as.integer(factor(name)))

sesong_ogspiller_id <- sesong_ogspiller_id %>%
  mutate(was_home = as.integer(was_home))  

### Datasett med GW i stigende rekkefølge ====
Alt_22_23 <- Sesong_22_23
Alt_23_24 <- Sesong_23_24 |> mutate(GW = GW + 38)
Alt_24_25 <- Sesong_24_25 |> mutate(GW = GW + 76)

# Sammenslåing
stigenderekkefølge <- bind_rows(Alt_22_23, Alt_23_24, Alt_24_25)

# Numerifisering

stigenderekkefølge <- stigenderekkefølge %>%
  mutate(was_home = as.integer(was_home))

stigenderekkefølge <- stigenderekkefølge %>%
  mutate(player_id = as.integer(factor(name)))

### Alternativt ====
alternativtsammensatt <- bind_rows(Alt_22_23, Alt_23_24, Alt_24_25)

alternativtsammensatt <- alternativtsammensatt %>%
  mutate(
    kickoff_time = as.POSIXct(kickoff_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    player_id = as.integer(factor(name)),         # Unique global player IDs
    oID = as.integer(factor(opponent_team)),  # Unique opponent IDs
    tID = as.integer(factor(team)),           # Unique team IDs
    hID = as.integer(was_home)                # Home/away indicator
  ) %>%
  arrange(player_id, kickoff_time) %>%  # Arrange by the new pID and kickoff_time
  mutate(row_id = row_number())   # Create a row identifier

# Lagre data =====

## Sesongvis ====
write_csv(Sesong_22_23, file = "Sesong 22 til 23.csv")
write_csv(Sesong_23_24, file = "Sesong 23 til 24.csv")
write_csv(Sesong_24_25, file = "Sesong 24 til 25.csv")
cat("Lagret filene\n")

## Samlet ====

# Samlet sett
write_csv(sesongogspiller_id, file = "Sesong id, alle tre sesonger(22-24).csv")
cat("Lagret filen\n")

# Stigende rekkefølger GW
write_csv(stigenderekkefølge, "Stigende GW, alle tre sesonger(22-24).csv")

# Alternativt Datasett 
write_csv(alternativtsammensatt, "Ekstra kolonner, stigende GW, alle tre sesonger(22-24), heltall.csv")
cat("Lagret alternativ filen\n")
