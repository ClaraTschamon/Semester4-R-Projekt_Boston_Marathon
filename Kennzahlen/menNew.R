Mens_Boston_Marathon_Winners <- read.csv("C:/Users/Lenovo/Desktop/4.Semsester/Stochastik/R-Projekt/Daten/Mens_Boston_Marathon_Winners.csv")


#libs
library("lubridate")
library("chron")
library("hms")
library(ggplot2)
library(dplyr)

#Durchschnittsdistanz in KM - 41.5693548387097
meanDistanceKm <- mean(Mens_Boston_Marathon_Winners$Distance..KM., na.rm = TRUE)

#Boxplot für die Distanz
boxplot(Mens_Boston_Marathon_Winners$Distance..KM., main = "Boxplot für die Distanz", horizontal = TRUE)
#macht ned würkli sinn weil man anzahl der läufer nicht sieht
# Erstelle ein data.frame mit den Länderzählungen
distance_counts <- table(Mens_Boston_Marathon_Winners$Distance..KM.)

df <- data.frame(Distance = names(distance_counts), Count = as.numeric(distance_counts))

# Erstelle das Balkendiagramm
ggplot(df, aes(x = Distance, y = Count)) +
  geom_bar(stat = "identity") +
  xlab("Distanz") +
  ylab("Häufigkeit") +
  ggtitle("Häufigkeit der Distanzen") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Zeit parsen
parsed_times <- strptime(Mens_Boston_Marathon_Winners$Time, format = "%H:%M:%S")

#Zeit in Sekunden
numeric_times_in_seconds <- as.numeric(parsed_times)

#Zeit in Minuten
numeric_times_in_minutes <- numeric_times_in_seconds / 60





#Boxplot für die Durchschnittszeit
# Geschwindigkeiten von Männern
men_times <- format(as.POSIXct(strptime(Mens_Boston_Marathon_Winners$Time, format="%H:%M:%S")), 
                     format = "%H:%M:%S")
men_times_hms <- as_hms(men_times) # Umwandlung in Stunden-Minuten-Sekunden
men_times_in_seconds <- lubridate::seconds(men_times_hms)
men_times_in_minutes <- as.numeric(men_times_in_seconds / 60)
men_speeds <- men_times_in_minutes / Mens_Boston_Marathon_Winners$Distance..KM.

# Boxplot für men_speeds erstellen
boxplot(men_speeds, main = "Boxplot für Geschwindigkeiten der Männer", xlab = "Geschwindigkeit (Minuten pro Kilometer)", horizontal = TRUE)




#DurchschnittsGeschwindigkeit der Männer - 3 min 24 sek
meanSpeed <- meanTime/meanDistanceKm





#Gewinneranzahl pro Land
frequency <- table(Mens_Boston_Marathon_Winners$Country, exclude = c(NULL, ""))
country_df <- data.frame(Land = names(frequency), Häufigkeit = as.vector(unname(frequency)))
country_df <- arrange(country_df, desc(frequency))
country_df

# Abgekürzte Ländernamen erstellen
abbreviated_names <- abbreviate(country_df$Land, minlength = 10)

# Säulendiagramm mit abgekürzten Ländernamen erstellen
barplot(country_df$Häufigkeit, main = "Säulendiagramm für Länder-Häufigkeit",
        xlab = "Land", ylab = "Häufigkeit", cex.names = 0.8, ylim = c(0, max(country_df$Häufigkeit) + 1), 
        las = 2, cex.axis = 0.8, cex.lab = 0.8, width = 0.8, space = 0.5, names.arg = abbreviated_names)



# Erstelle ein data.frame mit den Jahreszahlen, Zeit und Distanz
df <- data.frame(Jahr = Mens_Boston_Marathon_Winners$Year, Zeit = times(Mens_Boston_Marathon_Winners$Time), Distanz = Mens_Boston_Marathon_Winners$Distance..KM.)

df

#Geschwindigkeit berechnen und anschließend und hinzufügen
# Geschwindigkeiten von Männern
men_times <- format(as.POSIXct(strptime(Mens_Boston_Marathon_Winners$Time, format="%H:%M:%S")), 
                    format = "%H:%M:%S")
men_times_hms <- as_hms(men_times) # Umwandlung in Stunden-Minuten-Sekunden
men_times_in_seconds <- lubridate::seconds(men_times_hms)
men_times_in_minutes <- as.numeric(men_times_in_seconds / 60)
men_speeds <- men_times_in_minutes / Mens_Boston_Marathon_Winners$Distance..KM.

df <- data.frame(df, Geschwindigkeit = men_speeds)
df


# Erstelle Streudiagramm
plot(df$Geschwindigkeit, df$Distanz, xlab = "Geschwindigkeit", ylab = "Distanz", main ="Vergleich zwischen der Geschwindigkeit und der Entfernung")





