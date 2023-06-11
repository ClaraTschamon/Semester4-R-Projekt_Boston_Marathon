Mens_Boston_Marathon_Winners <- read.csv("C:/Users/Lenovo/Desktop/4.Semsester/Stochastik/R-Projekt/Daten/Mens_Boston_Marathon_Winners.csv")


#Durchschnittsdistanz in KM - 41.5693548387097
meanDistanceKm <- mean(Mens_Boston_Marathon_Winners$Distance..KM., na.rm = TRUE)

#Boxplot für die Distanz
boxplot(Mens_Boston_Marathon_Winners$Distance..KM., main = "Boxplot für die Distanz", horizontal = TRUE)



#Zeit parsen
parsed_times <- strptime(Mens_Boston_Marathon_Winners$Time, format = "%H:%M:%S")

#Zeit in Sekunden
numeric_times_in_seconds <- as.numeric(parsed_times)

#Zeit in Minuten
numeric_times_in_minutes <- numeric_times_in_seconds / 60



library(chron)

#Durchschnittszeit -02:21:31
meanTime <- mean(times(Mens_Boston_Marathon_Winners$Time))


#Boxplot für die Zeit
timeData <- times(Mens_Boston_Marathon_Winners$Time)

boxplot(timeData,
        main ="Boston Marathon Winners (Men)",
        xlab ="Time",
        col = "lightblue",
        horizontal = TRUE)



#DurchschnittsGeschwindigkeit der Männer - 3 min 24 sek
meanSpeed <- meanTime/meanDistanceKm


#Streudiagramm für die Entfernung
plot(Mens_Boston_Marathon_Winners$Distance..KM., main = "Streudiagramm (Männer)")

#Streudiagramm für Zeit und Entfernung
plot(parsed_times, Mens_Boston_Marathon_Winners$Distance..KM.)




#Gewinneranzahl pro Land
library(ggplot2)

#Datentabelle der Länderzählungen
country_counts <- table(Mens_Boston_Marathon_Winners$Country)

df <- data.frame(Country = names(country_counts), Count = as.numeric(country_counts))

print(df)

#Balkendiagramm für Gewinneranzahl pro Land
ggplot(df, aes(x = Country, y = Count)) +
  geom_bar(stat = "identity") +
  xlab("Land") +
  ylab("Häufigkeit") +
  ggtitle("Häufigkeit der Länder der Gewinner") +
  theme(axis.text = element_text(angle = 45, hjust = 1))
#Die meisten Gewinner kommen aus USA mit 43, gefolgt von Kenya mit 24 und Canada mit 16




# Erstelle ein data.frame mit den Jahreszahlen und den Zeit
df <- data.frame(Jahr = Mens_Boston_Marathon_Winners$Year, Zeit = times(Mens_Boston_Marathon_Winners$Time), Distanz = Mens_Boston_Marathon_Winners$Distance..KM.)


print(df)

#Geschwindigkeit berechnen und anschließend und hinzufügen
speed <- df$Distanz / df$Zeit

df <- data.frame(df, Geschwindigkeit = speed)
df

# Erstelle das Liniendiagramm
ggplot(df, aes(x = Jahr, y = Zeit)) +
  geom_line() +
  xlab("Jahr") +
  ylab("Zeit") +
  ggtitle("Zeit im Vergleich zur Jahreszahl") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Erstelle das Liniendiagramm
ggplot(df, aes(x = Geschwindigkeit, y = Jahr)) +
  geom_line() +
  xlab("Geschwindigkeit") +
  ylab("Jahr") +
  ggtitle("Zeit im Vergleich zur Jahreszahl") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#liniendiagramm nr2 - keine ahnung welles passt

plot(df$Jahr, df$Geschwindigkeit, type = "l", main = "Geschwindigkeiten über die Jahre", xlab = "Jahr", ylab = "Geschwindigkeit")







