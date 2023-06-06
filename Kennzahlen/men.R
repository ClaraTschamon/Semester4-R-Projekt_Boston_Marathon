#variable löschen
rm(meanDistance)

#ausgeben
print(meanDistanceKm)



#Durchschnittsgeschwindigkeit in min/km
rnorm(Mens_Boston_Marathon_Winners$`Distance (KM)`)


#arithmetisches Mittel von distance in km; zuerst in Variable speichern --> 41.5693548387097
meanDistanceKm <- mean(Mens_Boston_Marathon_Winners$`Distance (KM)`, na.rm = TRUE)

#arithmetisches Mittel von distance in miles; zuerst in Variable speichern
meanDistanceMiles <- mean(Mens_Boston_Marathon_Winners$`Distance (Miles)`, na.rm = TRUE)

#boxplot zeigt, dass es keine abweichungen gibt. in jedem jahr wurden 41.7 km gerannt
boxplot(Mens_Boston_Marathon_Winners$`Distance (KM)`)

##############################################################

parsed_times <- strptime(Mens_Boston_Marathon_Winners$Time, format="%H:%M:%S")

numeric_times_in_seconds <- as.numeric(parsed_times)

mean_time_in_seconds <- mean(Mens_Boston_Marathon_Winners$Time, na.rm = TRUE)
cat(mean_time_in_seconds)

library(chron)
meanTime <- mean(times(Mens_Boston_Marathon_Winners$Time))


#Erstellung eines Boxplots mit den formattierten Zeiten
boxplot(times(Mens_Boston_Marathon_Winners$Time),
        main = "Boston Marathon Winners",
        xlab = "Time (hh:mm:ss)",
        col = "lightblue",
        horizontal = TRUE)

#Streudiagramm: Entfernung
plot(Mens_Boston_Marathon_Winners$`Distance (KM)`, )

#Streudiagramm: Zeit und Entfernung
plot(parsed_times, Mens_Boston_Marathon_Winners$`Distance (KM)`)

#Anzahl der Gewinner pro Land
library(ggplot2)

#Erstellung eines data.frame mit Länderzählungen
country_counts <- table(Mens_Boston_Marathon_Winners$Country)

df <- data.frame(Country = names(country_counts), Count = as.numeric(country_counts))


#Erstelle das Balkendiagramm
ggplot(df, aes(x = Country, y = Count)) +
  geom_bar(stat = "identity") +
  xlab("Land") +
  ylab("Häufigkeit") +
  ggtitle("Häufigkeit der Länder der Gewinner") +
  theme(axis.text = element_text(angle = 45, hjust = 1))


# Erstelle ein data.frame mit den Jahreszahlen und den Geschwindigkeiten
df <- data.frame(Jahr = Mens_Boston_Marathon_Winners$Year, 
                 Geschwindigkeit = times(Mens_Boston_Marathon_Winners$Time))

# Erstelle das Liniendiagramm
ggplot(df, aes(x = Jahr, y = Geschwindigkeit)) +
  geom_line() +
  xlab("Jahr") +
  ylab("Geschwindigkeit") +
  ggtitle("Geschwindigkeit im Vergleich zur Jahreszahl") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





















