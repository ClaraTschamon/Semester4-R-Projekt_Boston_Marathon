# Durchschnittsgeschwindigkeit in minuten/km

rnorm(Womens_Boston_Marathon_Winners$`Distance (KM)`)


# mean ist 42.2km
meanDistance <- mean(Womens_Boston_Marathon_Winners$`Distance (KM)`, na.rm=TRUE)

# boxplot zeigt, dass es keine abweichungen gibt. in jedem jahr wurden 42.2 km gerannt
boxplot(Womens_Boston_Marathon_Winners$`Distance (KM)`)

##############################################################

parsed_times <- strptime(Womens_Boston_Marathon_Winners$Time, format="%H:%M:%S")

numeric_times_in_seconds <- as.numeric(parsed_times)

mean_time_in_seconds <- mean(Womens_Boston_Marathon_Winners$Time, na.rm = TRUE)
cat(mean_time_in_seconds)

library(chron)
meanTime <- mean(times(Womens_Boston_Marathon_Winners$Time))

# Create a boxplot of the formatted times
boxplot(times(Womens_Boston_Marathon_Winners$Time), 
        main = "Boston Marathon Winners", 
        xlab = "Time (hh:mm:ss)", 
        col = "lightblue",
        horizontal = TRUE)


# Streudiagramm: Entfernung
plot(Womens_Boston_Marathon_Winners$`Distance (KM)`, )

# Streudiagramm: Zeit und Entfernung
plot(parsed_times, Womens_Boston_Marathon_Winners$`Distance (KM)`)


## zahl der gewinner pro land
library(ggplot2)

# Erstelle ein data.frame mit den Länderzählungen
country_counts <- table(Womens_Boston_Marathon_Winners$Country)

df <- data.frame(Country = names(country_counts), Count = as.numeric(country_counts))

# Erstelle das Balkendiagramm
ggplot(df, aes(x = Country, y = Count)) +
  geom_bar(stat = "identity") +
  xlab("Land") +
  ylab("Häufigkeit") +
  ggtitle("Häufigkeit der Länder der Gewinnerinnen") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Erstelle ein data.frame mit den Jahreszahlen und den Geschwindigkeiten
df <- data.frame(Jahr = Womens_Boston_Marathon_Winners$Year, 
                 Geschwindigkeit = times(Womens_Boston_Marathon_Winners$Time))

# Erstelle das Liniendiagramm
ggplot(df, aes(x = Jahr, y = Geschwindigkeit)) +
  geom_line() +
  xlab("Jahr") +
  ylab("Geschwindigkeit") +
  ggtitle("Geschwindigkeit im Vergleich zur Jahreszahl") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




