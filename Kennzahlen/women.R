#v <- rnorm(Womens_Boston_Marathon_Winners$`Distance (KM)`) //keine ahnung warum ich das gemacht habe


# mean ist 42.2km
meanDistance <- mean(Womens_Boston_Marathon_Winners$`Distance (KM)`, na.rm=TRUE)

# boxplot zeigt, dass es keine abweichungen gibt. in jedem jahr wurden 42.2 km gerannt
boxplot(Womens_Boston_Marathon_Winners$`Distance (KM)`)


parsed_times <- strptime(Womens_Boston_Marathon_Winners$Time, format="%H:%M:%S")


library(chron)
# mean time is 02:35:09
meanTime <- mean(times(Womens_Boston_Marathon_Winners$Time))

# Durchschnittsgeschwindigkeit in minuten/km = 3min 41sec
mean <- meanTime/meanDistance

# Boxplot of time
data <- as.POSIXct(Womens_Boston_Marathon_Winners$Time, origin=cut(Sys.time(), "hours"))

boxplot(data, 
        main = "Boston Marathon Winners (Women)",
        xlab = "Time",
        pars=list(xaxt="n"), 
        col="lightblue", 
        horizontal = TRUE)

axis.POSIXct(1, at=pretty(data), format="%H:%M:%S", las=2)


# Streudiagramm: Entfernung
plot(Womens_Boston_Marathon_Winners$`Distance (KM)`, )

# Streudiagramm: Zeit und Entfernung
plot(times(Womens_Boston_Marathon_Winners$Time), 
     Womens_Boston_Marathon_Winners$`Distance (KM)`,
     xlab = "Time",
     ylab = "Distance")



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
                 Geschwindigkeit = Womens_Boston_Marathon_Winners$Time)

# Erstelle das Liniendiagramm
ggplot(df, aes(x = Jahr, y = Geschwindigkeit)) +
  geom_line() +
  xlab("Jahr") +
  ylab("Zeit") +
  ggtitle("Zeit im Vergleich zur Jahreszahl") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##TODO: 
#- eventuell irgendwas mit regression!
#- standardabweichung für geschwindigkeit in minuten/km
#- konfidenzintervall in minuten/km


