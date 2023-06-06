library("lubridate")
all_times <- format(as.POSIXct(strptime(all_data$Time, format="%H:%M:%S")), format = "%H:%M:%S")
all_times

library("hms")
all_times_hms <- as_hms(all_times)

all_times_in_seconds <- seconds(all_times_hms)
all_times_in_minutes <- as.numeric(all_times_in_seconds / 60)

all_speeds <- all_times_in_minutes / all_data$Distance..KM.

mean_speeds <- mean(all_speeds, na.rm=TRUE)
mean_speeds








# Durchschnittsgeschwindigkeit in minuten/km

rnorm(all_data$Distance..KM.)


# mean ist 42.2km
meanDistance <- mean(all_data$Distance..KM., na.rm=TRUE)

# boxplot zeigt, dass es keine abweichungen gibt. in jedem jahr wurden 42.2 km gerannt
boxplot(all_data$Distance..KM.)

##############################################################

parsed_times <- strptime(all_data$Time, format="%H:%M:%S")

numeric_times_in_seconds <- as.numeric(parsed_times)#

mean_time_in_seconds <- mean(all_data$Time, na.rm = TRUE)
cat(mean_time_in_seconds)

library(chron)
meanTime <- mean(times(all_data$Time))

# Create a boxplot of the formatted times
boxplot(times(all_data$Time), 
        main = "Boston Marathon Winners", 
        xlab = "Time (hh:mm:ss)", 
        col = "lightblue",
        horizontal = TRUE)


# Streudiagramm: Entfernung
plot(all_data$Distance..KM., )

# Streudiagramm: Zeit und Entfernung
plot(parsed_times, all_data$Distance..KM.)


## zahl der gewinner pro land
library(ggplot2)

# Erstelle ein data.frame mit den Länderzählungen
country_counts <- table(all_data$Country)

df <- data.frame(Country = names(country_counts), Count = as.numeric(country_counts))

# Erstelle das Balkendiagramm
ggplot(df, aes(x = Country, y = Count)) +
  geom_bar(stat = "identity") +
  xlab("Land") +
  ylab("Häufigkeit") +
  ggtitle("Häufigkeit der Länder der GewinnerInnen") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Erstelle ein data.frame mit den Jahreszahlen und den Geschwindigkeiten
df <- data.frame(Jahr = all_data$Year, 
                 Geschwindigkeit = times(all_data$Time))

# Erstelle das Liniendiagramm
ggplot(df, aes(x = Jahr, y = Geschwindigkeit)) +
  geom_line() +
  xlab("Jahr") +
  ylab("Geschwindigkeit") +
  ggtitle("Geschwindigkeit im Vergleich zur Jahreszahl") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
