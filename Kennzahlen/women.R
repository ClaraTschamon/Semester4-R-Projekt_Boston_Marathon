# Durchschnittsgeschwindigkeit in minuten/km

rnorm(Womens_Boston_Marathon_Winners$`Distance (KM)`)

summary(rnorm(Womens_Boston_Marathon_Winners$`Distance (KM)`))


# Remove NA fields
meanDistance <- mean(Womens_Boston_Marathon_Winners$`Distance (KM)`, na.rm=TRUE)

##############################################################

library(chron)

mean_time_formatted <- format(Womens_Boston_Marathon_Winners$Time, format = "%H:%M:%S")

# Print the mean time
print(mean_time_formatted)

# Convert character strings to time objects
time_objects <- as.POSIXct(mean_time_formatted, format = "%H:%M:%S")

# Calculate the mean time
mean_time <- mean(time_objects)


boxplot(sapply(strptime(all_data$Time, format = "%H:%M:%S"), function(x) as.numeric(format(x, "%M"))), type=5, col=c("red"))