Primes <- read.csv("Primes.csv", header=F)

avg <- (Primes[3]+Primes[4]+Primes[5]+Primes[6]+Primes[7]+Primes[8]+Primes[9]+Primes[10]+Primes[11]+Primes[12])/10
Data.name <- Primes[[1]]
Data.val <- Primes[[2]]
avg <- avg[[1]]

pdf(file = "Primes.pdf", width = 6.25, height = 5.5, family = "Times", pointsize = 18)

plot(Data.val[Data.name=='Choco'], avg[Data.name=='Choco'], type='l', xlim = c(5, 150), ylim = c(1, 3000),  xlab = "Size (n)", ylab = "Time (milliseconds)", col = c("dark green", "orange", "red","blue"), pch = c(21, 22, 23), cex = 3, lty = "solid", lwd = 3)
lines(Data.val[Data.name=='PAC'], avg[Data.name=='PAC'], col='orange',lty="longdash", lwd = 3)
lines(Data.val[Data.name=='PAS'], avg[Data.name=='PAS'], col='red',lty="dashed", lwd = 3)
lines(Data.val[Data.name=='PAS-SAT'], avg[Data.name=='PAS-SAT'], col='blue',lty="dotted", lwd = 3)


leg.txt <- c("Choco", "PAC", "PAS", "PAS-SAT")
savefont <- par(family="Helvetica")
legend("topleft", legend=leg.txt, cex=0.6666, col = c('dark green','orange','red','blue'), lty = c("solid","longdash", "dashed","dotted"), 
       merge = TRUE, bg="white")
par(savefont)       

dev.off()


Schedules <- read.csv("Schedules.csv", header=F)

avg <- (Schedules[3]+Schedules[4]+Schedules[5]+Schedules[6]+Schedules[7]+Schedules[8]+Schedules[9]+Schedules[10]+Schedules[11]+Schedules[12])/10
Data.name <- Schedules[[1]]
Data.val <- Schedules[[2]]
avg <- avg[[1]]

pdf(file = "Schedules.pdf", width = 6.25, height = 5.5, family = "Times", pointsize = 18)

plot(Data.val[Data.name=='Choco'], avg[Data.name=='Choco'], type='l', xlim = c(5, 70), ylim = c(1, 2000),  xlab = "Size (n)", ylab = "Time (milliseconds)", col = c("dark green", "orange","red","blue"), pch = c(21, 22, 23), cex = 3, lty = "solid", lwd = 3)
lines(Data.val[Data.name=='PAC'], avg[Data.name=='PAC'], col='orange',lty="longdash", lwd = 3)
lines(Data.val[Data.name=='PAS'], avg[Data.name=='PAS'], col='red',lty="dashed", lwd = 3)
lines(Data.val[Data.name=='PAS-SAT'], avg[Data.name=='PAS-SAT'], col='blue',lty="dotted", lwd = 3)

leg.txt <- c("Choco", "PAC", "PAS", "PAS-SAT")
savefont <- par(family="Helvetica")
legend("topleft", legend=leg.txt,cex=0.6666, col = c('dark green','orange','red','blue'), lty = c("solid","longdash", "dashed","dotted"), 
       merge = TRUE, bg="white")
par(savefont)
dev.off()
