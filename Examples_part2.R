### Examples Sao Carlos short course training - part 2

library(INLAjoint)
setwd("/home/dr/Documents/GitHub/Short-course-Sao-Carlos-INLA")
load("Data_examples.RData") # contains 3 datasets (pbc2 from package JM, bmt from package smcure and readmission from package frailtypack)

LongData <- pbc2[, c("id", "year", "serBilir","drug")]
print(head(LongData, 10), row.names=F)
# mixed effects regression model
M1 <- joint(formLong = serBilir ~ year + drug  +  (1 + year|id),
            dataLong = LongData, id = "id", timeVar = "year",
            family = "lognormal")
summary(M1)
summary(M1, sdcor=TRUE)
inla.priors.used(M1)
cat(capture.output(inla.priors.used(M1))[1:22], sep = "\n") # fixed effects
plot(M1)
plot(M1, sdcor=T)

plot(M1, priors=TRUE, sdcor=TRUE)$Covariances

# change priors
M1_2 <- joint(formLong = serBilir ~ year + drug  +  (1 + year|id),
            dataLong = LongData, id = "id", timeVar = "year",
            family = "lognormal",
            control=list(priorFixed=list(mean=0, prec=0.1, mean.intercept=0, prec.intercept=0.1),
                         priorRandom=list(r=10, R=1)))
summary(M1_2)
cat(capture.output(inla.priors.used(M1_2))[1:22], sep = "\n")








# prior sensitivity analysis
LongDatatime05 <- LongData[c(2,which(diff(as.integer(LongData$id))==1)+2),]
LongDatatime05 <- LongDatatime05[-which(diff(as.integer(LongDatatime05$id))==0),]
LongDatatime05$year <- 0.5

LongData_ni <- rbind(LongData[c(1,which(diff(as.integer(LongData$id))==1)+1),], LongDatatime05)
LongData_ni <- LongData_ni[order(LongData_ni$id),]
LongData_2 <- LongData[, c("id", "year", "serBilir", "drug")]

M2 <- joint(formLong = serBilir ~ year + drug  +  (1 + year|id),
            dataLong = LongData, id = "id", timeVar = "year",
            family = "lognormal", control=list(priorRandom=list(r=100, R=1)))

M3 <- joint(formLong = serBilir ~ year + drug  +  (1 + year|id),
            dataLong = LongData_ni, id = "id", timeVar = "year",
            family = "lognormal", control=list(priorRandom=list(r=10, R=1)))

M4 <- joint(formLong = serBilir ~ year + drug  +  (1 + year|id),
            dataLong = LongData_ni, id = "id", timeVar = "year",
            family = "lognormal", control=list(priorRandom=list(r=100, R=1)))

plot(M1, priors=TRUE, sdcor=TRUE)$Covariances
plot(M2, priors=TRUE, sdcor=TRUE)$Covariances
plot(M3, priors=TRUE, sdcor=TRUE)$Covariances
plot(M4, priors=TRUE, sdcor=TRUE)$Covariances



# This chunk makes the plot for the correlation parameter only:
# A <- plot(M1, priors=TRUE, sdcor=TRUE)$Covariances
# B <- plot(M2, priors=TRUE, sdcor=TRUE)$Covariances
# C <- plot(M3, priors=TRUE, sdcor=TRUE)$Covariances
# D <- plot(M4, priors=TRUE, sdcor=TRUE)$Covariances
#
# A$L1$data <- A$L1$data[which(A$L1$data$Effect=="year_L1:Intercept_L1"), ]
# A$L1$data$Effect <- "Identifiable model, prior 1"
# B$L1$data <- B$L1$data[which(B$L1$data$Effect=="year_L1:Intercept_L1"), ]
# B$L1$data$Effect <- "Identifiable model, prior 2"
# C$L1$data <- C$L1$data[which(C$L1$data$Effect=="year_L1:Intercept_L1"), ]
# C$L1$data$Effect <- "Non-identifiable model, prior 1"
# D$L1$data <- D$L1$data[which(D$L1$data$Effect=="year_L1:Intercept_L1"), ]
# D$L1$data$Effect <- "Non-identifiable model, prior 2"
#
# E <- rbind(A$L1$data, B$L1$data, C$L1$data, D$L1$data)
#
# CorPlots <- ggplot(E, aes(x=x,y=y,group=group)) +
#   xlab('') +
#   ylab('Density') +
#   geom_line(aes(color=group, linetype=group)) +
#   facet_wrap(~Effect, scales='free') + xlim(-1,1) + ylim(0,6.1)
#
# CorPlots







