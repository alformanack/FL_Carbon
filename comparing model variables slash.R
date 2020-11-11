rm(list=ls())

setwd("C:/Users/Alicia/Documents/GitHub/FL_Carbon")

df<-read.csv(file= "slash_compare_model_variables.csv")

plot(df$ï..observed.d_in-df$modeled.d_in~df$Age)
plot(df$ï..observed.d_in-df$modeled.d_in~df$SM)
model.1<-lm(log10(df$ï..observed.d_in/df$modeled.d_in)~df$Temp+log10(df$Age)+df$SM+log10(df$CN)+df$tree.density_Ha)
summary(model.1)
model.1<-lm(log10(ï..observed.d_in/modeled.d_in)~Temp+log10(Age)+SM+log10(CN)+tree.density_Ha,data=df)
model.2<-lm(log10(ï..observed.d_in/modeled.d_in)~log10(Age)+SM,data=df)
summary(model.2)

plot(df$ï..observed.d_in~df$modeled.d_in, xlim=c(2,9), xlab="Modeled Diameter", ylab="Observed Diameter", main= "Before Parameter Correction")
abline(0,1, col="red")

# Model corrections for slash with aridity --------------------------------

df<-read.csv(file= "slash simulation2 with aridity.csv")

plot(df$ï..observed.d_in-df$modeled.d_in~df$Age)
plot(df$ï..observed.d_in-df$modeled.d_in~df$SM)
model.1<-lm(log10(df$ï..observed.d_in/df$modeled.d_in)~df$Temp+log10(df$Age)+df$aridity+df$tree.density_Ha+df$CN)
summary(model.1)
res.age<-residuals(model.1)
plot(res.age~df$Age)
plot(res.age~df$aridity)

model.2<-lm(log10(ï..observed.d_in/modeled.d_in)~log10(Age)+Temp,data=df)
summary(model.2)

plot(df$ï..observed.d_in~df$modeled.d_in, xlim=c(1,8), xlab="Modeled Diameter", ylab="Observed Diameter", main= "Before Parameter Correction",
     cex.axis=1.5, col.axis="#027368", col="#75BFBF", pch=16, type="p", font=2)
abline(0,1, col="#048ABF")



# after correction with aridity -------------------------------------------

df<-read.csv(file= "after_correction_with_aridity_allsimulations.csv")


plot(df$ï..observed.d_in~df$modeled.d_in, xlim=c(1,8), xlab="Modeled Diameter", ylab="Observed Diameter", main= "After Parameter Correction")
abline(0,1, col="red")


# After correction with old growth ----------------------------------------


df<-read.csv(file= "old growth correction.csv")

plot(df$observed.d_in-df$modeled.d_in~df$Age)

model.1<-lm(log10(df$observed.d_in/df$modeled.d_in)~df$Temp+log10(df$Age)+df$aridity+df$tree.density_Ha+df$CN)
summary(model.1)
res.age<-residuals(model.1)
plot(res.age~df$Age)
plot(res.age~df$aridity)

model.2<-lm(log10(ï..observed.d_in/modeled.d_in)~log10(Age)+aridity,data=df)
summary(model.2)

plot(df$observed.d_in~df$modeled.d_in, xlim=c(1,8), xlab="Modeled Diameter", ylab="Observed Diameter", main= "After Parameter Correction",
     cex.axis=1.5, col.axis="#027368", col="#75BFBF", pch=16, type="p", font=2)
abline(0,1, col="#048ABF")










df<-read.csv(file= "slash_error_corrected.csv")

Age<-c(1:120)

modeled.age<-0.346*Age^(-0.654)
corrected.age<-0.866*Age^(-0.134)
longitudinal.age<-0.383*Age^(-.2)
longitudinal.age.min<-0.383*Age^(-.41)

plot(corrected.age~Age, ylim=c(.1,1), ylab="Effect of age coefficient")
lines(modeled.age~Age, type="p",col="red")
lines(longitudinal.age~Age, type="p",col="blue")
lines(longitudinal.age.max~Age, type="p",col="blue")

legend(40,1, legen=c("After Correction", "Original SFT", "Longitudinal"), col=c("black", "red", "blue"), lty = 1)

plot(log10(corrected.age)~(Age),ylim=c(-2,0), ylab="log10(Effect of age coefficient)")
lines(log10(modeled.age)~(Age), type="p",col="red")
lines(log10(longitudinal.age.min)~(Age), type="p",col="blue")
lines(longitudinal.age.max~Age, type="p",col="blue")


