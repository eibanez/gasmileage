# summary.r: Functions used in the summary section

require("ggplot2")

# Read data
mpg_full <- read.csv("mpg.csv")


# Get data for pure electric vehicles
summary(mpg_full[mpg_full$fuel == "Electricity", ])


# Summary and standard deviation of rest of vehicles
mpg <- mpg_full[mpg_full$fuel != "Electricity", ]
mpg$trans <- factor(mpg$trans)
summary(mpg)

sd(mpg$mpg)
sd(mpg$mpgcity)
sd(mpg$mpghwy)
sd(mpg$engine)
sd(mpg$valves)
sd(mpg$passenger)
sd(mpg$cargo)


# Plot mean MPG by vehicle class
sizes <- c("Subcompact", "Compact", "Midsize", "SUVs", "Std. Pickup", "Special")
mpgsize <- NULL
for(i in sizes) {
  mpgsize <- rbind(mpgsize, cbind.data.frame(i, mean(mpg$mpgcity[mpg$class == i]), mean(mpg$mpghwy[mpg$class == i])) )
}
colnames(mpgsize) <- c("size","City MPG","Highway MPG")
mpgsize2 <- melt(mpgsize, id="size")

qplot(size, value, data=mpgsize2, geom="line", xlab="Vehicle class", ylab="Performance (mpg)", group=variable, linetype=variable, size=I(1)) +
    scale_linetype("")


# Plot densities for city and highway MPG
dens <- cbind(mpg$mpg, 1)
dens <- rbind.data.frame( dens, cbind(mpg$mpgcity, 2) )
dens <- rbind.data.frame( dens, cbind(mpg$mpghwy, 3) )
colnames(dens) <- c("mpg", "type")
dens$type <- factor(dens$type)
levels(dens$type) <- c("Combined", "City MPG", "Highway MPG")
qplot(mpg,data=dens,geom="density",xlab="Performance (mpg)",ylab="Density",group=type,linetype=type,size=I(0.8)) +
    scale_linetype("")


# Plot by fuel type
mpg2 <- mpg[mpg$fuel!="LPG",]
qplot(jitter(mpgcity),jitter(mpghwy),data=mpg2,geom="point",xlab="City MPG (jittered)",ylab="Highway MPG (jittered)",size=I(0.01),asp=1)

qplot(mpgcity,mpghwy,data=mpg,geom="point",xlab="City MPG",ylab="Highway MPG",group=fuel,linetype=fuel,alpha=I(0),size=I(0.01),asp=1) +
    geom_smooth(se = FALSE, size=0.8, colour="black")


# Speeds
mpg$speeds2 <- mpg$speeds
mpg$speeds2[mpg$speeds == 0] <- "Variable"

qplot(speeds2, mpg, data=mpg, geom="boxplot", facets=.~trans,
      xlab="Speeds and transmission", ylab="Combined MPG")


# Drive and transmission
qplot(trans,mpg,data=mpg,geom="boxplot",xlab="Drive and transmission",ylab="Combined MPG",facets=.~drive)


# Engine size
mpg$invengine <- 1/mpg$engine
qplot(jitter(engine),jitter(mpg),data=mpg,geom="point",ylab="Combined MPG (jittered)",xlab="Engine size in liters (jittered)",size=I(1)) +
    geom_smooth(se = FALSE, size=0.8, colour="black")

qplot(jitter(invengine),jitter(mpg),data=mpg,geom="point",ylab="Combined MPG (jittered)",xlab="Engine size inverse (jittered)",size=I(1)) +
    geom_smooth(se = FALSE, size=0.5, colour="black")


# Cylinders vs engine
qplot(jitter(valves),engine,data=mpg,geom="point",ylab="Engine size in liters",xlab="Cylinders (jittered)",size=I(1)) +
     geom_smooth(method="lm", se = FALSE, size=0.5, colour="black")

qplot(valves,mpg,data=mpg,geom="boxplot",group=valves,xlab="Cylinders",ylab="Combined MPG")


# Evolution in time
cars <- c("Two Seaters","Minicompact","Subcompact", "Compact", "Midsize",
          "Large","Small Station", "Midsize Station")
mpg3 <- mpg
mpg3$class <- factor(mpg3$class, levels=cars)
mpg3 <- mpg3[!is.na(mpg3$class), ]
mpg3$year2 <- 1984 + 3*floor((mpg3$year - 1984)/3)

qplot(paste(year2, year2+2, sep="-"), mpg, data=mpg3, geom="boxplot",
  xlab="Year", ylab="Combined MPG", facets=~class) +
  opts(axis.text.x=theme_text(angle=-90, hjust=0))
