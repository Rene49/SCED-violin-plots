library(tseries)
library(ggplot2)
library(ggpubr)
library(grid) 
library(readr)

#Koegel: Tony Clinic
data <- as.vector(c(66.4,51.2,46.4,73.6,86.4,46.4,74.4,43.2,44.8,61.6,64,46.4,82.4,95.2,69.6,79.2,75.2,86.4,80.8,77.6,72.8,85.6,71.2,74.4,76.8,68,65.6,77.6,91.2,88.8,72,93.6,79.2,69.6,64.8,92,88.8,92,94.4,83.2,93.6,93.6,85.6,86.4))
dataset <- as.data.frame(data)
Groups <- c(rep("A",12),rep("B",32))
dataset <- cbind(dataset,Groups)


#Koegel: Tony All
data <- as.vector(c(66.4,51.2,46.4,73.6,86.4,46.4,74.4,43.2,44.8,61.6,64,46.4,
                    80, 34.97,56.41,15.15,46.11,35.01,50.1, 60.44,29.5,39.83,50.16,41.44,46.21,35.91,35.92,50.22,64.51,41.51,64.54,45.5,74.88,65.37,60.61,60.63,94.76,80.49,51.13,66.23,96.4,80.54,55.94,70.24,40.09,
                    44.84 ,31.27 ,70.63,55.5,65.97,52.12 ,31.34 ,32.16 ,20.98,
                    82.4,95.2,69.6,79.2,75.2,86.4,80.8,77.6,72.8,85.6,71.2,74.4,76.8,68,65.6,77.6,91.2,88.8,72,93.6,79.2,69.6,64.8,92,88.8,92,94.4,83.2,93.6,93.6,85.6,86.4,
                    81.37,65.51,70.28,100,    85.43,85.44,100,89.44,85.48,100,    100,      95.94, 
                    81.17 ,91.59 ,102.81 , 102.06 ,91.68 ,82.1 ,82.12 ,102.15 ,101.37 , 102.22 ,71.04 ,91.06 ,103.08 ,82.31 ,101.53 ,62.36 ,92.78 ,92.8 ,71.22 ,93.65 ,93.67
))
dataset <- as.data.frame(data)
Groups <- c(rep("A",(12+33+9)),rep("B",(32+12+21)))
dataset <- cbind(dataset,Groups)
context <- c(rep("blue",12),rep("green",33),rep("black",9),
             rep("blue",32),rep("green",12),rep("black",21))
dataset2 <- cbind(dataset,context)



vp <- ggplot(dataset, aes(Groups,data)) + 
  geom_violin() +
  ylim(0,100)
print(vp)

vp <- vp + geom_boxplot(width=0.2, color="red", outlier.shape=NA)
print(vp)

vp <- vp + geom_jitter(shape=16, position=position_jitter(0.05), aes(col=context)) + #  #setting the jitter position
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.background = element_blank(), legend.box.background = element_blank()) +
  labs(y= "Percentage of appropriate verbal responses", x = "") +
  scale_color_manual(labels=c("Clinic","Community","School"),values=c("blue","green","black"))
print(vp)



vp2 <- ggplot(dataset, aes(Groups,data)) + 
  geom_violin()+
  ylim(0,100) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y= "Percentage of appropriate verbal responses", x = "")
print(vp2)

ggarrange(vp2, vp, ncol = 1, nrow = 2)




#############3

# Time series plot


datosnum <- read.table(file.choose(),header=T,sep="\t")
datosnum2 <- read.table(file.choose(),header=T,sep="\t")
datosnum3 <- read.table(file.choose(),header=T,sep="\t")


minplot <- 0
maxplot <- 100

indep <- 1:length(data)

par(mfrow=c(3,1))

plot(datosnum$Time,datosnum$Score,
     pch=16,
     ylim=c(minplot,maxplot),
     #xlim=c(1,max(datosnum$Time[datosnum$Tier==i & datosnum$Id==j])),
     xlim=c(1,76),
     #ylab="DV",xlab=paste("Case",i,". Sessions"))
     ylab="% appropriate verbal response",xlab=paste("Sessions in clinical setting"),
     axes=FALSE)
axis(1,seq(1,76,5))
axis(2,seq(0,100,25),cex.axis=0.9)
abline(v=(max(datosnum$Time[datosnum$Phase==0]+0.5)))
lines(datosnum$Time[datosnum$Phase==0],
      datosnum$Score[datosnum$Phase==0])
lines(datosnum$Time[datosnum$Phase==1],
      datosnum$Score[datosnum$Phase==1])

arrows(datosnum$Time[8],datosnum$Score[8],
       datosnum$Time[10],datosnum$Score[10],
       length=0)

arrows(datosnum$Time[11],datosnum$Score[11],
       datosnum$Time[13],datosnum$Score[13],
       length=0)

arrows(datosnum$Time[31],datosnum$Score[31],
       datosnum$Time[33],datosnum$Score[33],
       length=0)


arrows(datosnum$Time[40],datosnum$Score[40],
       datosnum$Time[44],datosnum$Score[44],
       length=0)

arrows(datosnum$Time[48],datosnum$Score[48],
       datosnum$Time[50],datosnum$Score[50],
       length=0)

arrows(datosnum$Time[50],datosnum$Score[50],
       datosnum$Time[52],datosnum$Score[52],
       length=0)

arrows(datosnum$Time[52],datosnum$Score[52],
       datosnum$Time[54],datosnum$Score[54],
       length=0)


arrows(datosnum$Time[54],datosnum$Score[54],
       datosnum$Time[61],datosnum$Score[61],
       length=0)


text(8,115,"Baseline", xpd=NA)
text(40,115,"Intervention", xpd=NA)

#------------------------------------------------------

plot(datosnum2$Time,datosnum2$Score,
     pch=16,
     ylim=c(minplot,maxplot),
     #xlim=c(1,max(datosnum$Time[datosnum$Tier==i & datosnum$Id==j])),
     xlim=c(1,76),
     #ylab="DV",xlab=paste("Case",i,". Sessions"))
     ylab="% appropriate verbal response",xlab=paste("Sessions in community setting"),
     axes=FALSE)
axis(1,seq(1,76,5))
axis(2,seq(0,100,25),cex.axis=0.9)
abline(v=(max(datosnum2$Time[datosnum2$Phase==0]+0.5)))
lines(datosnum2$Time[datosnum2$Phase==0],
      datosnum2$Score[datosnum2$Phase==0])
lines(datosnum2$Time[datosnum2$Phase==1],
      datosnum2$Score[datosnum2$Phase==1])


arrows(datosnum2$Time[1],datosnum2$Score[1],
       datosnum2$Time[3],datosnum2$Score[3],
       length=0)

arrows(datosnum2$Time[8],datosnum2$Score[8],
       datosnum2$Time[10],datosnum2$Score[10],
       length=0)

arrows(datosnum2$Time[39],datosnum2$Score[39],
       datosnum2$Time[44],datosnum2$Score[44],
       length=0)

arrows(datosnum2$Time[49],datosnum2$Score[49],
       datosnum2$Time[54],datosnum2$Score[54],
       length=0)

arrows(datosnum2$Time[54],datosnum2$Score[54],
       datosnum2$Time[61],datosnum2$Score[61],
       length=0)

text(21,115,"Baseline", xpd=NA)
text(51,115,"Intervention", xpd=NA)
#------------------------------------------------------

plot(datosnum3$Time,datosnum3$Score,
     pch=16,
     ylim=c(minplot,maxplot),
     #xlim=c(1,max(datosnum$Time[datosnum$Tier==i & datosnum$Id==j])),
     xlim=c(1,76),
     #ylab="DV",xlab=paste("Case",i,". Sessions"))
     ylab="% appropriate verbal response",xlab=paste("Sessions in school setting"),
     axes=FALSE)
axis(1,seq(1,76,5))
axis(2,seq(0,100,25),cex.axis=0.9)
abline(v=(max(datosnum3$Time[datosnum3$Phase==0]+0.5)))
lines(datosnum3$Time[datosnum3$Phase==0],
      datosnum3$Score[datosnum3$Phase==0])
lines(datosnum3$Time[datosnum3$Phase==1],
      datosnum3$Score[datosnum3$Phase==1])


arrows(datosnum3$Time[2],datosnum3$Score[2],
       datosnum3$Time[9],datosnum3$Score[9],
       length=0)

arrows(datosnum3$Time[9],datosnum3$Score[9],
       datosnum3$Time[12],datosnum3$Score[12],
       length=0)

arrows(datosnum3$Time[12],datosnum3$Score[12],
       datosnum3$Time[15],datosnum3$Score[15],
       length=0)

arrows(datosnum3$Time[15],datosnum3$Score[15],
       datosnum3$Time[39],datosnum3$Score[39],
       length=0)

arrows(datosnum3$Time[42],datosnum3$Score[42],
       datosnum3$Time[50],datosnum3$Score[50],
       length=0)

arrows(datosnum3$Time[52],datosnum3$Score[52],
       datosnum3$Time[54],datosnum3$Score[54],
       length=0)

arrows(datosnum3$Time[59],datosnum3$Score[59],
       datosnum3$Time[61],datosnum3$Score[61],
       length=0)

arrows(datosnum3$Time[42],datosnum3$Score[42],
       datosnum3$Time[50],datosnum3$Score[50],
       length=0,col="white")

text(31,115,"Baseline", xpd=NA)
text(61,115,"Intervention", xpd=NA)