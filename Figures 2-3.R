setwd("C:/Users/u0113028/OneDrive/Documents/R.Tanious/studies/violin plots/calculations")
library(ggplot2)
library(ggpubr)
library(grid) 
library(readr)

disrbehperc <- read.delim("disruptive behavior percentage.txt", header = TRUE, sep = ",")
disrbehperc$article = as.factor(disrbehperc$article)

A1 <- disrbehperc[disrbehperc$article==1 & disrbehperc$phase=='A', ] #create an object for baseline data points from article 1
A1mean <- mean(A1$score)                                             #calculate the baseline mean for article 1
A1size <- nrow(A1)                                                   #create an object for the number baseline data in article 1

B1 <- disrbehperc[disrbehperc$article==1 & disrbehperc$phase=='B', ] #create an object for intervention data points from article 1
B1mean <- mean(B1$score)                                             #calculate the intervention mean for article 1
B1size <- nrow(B1)                                                   #create an object for the number intervention data in article 1

A2 <- disrbehperc[disrbehperc$article==2 & disrbehperc$phase=='A', ] 
A2mean <- mean(A2$score)
A2size <- nrow(A2)

B2 <- disrbehperc[disrbehperc$article==2 & disrbehperc$phase=='B', ] 
B2mean <- mean(B2$score)
B2size <- nrow(B2)

A4 <- disrbehperc[disrbehperc$article==4 & disrbehperc$phase=='A', ] 
A4mean <- mean(A4$score)
A4size <- nrow(A4)

B4 <- disrbehperc[disrbehperc$article==4 & disrbehperc$phase=='B', ] 
B4mean <- mean(B4$score)
B4size <- nrow(B4)

A6 <- disrbehperc[disrbehperc$article==6 & disrbehperc$phase=='A', ] 
A6mean <- mean(A6$score)
A6size <- nrow(A6)

B6 <- disrbehperc[disrbehperc$article==6 & disrbehperc$phase=='B', ] 
B6mean <- mean(B6$score)
B6size <- nrow(B6)

A7 <- disrbehperc[disrbehperc$article==7 & disrbehperc$phase=='A', ] 
A7mean <- mean(A7$score)
A7size <- nrow(A7)

B7 <- disrbehperc[disrbehperc$article==7 & disrbehperc$phase=='B', ] 
B7mean <- mean(B7$score)
B7size <- nrow(B7)

A9 <- disrbehperc[disrbehperc$article==9 & disrbehperc$phase=='A', ] 
A9mean <- mean(A9$score)
A9size <- nrow(A9)

B9 <- disrbehperc[disrbehperc$article==9 & disrbehperc$phase=='B', ] 
B9mean <- mean(B9$score)
B9size <- nrow(B9)

A10 <- disrbehperc[disrbehperc$article==10 & disrbehperc$phase=='A', ] 
A10mean <- mean(A10$score)
A10size <- nrow(A10)

B10 <- disrbehperc[disrbehperc$article==10 & disrbehperc$phase=='B', ] 
B10mean <- mean(B10$score)
B10size <- nrow(B10)

A12 <- disrbehperc[disrbehperc$article==12 & disrbehperc$phase=='A', ] 
A12mean <- mean(A12$score)
A12size <- nrow(A12)

B12 <- disrbehperc[disrbehperc$article==12 & disrbehperc$phase=='B', ] 
B12mean <- mean(B12$score)
B12size <- nrow(B12)

A16 <- disrbehperc[disrbehperc$article==16 & disrbehperc$phase=='A', ] 
A16mean <- mean(A16$score)
A16size <- nrow(A16)

B16 <- disrbehperc[disrbehperc$article==16 & disrbehperc$phase=='B', ] 
B16mean <- mean(B16$score)
B16size <- nrow(B16)

B17 <- disrbehperc[disrbehperc$article==17 & disrbehperc$phase=='B', ] 
B17mean <- mean(B17$score)
B17size <- nrow(B17)

A21 <- disrbehperc[disrbehperc$article==21 & disrbehperc$phase=='A', ] 
A21mean <- mean(A21$score)
A21size <- nrow(A21)

B21 <- disrbehperc[disrbehperc$article==21 & disrbehperc$phase=='B', ] 
B21mean <- mean(B21$score)
B21size <- nrow(B21)

A22 <- disrbehperc[disrbehperc$article==22 & disrbehperc$phase=='A', ] 
A22mean <- mean(A22$score)
A22size <- nrow(A22)

B22 <- disrbehperc[disrbehperc$article==22 & disrbehperc$phase=='B', ] 
B22mean <- mean(B22$score)
B22size <- nrow(B22)

set.seed(234) #setting the seed to ensure reproducibility
vp3 <- ggplot(disrbehperc, aes(phase,score, colour = article)) + # Create the main violin plot with different colors per study
  geom_violin(width=1.5, color= "blue",size=1.1)+                # setting the width, color, and size of the violin
  geom_jitter(shape=16, position=position_jitter(0.08)) +        # adding the jitter, with shape and position
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y= "Percentage of disruptive behavior", x = "")
print(vp3)

g <- ggplot_build(vp3)        #checking the color scheme assigned by R (needed in next step)
unique(g$data[[2]]["colour"])

vp3 <- vp3 +                                                      #adding the mean dots per study with color and size (according to number of measurements)
  geom_point(aes(x=0.75,y=A1mean),colour="#F8766D", size=2.18)+
  geom_point(aes(x=0.75,y=A2mean),colour="#DE8C00", size=2.78) +
  geom_point(aes(x=0.75,y=A4mean),colour="#B79F00", size=2.16) +
  geom_point(aes(x=0.75,y=A6mean),colour="#7CAE00", size=2.94) +
  geom_point(aes(x=0.75,y=A7mean),colour="#00BA38", size=2.58) +
  geom_point(aes(x=0.75,y=A9mean),colour="#00C08B", size=2.10) +
  geom_point(aes(x=0.75,y=A10mean),colour="#00BFC4", size=2.56)+
  geom_point(aes(x=0.75,y=A12mean),colour="#00B4F0", size=2.50)+
  geom_point(aes(x=0.75,y=A16mean),colour="#619CFF", size=2.26)+
  geom_point(aes(x=0.75,y=A21mean),colour="#F564E3", size=3.54)+
  geom_point(aes(x=0.75,y=A22mean),colour="#FF64B0", size=2.12)+
  geom_point(aes(x=2.25,y=B1mean),colour="#F8766D", size=2.50) +
  geom_point(aes(x=2.25,y=B2mean),colour="#DE8C00", size=3.26) +
  geom_point(aes(x=2.25,y=B4mean),colour="#B79F00", size=2.16) +
  geom_point(aes(x=2.25,y=B6mean),colour="#7CAE00", size=3.26) +
  geom_point(aes(x=2.25,y=B7mean),colour="#00BA38", size=2.54) +
  geom_point(aes(x=2.25,y=B9mean),colour="#00C08B", size=2.42) +
  geom_point(aes(x=2.25,y=B10mean),colour="#00BFC4", size=2.52)+
  geom_point(aes(x=2.25,y=B12mean),colour="#00B4F0", size=3.18)+
  geom_point(aes(x=2.25,y=B16mean),colour="#619CFF", size=2.34)+
  geom_point(aes(x=2.25,y=B17mean),colour="#C77CFF", size=2.92)+
  geom_point(aes(x=2.25,y=B21mean),colour="#F564E3", size=3.66)+
  geom_point(aes(x=2.25,y=B22mean),colour="#FF64B0", size=2.28)
print(vp3)
                              
vp3 <- vp3 +                                        #adding boxplot to the graph
  geom_boxplot(width=0.03, color="red", size=1)     #setting width, color, and size of the boxplot
print(vp3)

nrow(subset(disrbehperc,disrbehperc$phase=="A"))  #total number of baseline measures
nrow(subset(disrbehperc,disrbehperc$phase=="B"))  #total number of intervention measures

summary(subset(disrbehperc,disrbehperc$phase=="A")) #summary statistics for baseline data
summary(subset(disrbehperc,disrbehperc$phase=="B")) #summary statistics for intervention data

####Below same steps for Figure 3####
ontaskperc <- read.delim("on-task percentage.txt", header = TRUE, sep = ",")
ontaskperc$article <- as.factor(ontaskperc$article)

Aon6 <- ontaskperc[ontaskperc$article==6 & ontaskperc$phase=='A', ] 
Aon6mean <- mean(Aon6$score)
Aon6size <- nrow(Aon6)

Bon6 <- ontaskperc[ontaskperc$article==6 & ontaskperc$phase=='B', ] 
Bon6mean <- mean(Bon6$score)
Bon6size <- nrow(Bon6)

Aon8 <- ontaskperc[ontaskperc$article==8 & ontaskperc$phase=='A', ] 
Aon8mean <- mean(Aon8$score)
Aon8size <- nrow(Aon8)

Bon8 <- ontaskperc[ontaskperc$article==8 & ontaskperc$phase=='B', ] 
Bon8mean <- mean(Bon8$score)
Bon8size <- nrow(Bon8)

Aon14 <- ontaskperc[ontaskperc$article==14 & ontaskperc$phase=='A', ] 
Aon14mean <- mean(Aon14$score)
Aon14size <- nrow(Aon14)

Bon14 <- ontaskperc[ontaskperc$article==14 & ontaskperc$phase=='B', ] 
Bon14mean <- mean(Bon14$score)
Bon14size <- nrow(Bon14)

Aon16 <- ontaskperc[ontaskperc$article==16 & ontaskperc$phase=='A', ] 
Aon16mean <- mean(Aon16$score)
Aon16size <- nrow(Aon16)

Bon16 <- ontaskperc[ontaskperc$article==16 & ontaskperc$phase=='B', ] 
Bon16mean <- mean(Bon16$score)
Bon16size <- nrow(Bon16)

Bon17 <- ontaskperc[ontaskperc$article==17 & ontaskperc$phase=='B', ] 
Bon17mean <- mean(Bon17$score)
Bon17size <- nrow(Bon17)

Bon19 <- ontaskperc[ontaskperc$article==19 & ontaskperc$phase=='B', ] 
Bon19mean <- mean(Bon19$score)
Bon19size <- nrow(Bon19)

Aon20 <- ontaskperc[ontaskperc$article==20 & ontaskperc$phase=='A', ] 
Aon20mean <- mean(Aon20$score)
Aon20size <- nrow(Aon20)

Bon20 <- ontaskperc[ontaskperc$article==20 & ontaskperc$phase=='B', ] 
Bon20mean <- mean(Bon20$score)
Bon20size <- nrow(Bon20)

Aon21 <- ontaskperc[ontaskperc$article==21 & ontaskperc$phase=='A', ] 
Aon21mean <- mean(Aon21$score)
Aon21size <- nrow(Aon21)

Bon21 <- ontaskperc[ontaskperc$article==21 & ontaskperc$phase=='B', ] 
Bon21mean <- mean(Bon21$score)
Bon21size <- nrow(Bon21)

Aon22 <- ontaskperc[ontaskperc$article==22 & ontaskperc$phase=='A', ] 
Aon22mean <- mean(Aon22$score)
Aon22size <- nrow(Aon22)

Bon22 <- ontaskperc[ontaskperc$article==22 & ontaskperc$phase=='B', ] 
Bon22mean <- mean(Bon22$score)
Bon22size <- nrow(Bon22)

set.seed(123)
vp4 <- ggplot(ontaskperc, aes(phase,score,colour = article)) + 
  geom_violin(width=1.5, color= "blue",size=1.1)+
  geom_jitter(shape=16, position=position_jitter(0.08)) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y= "Percentage on-task", x = "")
print(vp4)

vp4 <- vp4 +
  geom_point(aes(x=0.75,y=Aon6mean),colour="#F8766D", size=2.94)+
  geom_point(aes(x=0.75,y=Aon8mean),colour="#D39200", size=2.28)+
  geom_point(aes(x=0.75,y=Aon14mean),colour="#93AA00", size=2.74)+
  geom_point(aes(x=0.75,y=Aon16mean),colour="#00BA38", size=2.12)+
  geom_point(aes(x=0.75,y=Aon20mean),colour="#DB72FB", size=2.06)+
  geom_point(aes(x=0.75,y=Aon21mean),colour="#619CFF", size=3.46)+
  geom_point(aes(x=0.75,y=Aon22mean),colour="#FF61C3", size=2.12)+ 
  geom_point(aes(x=2.25,y=Bon6mean),colour="#F8766D", size=3.16)+
  geom_point(aes(x=2.25,y=Bon8mean),colour="#D39200", size=2.44)+
  geom_point(aes(x=2.25,y=Bon14mean),colour="#93AA00", size=3.06)+
  geom_point(aes(x=2.25,y=Bon16mean),colour="#00BA38", size=2.42)+
  geom_point(aes(x=2.25,y=Bon17mean),colour="#00C19F", size=2.94)+
  geom_point(aes(x=2.25,y=Bon19mean),colour="#00B9E3", size=2.10)+
  geom_point(aes(x=2.25,y=Bon20mean),colour="#DB72FB", size=2.24)+
  geom_point(aes(x=2.25,y=Bon21mean),colour="#619CFF", size=3.10)+
  geom_point(aes(x=2.25,y=Bon22mean),colour="#FF61C3", size=2.28)
print(vp4)

  
vp4 <- vp4 +
  geom_boxplot(width=0.03, color="red", size=1)
print(vp4)

h <- ggplot_build(vp4)
unique(h$data[[2]]["colour"])

nrow(subset(ontaskperc,ontaskperc$phase=="A"))
nrow(subset(ontaskperc,ontaskperc$phase=="B"))

summary(subset(ontaskperc,ontaskperc$phase=="A"))
summary(subset(ontaskperc,ontaskperc$phase=="B"))


