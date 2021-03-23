library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape2)

setwd("D:/M1 MLSD/DATA1/Projet")
aggregation = data.frame(read.delim("./datasets/Aggregation.txt", header = FALSE))
aggregation$V3 = factor(aggregation$V3)

# Récupérer la matrice des features
X = data.frame(aggregation[,1:2])

# Centrer et réduire X
X_scaled = scale(X)

# Récupérer la classe target
target = aggregation[,3]


################################# ANALYSE UNIVARIEE
# Summary
summary(aggregation)

# Equilibre des Classes
classes_count = data.frame(table(aggregation$V3))

#bar
ggplot(classes_count, aes(x=Var1,y=Freq, fill = levels(aggregation$V3)))+
  geom_bar(stat="identity", alpha=0.4)+
  labs(fill="Classes", y="Effectifs", x="Classes")

#pie
ggplot(classes_count, aes(x="",y=Freq, fill = levels(aggregation$V3))) +
  geom_bar(stat="identity", width=1, alpha=0.5) +
  coord_polar("y", start=0)+
  labs(fill="Classes", x="", y="")

# Boxplot
boxplot(X)

# Histogrammes 
#V1
ggplot(aggregation, aes(x=V1)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Histogramme de V1") +
  theme(
    plot.title = element_text(size=15)
  )

ggplot(aggregation, aes(x=V1, fill="red")) +
  geom_density(alpha=0.4)+
  geom_vline(aes(xintercept=mean(V1)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("Densité de la distribution de V1")


#V2
ggplot(aggregation, aes(x=V2)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Histogramme de V2") +
  theme(
    plot.title = element_text(size=15)
  )

ggplot(aggregation, aes(x=V2, fill="red")) +
  geom_density(alpha=0.4)+
  geom_vline(aes(xintercept=mean(V2)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("Densité de la distribution de V2")


# Moyennes des variables par classe
mu_V1 <- ddply(aggregation, "V3", summarise, grp.mean=mean(V1))
mu_V2 <- ddply(aggregation, "V3", summarise, grp.mean=mean(V2))


################################# ANALYSE BIVARIEE

# Densités par classe par variable
# Ajouter les moyennes
h1 <-ggplot(aggregation, aes(x=V1, fill=V3)) +
  geom_density(alpha=0.4)+
  geom_vline(data=mu_V1, aes(xintercept=grp.mean, color=V3),
             linetype="dashed")
  #+ggtitle("Densité de la distribution de V1 par classe")

h2<-ggplot(aggregation, aes(x=V2, fill=V3)) +
  geom_density(alpha=0.4)+
  geom_vline(data=mu_V2, aes(xintercept=grp.mean, color=V3),
             linetype="dashed")+
  #ggtitle("Densité de la distribution de V2 par classe")+
  coord_flip()

nuage <- ggplot(aggregation, aes(x=V1, y=V2)) +
         geom_point(aes(color=V3))

empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(), 
        panel.background=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),           
        axis.title.x=element_blank(), axis.title.y=element_blank())

grid.arrange(h1, empty, nuage, h2, ncol=2, nrow=2, widths=c(4, 2), heights=c(2, 4))

# Boxplot des variables en regroupant par classe

#V1
qplot(x=aggregation$V3, y = aggregation$V1, 
      xlab = "Classes", ylab = "V1",
      geom=c("boxplot", "jitter"), fill=aggregation$V3, alpha=0.01) +
  theme(legend.title=element_blank())

#V2
qplot(x=aggregation$V3, y = aggregation$V2, 
      xlab = "Classes", ylab = "V2",
      geom=c("boxplot", "jitter"), fill=aggregation$V3, alpha=0.01) +
  theme(legend.title=element_blank())


library(ggpubr)
# Test de Normalité des classes :
qq_v1_1=ggqqplot(aggregation[aggregation$V3 == 1,]$V1)

qq_v1_2=ggqqplot(aggregation[aggregation$V3 == 2,]$V1)
qq_v1_3=ggqqplot(aggregation[aggregation$V3 == 3,]$V1)
qq_v1_4=ggqqplot(aggregation[aggregation$V3 == 4,]$V1)
qq_v1_5=ggqqplot(aggregation[aggregation$V3 == 5,]$V1)
qq_v1_6=ggqqplot(aggregation[aggregation$V3 == 6,]$V1)
qq_v1_7=ggqqplot(aggregation[aggregation$V3 == 7,]$V1)

grid.arrange(qq_v1_1,qq_v1_2,qq_v1_3,
             qq_v1_4,qq_v1_5,qq_v1_6,
             qq_v1_7, empty, empty,
             ncol=3, nrow=3, 
             widths=c(2,2,2), heights=c(2, 2, 2), 
             top="QQ-Plot de V1 par classe")

qq_v2_1=ggqqplot(aggregation[aggregation$V3 == 1,]$V1)

qq_v2_2=ggqqplot(aggregation[aggregation$V3 == 2,]$V2)
qq_v2_3=ggqqplot(aggregation[aggregation$V3 == 3,]$V2)
qq_v2_4=ggqqplot(aggregation[aggregation$V3 == 4,]$V2)
qq_v2_5=ggqqplot(aggregation[aggregation$V3 == 5,]$V2)
qq_v2_6=ggqqplot(aggregation[aggregation$V3 == 6,]$V2)
qq_v2_7=ggqqplot(aggregation[aggregation$V3 == 7,]$V2)

grid.arrange(qq_v2_1,qq_v2_2,qq_v2_3,
             qq_v2_4,qq_v2_5,qq_v2_6,
             qq_v2_7, empty, empty, 
             ncol=3, nrow=3, 
             widths=c(2,2,2), heights=c(2, 2, 2), 
             top="QQ-Plot de V2 par classe")

shapiro.test(aggregation[aggregation$V3 == 7,]$V1)
library(corrplot)
cor_m = cor(aggregation[1:2])
corrplot(cor_m, method="color", title = "Matrice des corrélations")
