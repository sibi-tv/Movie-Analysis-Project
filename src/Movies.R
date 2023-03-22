movies <- read.csv("Movies2022F-4.csv")
install.packages(RColorBrewer)
library(RColorBrewer)
install.packages("devtools")
devtools::install_github("devanshagr/PermutationTestSecond")
devtools::install_github("devanshagr/PermutationTestManual")


#Hypothesis 1
moviesR = subset(movies, content == "R")
boxplot(imdb_score~genre, data=moviesR, xlab="Movie Genre", ylab="IMDB score", main = "Boxplot: IMDB scores 
        of R-rated movies in different genres", col=brewer.pal(6, "Set1") )
tapply(moviesR$imdb_score, moviesR$genre, mean)


#Hypothesis 2
sciFiHi = subset(movies, movies$genre == "Sci-Fi" & movies$Budget == "High");
sciFiMed = subset(movies, movies$genre == "Sci-Fi" & movies$Budget == "Medium");
sciFiLo = subset(movies, movies$genre == "Sci-Fi" & movies$Budget == "Low");

mean(sciFiHi$imdb_score)
mean(sciFiMed$imdb_score)
mean(sciFiLo$imdb_score)

#Hypothesis 3
familyHighGross = subset(movies, movies$genre == "History" & movies$Gross == "High");
comedyHighGross = subset(movies, movies$genre == "Comedy" & movies$Gross == "High");
mean(familyHighGross$imdb_score)
mean(comedyHighGross$imdb_score)

############################################# PART B #######################################################
histmov = subset(movies, movies$genre == "Comedy")
h <- nrow(histmov)
m1 <- mean(histmov$imdb_score)
s1 <- sd(histmov$imdb_score)
comov = subset(movies, movies$genre == "Action")
c <- nrow(comov)
m2 <- mean(comov$imdb_score)
s2 <- sd(histmov$imdb_score)

Val1 <- rnorm(1000, mean=m1, sd=s1)
Val2 <- rnorm(1000, mean=m2, sd=s2)
Val <- c(Val1,Val2)

Cat1 <- rep("Comedy", 1000)
Cat2 <- rep("Action", 1000)
Cat <- c(Cat1,Cat2)

d <- data.frame(Cat, Val)

PermutationTestSecond::Permutation(d, "Cat", "Val", 1000, "Comedy", "Action")

Observed_Difference<-mean(d[d$Cat=="Action",2])-mean(d[d$Cat=="Comedy",2])
Observed_Difference


