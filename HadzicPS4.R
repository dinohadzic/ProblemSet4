#Dino Hadzic
#R Programming: Problem Set 4

#Creating the function
NetLogo <- function(input){
  NL_File <- scan(file=input, what="", skip=1, n=1, sep=",") #Extracts the name of the file.
  Top_Level <- scan(file=input, what="", skip=2, n=1, sep=",") #Extracts the date and time simulation was run.
  
  #The code below combines the name of the file with the date and time the simulation was run. Then, it splits the element so I may 
  #remove the unecessary informaiton (-0400). Finally, it recombines the name and date and time the simulation was run, removing
  #characters that prevent the creation of a directory.
  Top_Level <- paste(NL_File, Top_Level)
  Top_Level <- strsplit(Top_Level, " ")
  Top_Level <- Top_Level[[1]][-4]
  Top_Level <- paste(Top_Level, collapse=" ")
  Top_Level <- gsub("/", "_", Top_Level)
  Top_Level <- gsub(" ", "_", Top_Level)
  
  dir.create(Top_Level) #Creates the appropriate directory.
  
  #Code below creates appropriate sub-directories, "Globals," "Turtules," and "Plots."
  sapply(c("Globals", "Turtles", "Plots"), function(x) dir.create(file.path(Top_Level, x)))
  sapply(c("PositionPlot", "WinnersPlot", "PolarizationPlot", "IncumbentPercentagePlot"), function(x)
    dir.create(file.path(Top_Level, "Plots", x)))
  
  #Code below extracts the Global names and values. Then, after removing unnecessary characters from the values,
  #it combines the names and values and appropriately names the values.
  Global_Names <- scan(file=input, what="", skip=8, n=-1, nlines=1, sep="," )
  Global_Values <- scan(file=input, what="", skip=9, n=-1, nlines=1, sep=",")
  Global_Values <- gsub("\\[|\\]", "", Global_Values)
  Global_Values <- gsub("^\"|\"", "", Global_Values)
  Globals <- sapply(Global_Values, strsplit, " ")
  names(Globals) <- Global_Names
  
  #Dumps Globals into the direcotry, in the "Globals" folder.
  dump("Globals", file.path(Top_Level, "Globals", "Globals.R"))
  
  #Extracts the Turtles names, and then using an index, removes entries that are blank. Finally, stores all of the Turtles names
  #as Turtles_Names.
  Turtles_Names <- scan(file=input, what="", skip=12, n=-1, nlines=1, sep=",")
  Names_T_F <- Turtles_Names == ""
  Names_Index <- which(Names_T_F == FALSE)
  Turtles_Names <- Turtles_Names[Names_Index]
  
  #First, the code below unlists the appropriate Globals names and then uses the the informaiton provided to calculate the 
  #number of districts, activists, voters, and candidates we will be working with. Also, calculates the total number of observations.
  tots <- unlist(Globals[c("num-districts", "num-activists-per-district", "num-voters-per-district", "n.parties")])
  tots <- as.numeric(tots)
  district_tot <- tots[1]
  activist_tot <- district_tot*tots[2]
  voters_tot <- district_tot*tots[3]
  parties_tot <- tots[4]
  candidates_tot <- district_tot*parties_tot
  sum_totals <- district_tot + activist_tot + voters_tot + parties_tot + candidates_tot
  
  #Extracts Turtle entries and then converts vector into matrix.
  Turtles <- scan(file=input, what="", skip=13, n=-1, nlines=sum_totals, sep=",")
  Turtles <- matrix(Turtles, nrow=sum_totals, byrow=T)
  Turtles <- Turtles[,Names_Index]
  colnames(Turtles) <- Turtles_Names
  
  #The code below extracts the appropriate rows that include district as the breed.  Then,
  #using several repetitions of indexing, it drops columns for which entries are empty. Finally, 
  #writes a .csv file for Districts and stores it the appropriate folder in the directory.
  Indexing_Dist <- seq(1:district_tot)   
  Districts <- Turtles[Indexing_Dist,]
  Indexing_Dist2 <- colSums(apply(Districts, 2, function(x){x == ""})) == 0
  Districts <- Districts[,Indexing_Dist2]
  Indexing_Dist3 <- apply(Districts, 2, function(x){length(unique(x))})
  Indexing_Dist4 <- which(Indexing_Dist3 == 1)
  Districts <- Districts[, -Indexing_Dist4]
  Districts <- gsub("\\]|\\[|^\"|\"$", "", Districts)
  write.csv(Districts, file.path(Top_Level, "Turtles", "Districts.csv"))
  
  #Repeats the process from above but now with voters. The code extracts the appropriate rows that 
  #include voters as the breed.  Then, using several repetitions of indexing, it drops columns for which entries 
  #are empty. Finally, #writes a .csv file for Voters and stores it the appropriate folder in the directory.
  Indexing_Voters <- seq(district_tot + 1, activist_tot + district_tot)
  Voters <- Turtles[Indexing_Voters,]
  Indexing_Voters2 <- colSums(apply(Voters, 2, function(x){x == ""})) == 0
  Voters <- Voters[,Indexing_Voters2]
  Indexing_Voters3 <- apply(Voters, 2, function(x){length(unique(x))})
  Indexing_Voters4 <- which(Indexing_Voters3 == 1)
  Voters <- Voters[, -Indexing_Voters4]
  Voters <- gsub("\\]|\\[|^\"|\"$", "", Voters)
  write.csv(Voters, file.path(Top_Level, "Turtles", "Voters.csv"))
  
  #Repeats the process from above but now with activists. The code extracts the appropriate rows that 
  #include activists as the breed.  Then, using several repetitions of indexing, it drops columns for which entries 
  #are empty. Finally, writes a .csv file for Activists and stores it the appropriate folder in the directory.
  Indexing_Activists <- seq(max(seq(district_tot + 1, activist_tot + district_tot)) + 1, activist_tot*2 + district_tot)
  Activists <- Turtles[Indexing_Activists,]
  Indexing_Activists2 <- colSums(apply(Activists, 2, function(x){x == ""})) == 0
  Activists <- Activists[,Indexing_Activists2]
  Indexing_Activists3 <- apply(Activists, 2, function(x){length(unique(x))})
  Indexing_Activists4 <- which(Indexing_Activists3 == 1)
  Activists <- Activists[, -Indexing_Activists4]
  Activists <- gsub("\\]|\\[|^\"|\"$", "", Activists)
  write.csv(Activists, file.path(Top_Level, "Turtles", "Activists.csv"))
  
  #Repeats the process from above but now with parties The code extracts the appropriate rows that 
  #include parties as the breed.  Then, using several repetitions of indexing, it drops columns for which entries 
  #are empty. Finally, writes a .csv file for Parties and stores it the appropriate folder in the directory.
  p <- max(seq(max(seq(district_tot + 1, activist_tot + district_tot)) + 1, activist_tot*2 + district_tot))
  Indexing_Parties <- seq(p+1, p+parties_tot)
  Parties <- Turtles[Indexing_Parties,]
  Indexing_Parties2 <- colSums(apply(Parties, 2, function(x){x == ""})) == 0
  Parties <- Parties[,Indexing_Parties2]
  Indexing_Parties3 <- apply(Parties, 2, function(x){length(unique(x))})
  Indexing_Parties4 <- which(Indexing_Parties3 == 1)
  Parties <- Parties[, -Indexing_Parties4]
  Parties <- gsub("\\]|\\[|^\"|\"$", "", Parties)
  write.csv(Parties, file.path(Top_Level, "Turtles", "Parties.csv"))
  
  #Repeats the process from above but now with candidates The code extracts the appropriate rows that 
  #include parties as the breed.  Then, using several repetitions of indexing, it drops columns for which entries 
  #are empty. Finally, writes a .csv file for Candidates and stores it the appropriate folder in the directory.
  Indexing_Candidates <- seq(max(Indexing_Parties) + 1, max(Indexing_Parties) + candidates_tot)
  Candidates <- Turtles[Indexing_Candidates,]
  Indexing_Candidates2 <- colSums(apply(Candidates, 2, function(x){x == ""})) == 0
  Candidates <- Candidates[,Indexing_Candidates2]
  Indexing_Candidates3 <- apply(Candidates, 2, function(x){length(unique(x))})
  Indexing_Candidates4 <- which(Indexing_Candidates3 == 1)
  Candidates <- Candidates[, -Indexing_Candidates4]
  Candidates <- gsub("\\]|\\[|^\"|\"$", "", Candidates)
  write.csv(Candidates, file.path(Top_Level, "Turtles", "Candidates.csv"))
  
  #The below code extracts the appropriate values for D1 along with the column headings. It also removes
  #cases where there is a blank entry as well as various punctuation marks.  In matrix form, D1 is then
  #written out as a .csv file to the appropriate directory.  For D2 and D3, I will follow the same process.
  D1.cols <- scan(file=input, skip=8544, nlines=1, what="", sep=",")
  D1.cols <- D1.cols[which(D1.cols != "")]
  D1.cols <- gsub("\\]|\\[|^\"|\"$", "", D1.cols)
  D1 <- scan(file=input, what="", skip=8546, nlines=169, sep=",")
  D1 <- matrix(D1, nrow=169, byrow=T)
  D1 <- gsub("\\]|\\[|^\"|\"$", "", D1)
  D1 <- D1[,c(1:24)]
  D1 <- D1[,c(1,2,6,10,14,18,22)]
  mode(D1) <- "numeric"
  colnames(D1) <- c("X", D1.cols)
  write.csv(D1, file.path(Top_Level, "Plots", "PositionPlot", "D1.csv"))  
  
  #Same process followed as for D1.
  D2.cols <- scan(file=input, skip=8728, nlines=1, what="", sep=",")
  D2.cols <- D2.cols[which(D2.cols != "")]
  D2.cols <- gsub("\\]|\\[|^\"|\"$", "", D2.cols)
  D2 <- scan(file=input, what="", skip=8730, nlines=169, sep=",")
  D2 <- matrix(D2, nrow=169, byrow=T)
  D2 <- gsub("\\]|\\[|^\"|\"$", "", D2)
  D2 <- D2[,c(1:24)] 
  D2 <- D2[,c(1,2,6,10,14,18,22)]
  mode(D2) <- "numeric"
  colnames(D2) <- c("X", D2.cols)
  write.csv(D2, file.path(Top_Level, "Plots", "PositionPlot", "D2.csv"))
  
  #Same process followed as for D1 and D2.
  D3.cols <- scan(file=input, skip=8912, nlines=1, what="", sep=",")
  D3.cols <- D3.cols[which(D3.cols != "")]
  D3.cols <- gsub("\\]|\\[|^\"|\"$", "", D3.cols)
  D3 <- scan(file=input, what="", skip=8914, nlines=169, sep=",")
  D3 <- matrix(D3, nrow=169, byrow=T)
  D3 <- gsub("\\]|\\[|^\"|\"$", "", D3)
  D3 <- D3[,c(1:24)]
  D3 <- D3[,c(1,2,6,10,14,18,22)]
  mode(D3) <- "numeric"
  colnames(D3) <- c("X", D3.cols)
  write.csv(D2, file.path(Top_Level, "Plots", "PositionPlot", "D3.csv")) 
  
  pdf(file=file.path(Top_Level, "Plots", "PositionPlot", "Positioning.pdf")) #Creates the pdf files which will host the polots.
  
  #The code below plots incumbents, activists, and voters along the relevant dimensions for D1.  I use red to indicate Republicans
  #and blue to denote Democrats for all three groups.
  plot(D1[,"X"], D1[,"Red"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="red", main="D1 - Incumbents", ylab="Positioning", xlab="Cycle", type="l")
  par(new=TRUE)
  plot(D1[,"X"], D1[,"Blue"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="blue", main="D1 - Incumbents", ylab="Positioning", xlab="Cycle", type="l")
  plot(D1[,"X"], D1[,"RedActivists"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="red", main="D1 - Activists", ylab="Positioning", xlab="Cycle", type="l")
  par(new=TRUE)
  plot(D1[,"X"], D1[,"BlueActivists"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="blue", main="D1 - Activists", ylab="Positioning", xlab="Cycle", type="l")
  plot(D1[,"X"], D1[,"RedVoters"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="red", main="D1 - Voters", ylab="Positioning", xlab="Cycle", type="l")
  par(new=TRUE)
  plot(D1[,"X"], D1[,"BlueVoters"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="blue", main="D1 - Voters", ylab="Positioning", xlab="Cycle", type="l")
  
  #This code works as the one did above. In this case, I am plotting the three groups for D2 however.
  plot(D2[,"X"], D2[,"Red"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="red", main="D2 - Incumbents", ylab="Positioning", xlab="Cycle", type="l")
  par(new=TRUE)
  plot(D2[,"X"], D2[,"Blue"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="blue", main="", ylab="", xlab="", type="l")
  plot(D2[,"X"], D2[,"RedActivists"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="red", main="D2 - Activists", ylab="Positioning", xlab="Cycle", type="l")
  par(new=TRUE)
  plot(D2[,"X"], D2[,"BlueActivists"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="blue", main="", ylab="", xlab="", type="l")
  plot(D2[,"X"], D2[,"RedVoters"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="red", main="D2 - Voters", ylab="Positioning", xlab="Cycle", type="l")
  par(new=TRUE)
  plot(D2[,"X"], D2[,"BlueVoters"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="blue", main="", ylab="", xlab="", type="l")
  
  #Finally, this code plots the three groups for D3.
  plot(D3[,"X"], D3[,"Red"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="red", main="D3 - Incumbents", ylab="Positioning", xlab="Cycle", type="l")
  par(new=TRUE)
  plot(D3[,"X"], D3[,"Blue"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="blue", main="", ylab="", xlab="", type="l")
  plot(D3[,"X"], D3[,"RedActivists"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="red", main="D3 - Activists", ylab="Positioning", xlab="Cycle", type="l")
  par(new=TRUE)
  plot(D3[,"X"], D3[,"BlueActivists"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="blue", main="", ylab="", xlab="", type="l")
  plot(D3[,"X"], D3[,"RedVoters"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="red", main="D3 - Voters", ylab="Positioning", xlab="Cycle", type="l")
  par(new=TRUE)
  plot(D3[,"X"], D3[,"BlueVoters"], xlim=c(0,168), ylim=c(-10,10), pch=16, col="blue", main="", ylab="", xlab="", type="l")
  
  dev.off() #Turns off the plotting.
  
  #This code below extracts the relevant information for winners (the percentage of candidates from each party
  #who won in a particular cycle). It also the extracted data into matrix form before removing any unnecessary
  #punctuation marks.  Finally, once the matrix has been cleaned up, column names have been added, and the
  #appropriate columns have been selected, the code writes out winners as a .csv file to the appropriate directory.
  winners <- scan(file=input, what="", skip=9140, nlines=169, sep=",")
  winners <- matrix(winners, nrow=169, byrow=T)
  winners <- gsub("\\]|\\[|^\"|\"$", "", winners)
  winners <- winners[,c(1:12)]
  winners <- winners[,c(1, 2, 10)]
  mode(winners) <- "numeric"
  colnames(winners) <- c("Cycle", "Dem", "Rep")
  write.csv(winners, file.path(Top_Level, "Plots", "WinnersPlot", "Winners.csv")) 
  
  pdf(file=file.path(Top_Level, "Plots", "WinnersPlot", "Winners.pdf")) #Turns on pdf plotting device.
  
  #Plots the percentage of candidates from each party who in a particular cycle on the same plot. The red denotes
  #Republicans and the blue Democrats.
  plot(winners[,"Cycle"], winners[,"Dem"], type="l", xlim=c(0,168), ylim=c(30,60), col="blue", xlab="Cycle", ylab="Percentage of Candidates Winning ", main="Dems v. Reps")
  par(new=TRUE)
  plot(winners[,"Cycle"], winners[,"Rep"], type="l", xlim=c(0,168), ylim=c(30,60), col="red", xlab="", ylab="", main="")
  
  dev.off() #Turns off plotting device.
  
  #Same process followed as with winners.
  polarization <- scan(file=input, what="", skip=9321, nlines=169, sep=",")
  polarization <- matrix(polarization, nrow=169, byrow=T)
  polarization <- gsub("\\]|\\[|^\"|\"$", "", polarization)
  polarization <- polarization[,c(1:12)]
  polarization <- polarization[,c(1, 2, 6, 10)]
  mode(polarization) <- "numeric"
  colnames(polarization) <- c("Cycle","Candidate", "Voter", "Activist")
  write.csv(polarization, file.path(Top_Level, "Plots", "PolarizationPlot", "Polarization.csv"))
  
  pdf(file=file.path(Top_Level, "Plots", "PolarizationPlot", "Polarization.pdf")) #Turns on plotting device.
  
  #Plots the mean position of candidates, voters, and activists on the same graph. I use black to represent
  #candidates, violet for voters, and darkgreen for activists. I also add text for presentation purposes.
  plot(polarization[,"Cycle"], polarization[,"Candidate"], type="l", xlim=c(0,168), ylim=c(0,9), 
       col="black", xlab="Cycle", ylab="Mean Position", main="Mean Position - Candidate, Voter, Activist")
  par(new=TRUE)
  plot(polarization[,"Cycle"], polarization[,"Voter"], type="l", xlim=c(0,168), ylim=c(0,9), 
       col="violet", xlab="", ylab="", main="")
  par(new=TRUE)
  plot(polarization[,"Cycle"], polarization[,"Activist"], type="l", xlim=c(0,168), ylim=c(0,9), 
       col="darkgreen", xlab="", ylab="", main="")
  text(125,4,"Candidate")
  text(100,6,"Voter", col="violet")
  text(50,8,"Activist", col="darkgreen")
  
  dev.off() #Turns off plotting device.
  
  #Same process followed as with winners and polarization.
  Incumbents <- scan(file=input, what="", skip=9500, nlines=169, sep=",")
  Incumbents <- matrix(Incumbents, nrow=169, byrow=T)
  Incumbents <- gsub("\\]|\\[|^\"|\"$", "", Incumbents)
  Incumbents <- Incumbents[,c(1,2)]
  mode(Incumbents) <- "numeric"
  colnames(Incumbents) <- c("Cycle", "%_Winning_Incumbents")
  write.csv(Incumbents, file.path(Top_Level, "Plots", "IncumbentPercentagePlot", "Incumbents.csv"))
  
  pdf(file=file.path(Top_Level, "Plots", "IncumbentPercentagePlot", "Incumbents.pdf")) #Turns on plotting device.
  
  #Plots the percentage of candidates from both parties winning re-election in a particular cycle.
  plot(Incumbents[,"Cycle"], Incumbents[,"%_Winning_Incumbents"], type="l", xlim=c(0,168), ylim=c(0,70), ylab="% of Incumbents Winning", xlab="Cycle", main="Winning Incumbents")
  
  dev.off() #Turns off plotting device.
}
NetLogo("NetLogo.csv") #Tests the function in order to ensure that is working correctly.


##JMR Chapter 3, Probelm 3

#First set n equal to whatever value you wish, and then ensure that "number," "square," and "cube" 
#are all numeric vectors of length n.
n <- 7
number <- numeric(n)
square <- numeric(n)
cube <- numeric(n)

#The for loop below runs from 1 to n, and does the appropriate calculations. It then stores those
#calculations as a table (matrix).
for(i in 1:n){
  number[i] <- i
  square[i] <- i^2
  cube[i] <- i^3
  table <- cbind(number, square, cube)
  print(table)
}

##JMR Chapter 3, Problem 4

#mtable is a 9 by 9 matrix filled with zeroes.
mtable <- matrix(ncol=9, nrow=9, 0)

#This for loop takes the i^th row of mtable and then multiplies i (whichever row that may be)
#by a sequence of numbers, 1 through 9.
for(i in 1:9){
  mtable[i,] <- i*seq(1,9)
  show(mtable)
}


##JMR Chapter 7, Problem 3
library(lattice) 
pop <- data.frame(m=rnorm(100, 160, 20), f=rnorm(100, 160, 20))

next.gen <- function(pop){
  pop$m <- sample(pop$m)
  pop$m <- apply(pop, 1, mean)
  pop$f <- pop$m
  return(pop)
}

#Creates an empty element which will then be turned into a data.frame. The for loop collects the male heights by generation.
Height_D <- NULL
for(i in 1:9){
  M_Height <- data.frame(next.gen(pop)$m, rep(i, 100))
  Height_D <- rbind(Height_D, M_Height)
}

colnames(Height_D) <- c("height", "generation") #Assigning column names

#Plots the histogram of male heights by generation.
histogram(~height | generation, data=Height_D, xlab="Male Height", main="Distribution of Male Height by Generation" )

#JMR Chapter 7, Problem 4
#Finally, the code below recreates figure 6.1 from JMR.
library(spuRs)
data(treeg)
xyplot(height.ft ~ age, type="l", data=treeg, group=tree.ID, xlab="age (years)", ylab="height (feet)")
