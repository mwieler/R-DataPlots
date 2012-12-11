#0 INPUT PARAMETERS
library(sqldf)
setwd("/users/matt/") #set the directory where the raw data input file is stored
datafile="sample_data" #this is the name of the input CSV file stored in the directory specified above.  This file is produced directly by the flourescence spectroscopy machine. 

pep_v0 = 1.5*10^-3 #initial peptide volume, liters (L)
pep_c0 = 20*10^-9 #initial peptide concentration, moles (M)
pep_m0 = 3*10^-11

RNA_m = 3*10^-11 #mol RNA added per trial
RNA_v = 1*10^-6 # volume of RNA solution (L) added in each subsequent trial
#RNA_c = # RNA solution concentration, M

lambda = 590 # a subjective choice made by the user based on the most common lambda max across trials (in theory, the lambda max should remain roughly constant as RNA concentration is increased.  In practice, it moves around a bit.

#1 INPUT DATA FROM CSV FILE
i<-read.table(paste(datafile,".csv",sep=""),header=TRUE,sep=",",, blank.lines.skip = TRUE)

i1<-as.data.frame(apply(i,2,which.max)) # returns the row number of each column's maximum
table(i1) #creates a frequency table of the row

#2 SHOW LAMBDA MAX FOR EACH TRIAL
i1<-as.data.frame(apply(i,2,max)) #create a matrix that has the maximum values from each column
colnames(i1)<-c("b") #rename the columns with shorter names

#3 GRAPH INTENSITY VS [RNA] LAMBDA DEFINED ABOVE
j<-subset(i, wavelength==lambda)
k<-t(j)
l<-k[-1,] #remove the first row of the matrix, because that contains the wavelength
trials<-seq(0,nrow(k)-1,by=1) #each row is a different trial.  This numbers the trials; the first trial is trial 0

vol<-pep_v0 + RNA_v*trials
pep_M<-pep_m0/vol
RNA_M<-RNA_m*trials/vol
l<-cbind(l,pep_M,RNA_M)
plot(l[,3]*10^9,l[,1],main=paste('Emission Intensity vs RNA Concentration (',lambda,'nm excitation)',sep=""),xlab='[RNA] (nM)',ylab='Emission Intensity (AU)')

