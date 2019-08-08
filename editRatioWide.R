# editRatioWide version 1.1 (08 Aguust 2019)--an R software script for
#automated phonetic transcription analysis by Kevin T Cunningham. Copyright (C)
#2019. The University of North Carolina at Chapel Hill.
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. AMDG. 

#This script calculates the edit distance ratio (Smith, Cunningham, & Haley 2019) for a
#broad level phonemic transcript where each word is transcribed in wide form in CSV format. Please see accompanying data file (toydata.txt) for an example
#It calculates this ratio for all the .txt files in the working directory and outputs a single data file called "data.csv"
datafinal<-{}
fileNames = dir(pattern = ".txt")
for (fileName in fileNames){
df <- read.csv(fileName,header = FALSE,stringsAsFactors = FALSE)

#This converts characters in Klattese that have 2 symbols for 1 segment into 1 symbol
df <- df[!apply(is.na(df) | df == "", 1, all),]
df[df=="ar"]<-"1"
df[df=="|"]<-"2"
df[df=="c"]<-"3"
df[df=="^"]<-"4"
df[df=="cr"]<-"5"
df[df=="or"]<-"6"
df[df=="@r"]<-"7"
df[df=="Er"]<-"8"
df[df=="Ir"]<-"9"
df[df=="Ur"]<-"10"
df[df=="Yr"]<-"11"
df[df=="Wr"]<-"12"
df[df=="aI"]<-"13"
df[df=="aU"]<-"14"
df[df=="oU"]<-"15"
df[df=="cI"]<-"16"
df[df=="ar"]<-"17"
df.new <- cbind(df[,2], df[,3])
df.new<-gsub(" ", "", df.new, fixed=TRUE)
numerator<-{}

#get edit distance by computing number of in  each edit class and then summing them
for (i in 1:nrow(df.new)){
  target<-df.new[i,1]
  production<-df.new[i,2]
   target <- paste(target, collapse = "")
    production <-paste(production, collapse = "")
    ld <- drop(attr(adist(target, production, counts = TRUE), "counts"))
    ldwrite <- sum(ld)
    numerator <- append(numerator, ldwrite)
  }
 numerator <- sum(numerator)
 
#get number of target segments 
targetsegments <- sum(nchar(df.new[,1]))

#get number of produced segments
producedsegments <- sum(nchar(df.new[,2]))
writedata<-{}
#get phonemic edit distance ratio and write to file
if(producedsegments>targetsegments){
  editdistance <- (numerator/producedsegments)
  writedata <- append(writedata,editdistance)
  #write.table(editdistance, file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
}

if(targetsegments>=producedsegments){
  editdistance <- numerator/targetsegments
  writedata <- append(writedata,editdistance)
}
final <- c(fileName, writedata)
datafinal<- rbind(datafinal, final)
}
write.csv(datafinal, file = "data.csv")
# editRatioWide: An R software script for automated phonetic analysis (c) 2019 by Kevin T Cunningham. AMDG.
#  This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License.
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <https://www.gnu.org/licenses/>. 
#fiat mihi secundum verbum tuum
