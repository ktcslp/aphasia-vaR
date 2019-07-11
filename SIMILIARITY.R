#This is a script to calculate the standard deviation and coefficient of variation for Aline distances of repeated productions. It requires a CSV file of a broad narrow transcript with three columns: column 1 a placeholder number, column 2 the target transcription in Klattese, and column 3 the produced transcription in Klattese. 
library(alineR)

df <- read.csv(file="NC010a.txt", header=FALSE, stringsAsFactors = FALSE)
fn <- "NC029data.txt"

data <- {}
for (i in 1:nrow(df)) {
  first <- as.character(df[i,2])
  first <- paste(first, collapse="")
  first<-gsub(" ", "", first, fixed = TRUE)
  first <- lapply(first,utf8ToInt)
  first <- lapply(first, function(x) {
    gsub("67", "679", x)
  })
  
  first <- lapply(first, function(x) {
    gsub("74", "676", x)
  })
  
  first <- lapply(first, function(x) {
    gsub("83", "643", x)
  })
  
  first <- lapply(first, function(x) {
    gsub("90", "658", x)
  })
  
  first <- lapply(first, function(x) {
    gsub("84", "952", x)
  })
  
  first <- lapply(first, function(x) {
    gsub("68", "240", x)
  })
  
  first <- lapply(first, function(x) {
    gsub("71", "331", x)
  })
  
  # first <- lapply(first, function(x) {
  ## gsub("114", "633", x)
  # })
  
  first <- lapply(first, function(x) {
    gsub("121", "106", x)
  })
  
  #first <- lapply(first, function(x) {
  ##  gsub("73", "618", x)
  #})
  
  first <- lapply(first, function(x) {
    gsub("69", "603", x)
  })
  
  first <- lapply(first, function(x) {
    gsub("64", "230", x)
  })
  
  first <- lapply(first, function(x) {
    gsub("97", "97", x)
  })
  
  first <- lapply(first, function(x) {
    gsub("87", c("593", "117"), x)
  })
  
  first <- lapply(first, function(x) {
    gsub("89", c("593", "618"), x)
  })
  
  first <- lapply(first, function(x) {
    gsub("94", "652", x)
  })
  
  first <- lapply(first, function(x) {
    gsub("99", "596", x)
  })
  
  first <- lapply(first, function(x) {
    gsub("79", c("111", "618"), x)
  })
  
  first <- lapply(first, function(x) {
    gsub("117", "650", x)
  })
  
  first <- lapply(first, function(x) {
    gsub("82", c("601","114"), x)
  }) 
  
  first <- lapply(first, function(x) {
    gsub("120", "601", x)
  })
  
  # first <- lapply(first, function(x) {
  #   gsub("124", "619", x)
  # }) 
  
  first <- lapply(first, function(x) {
    gsub("88", c("601","114"), x)
  }) 
  
  first <-lapply(first, intToUtf8)
  ###
  
  second <- as.character(df[i,3])
  second <- paste(second, collapse="")
  second<-gsub(" ", "", second, fixed = TRUE)

  second <- lapply(second,utf8ToInt)
  second <- lapply(second, function(x) {
    gsub("67", "679", x)
  })
  
  second <- lapply(second, function(x) {
    gsub("74", "676", x)
  })
  
  second <- lapply(second, function(x) {
    gsub("83", "643", x)
  })
  
  second <- lapply(second, function(x) {
    gsub("90", "658", x)
  })
  
  second <- lapply(second, function(x) {
    gsub("84", "952", x)
  })
  
  second <- lapply(second, function(x) {
    gsub("68", "240", x)
  })
  
  second <- lapply(second, function(x) {
    gsub("71", "331", x)
  })
  
  # second <- lapply(second, function(x) {
  ## gsub("114", "633", x)
  # })
  
  second <- lapply(second, function(x) {
    gsub("121", "106", x)
  })
  
  #second <- lapply(second, function(x) {
  ##  gsub("73", "618", x)
  #})
  
  second <- lapply(second, function(x) {
    gsub("69", "603", x)
  })
  
  second <- lapply(second, function(x) {
    gsub("64", "230", x)
  })
  
  second <- lapply(second, function(x) {
    gsub("97", "97", x)
  })
  
  second <- lapply(second, function(x) {
    gsub("87", c("593", "117"), x)
  })
  
  second <- lapply(second, function(x) {
    gsub("89", c("593", "618"), x)
  })
  
  second <- lapply(second, function(x) {
    gsub("94", "652", x)
  })
  
  second <- lapply(second, function(x) {
    gsub("99", "596", x)
  })
  
  second <- lapply(second, function(x) {
    gsub("79", c("111", "618"), x)
  })
  
  second <- lapply(second, function(x) {
    gsub("117", "650", x)
  })
  
  second <- lapply(second, function(x) {
    gsub("82", c("601","114"), x)
  }) 
  
  second <- lapply(second, function(x) {
    gsub("120", "601", x)
  })
  
  # second <- lapply(second, function(x) {
  #   gsub("124", "619", x)
  # }) 
  
  second <- lapply(second, function(x) {
    gsub("88", c("601","114"), x)
  }) 
  
  second <-lapply(second, intToUtf8)
  second<-as.character(second)
  fiat <-aline(first, second)
  data <- append(fiat, data)
}

standard_deviation <- sd(data)
coeff_variation <- standard_deviation/mean(data)

#Kevin T Cunningham 7/10/19
#www.github.com/ktcslp
#fiat mihi secundum verbum tuum