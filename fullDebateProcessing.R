######Process all debates

##stored in files with titles, "partyMMDDYYY"

library(koRpus)
library(stringr)

setwd("/home/ian/Desktop/speechAnalysis/debates")

files <- list.files()
files <- gsub('~', '\\1', files)

#list of candidates
candidates <- list(republicans = c('TRUMP', 'CRUZ', 'BUSH', 'RUBIO'), democrats = c('CLINTON', 'SANDERS'))

repDebates <- files[grep('rep', files)]
demDebates <- files[grep('dem', files)]

finalDF <- data.frame(date=character(), party=character(), candidate=character(), grade=numeric(), age=numeric(), stringsAsFactors=FALSE)
for(i in 1:length(candidates$democrats)) {
  candidate <- candidates$democrats[i]
  print(candidate)
  
  #1st debate
  for(j in 1:length(demDebates)){
    debate <- readLines(demDebates[j])
    split <- unlist(strsplit(debate, split = ' '))
    
    startTerms <- grep(paste(candidate, ':', sep=''), split)
    start <- numeric()
    end <- numeric()
    statements <- character()
    if(length(startTerms > 0)){
      for(k in 1:length(startTerms)){
        start <- startTerms[k]
        newSplit <- split[startTerms[k]:length(split)]
        end <- start + grep('[A-Z]+\\:', newSplit[2:length(newSplit)])[1] - 1
        if(is.na(end)) {
          statements <- c(statements, split[start:length(split)])
        } else{
          statements <- c(statements, split[start:end])
        }
      }
      statements <- gsub(paste(candidate, ':', sep=''), '\\1', statements)
      
      debate <- tokenize(statements, format='obj', lang='en')
      model <- flesch.kincaid(debate)
      modelDF <- data.frame(model@Flesch.Kincaid)
      
      temp <- data.frame(date = as.Date(str_sub(demDebates[j],4), '%m%d%Y'), party = 'dem', candidate=candidate, grade=modelDF[1,2], age=modelDF[1,3], stringsAsFactors=FALSE)
      
      finalDF <- rbind(finalDF, temp)
    }
  }
}

firstRun <- rbind(repFinal, finalDF)

write.csv(firstRun, 'firstRun.csv', row.names=FALSE)

#fix dates
plot <- nPlot(grade ~ date, group =  'candidate', data = firstRun, 
            type = 'multiBarChart', id = 'chart'
)
plot

#fix dates
plot <- nPlot(grade ~ date, group =  'candidate', data = firstRun, 
              type = 'scatterChart', id = 'chart'
)
plot




