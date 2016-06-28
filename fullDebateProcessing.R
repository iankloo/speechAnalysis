######Process all debates

##stored in files with titles, "partyMMDDYYY"

library(koRpus)
library(stringr)

setwd("/home/ian/Desktop/speechAnalysis/debates")

files <- list.files()
files <- gsub('~', '\\1', files)

#list of candidates
candidates <- list(republicans = c('TRUMP', 'CRUZ', 'BUSH', 'RUBIO'), democrats = c('CLINTON', 'SANDERS'))

reps <- c("TRUMP", "CRUZ", "BUSH", "RUBIO")
dems <- c("CLINTON", "SANDERS")

candidates <- data.frame(name=c(reps, dems), stringsAsFactors=FALSE)
candidates$party <- ''
for(i in 1:nrow(candidates)) {
  if(candidates$name[i] %in% reps){
    candidates$party[i] <- 'rep'
  } else{
    candidates$party[i] <- 'dem'
  }
}

repDebates <- files[grep('rep', files)]
demDebates <- files[grep('dem', files)]

finalDF <- data.frame(date=character(), party=character(), candidate=character(), grade=numeric(), age=numeric(), stringsAsFactors=FALSE)
for(i in 1:nrow(candidates)) {
  tempCandidate <- candidates$name[i]
  tempDebate <- files[grep(candidates$party[i], files)]
  
  for(j in 1:length(tempDebate)){
    debate <- readLines(tempDebate[j])
    split <- unlist(strsplit(debate, split = ' '))
    
    startTerms <- grep(paste(tempCandidate, ':', sep=''), split)
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
      statements <- gsub(paste(tempCandidate, ':', sep=''), '\\1', statements)
      
      debate <- tokenize(statements, format='obj', lang='en')
      model <- flesch.kincaid(debate)
      modelDF <- data.frame(model@Flesch.Kincaid)
      
      temp <- data.frame(date = as.Date(str_sub(tempDebate[j],4), '%m%d%Y'), party = candidates$party[i], candidate=tempCandidate, grade=modelDF[1,2], age=modelDF[1,3], stringsAsFactors=FALSE)
      
      finalDF <- rbind(finalDF, temp)
    }
  }
}


test <- finalDF[order(finalDF$date),]

#fix dates
plot <- nPlot(grade ~ date, group =  'candidate', data = test,
            type = 'multiBarChart', id = 'chart'
)
plot

plot <- nPlot(grade ~ date, group =  'candidate', data = finalDF, 
              type = 'scatterChart', id = 'chart'
)
plot$chart(sizeRange = c(200,200))
plot$chart(color = c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', '#ffff33'))
plot$chart(forceY = c(0, 10))
plot$chart(tooltipContent = "#! function(d){ 
  return d
} !#")
plot$xAxis(axisLabel='Date',
  tickFormat =   "#!
      function(d) {return d3.time.format('%Y-%m-%d')(new Date(d * 86400000 + 86400000));}
    !#"
)
plot$yAxis(axisLabel='Grade Level')
plot$chart(margin = list(right = 50))
plot



p8$xAxis( tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#" )






