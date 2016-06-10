
debate <- readLines('debate.txt')

split <- unlist(strsplit(debate, split = ' '))

trumpStarts <- grep('TRUMP:', split)
start <- numeric()
end <- numeric()
statements <- character()
for(i in 1:length(trumpStarts)){
  start <- trumpStarts[i]
  newSplit <- split[trumpStarts[i]:length(split)]
  end <- start + grep('[A-Z]+\\:', newSplit[2:length(newSplit)])[1] - 1
  
  statements <- c(statements, split[start:end])
}

statements <- gsub('TRUMP:', '\\1', statements)

debate <- tokenize(statements, format='obj', lang='en')
flesch.kincaid(debate)


trumpStarts <- grep('KASICH:', split)
start <- numeric()
end <- numeric()
statements <- character()
for(i in 1:length(trumpStarts)){
  start <- trumpStarts[i]
  newSplit <- split[trumpStarts[i]:length(split)]
  end <- start + grep('[A-Z]+\\:', newSplit[2:length(newSplit)])[1] - 1
  
  statements <- c(statements, split[start:end])
}

statements <- gsub('KASICH:', '\\1', statements)

debate <- tokenize(statements, format='obj', lang='en')
flesch.kincaid(debate)



trumpStarts <- grep('CRUZ:', split)
start <- numeric()
end <- numeric()
statements <- character()
for(i in 1:length(trumpStarts)){
  start <- trumpStarts[i]
  newSplit <- split[trumpStarts[i]:length(split)]
  end <- start + grep('[A-Z]+\\:', newSplit[2:length(newSplit)])[1] - 1
  
  statements <- c(statements, split[start:end])
}

statements <- gsub('CRUZ:', '\\1', statements)

debate <- tokenize(statements, format='obj', lang='en')
flesch.kincaid(debate)




trumpStarts <- grep('RUBIO:', split)
start <- numeric()
end <- numeric()
statements <- character()
for(i in 1:length(trumpStarts)){
  start <- trumpStarts[i]
  newSplit <- split[trumpStarts[i]:length(split)]
  end <- start + grep('[A-Z]+\\:', newSplit[2:length(newSplit)])[1] - 1
  
  statements <- c(statements, split[start:end])
}

statements <- gsub('RUBIO:', '\\1', statements)

debate <- tokenize(statements, format='obj', lang='en')
flesch.kincaid(debate)







#########dems


debate <- readLines('demDebate.txt')

split <- unlist(strsplit(debate, split = ' '))

trumpStarts <- grep('CLINTON:', split)
start <- numeric()
end <- numeric()
statements <- character()
for(i in 1:length(trumpStarts)){
  start <- trumpStarts[i]
  newSplit <- split[trumpStarts[i]:length(split)]
  end <- start + grep('[A-Z]+\\:', newSplit[2:length(newSplit)])[1] - 1
  
  statements <- c(statements, split[start:end])
}

statements <- gsub('CLINTON:', '\\1', statements)

debate <- tokenize(statements, format='obj', lang='en')
flesch.kincaid(debate)


trumpStarts <- grep('SANDERS:', split)
start <- numeric()
end <- numeric()
statements <- character()
for(i in 1:length(trumpStarts)){
  start <- trumpStarts[i]
  newSplit <- split[trumpStarts[i]:length(split)]
  end <- start + grep('[A-Z]+\\:', newSplit[2:length(newSplit)])[1] - 1
  
  statements <- c(statements, split[start:end])
}

statements <- gsub('SANDERS:', '\\1', statements)

debate <- tokenize(statements, format='obj', lang='en')
flesch.kincaid(debate)
