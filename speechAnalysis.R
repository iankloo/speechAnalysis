library(koRpus)

taggedTrump <- tokenize('trumpPolicySpeech.txt', lang='en')
flesch.kincaid(taggedTrump)

taggedObama <- tokenize('obamaSpeech.txt', lang='en')
flesch.kincaid(taggedObama)


dat <- readLines('trumpPolicySpeech.txt')
dat <- dat[dat != '']

dat <- gsub("[[:punct:]]", '\\1', dat)


newWords <- ''
newWordsOld <- ''
creativity <- data.frame(sentence = numeric(), uniqueWords = numeric(), uniqueWordsScaled = numeric())
for(i in 1:length(dat)){
  words <- unlist(strsplit(dat[i], split=' '))
  
  newWordsOld <- newWords
  
  newWords <- c(newWords, words[!words %in% newWords])
  
  creativity <- rbind(creativity, data.frame(sentence = i, uniqueWords = length(newWords) - length(newWordsOld),
                                             uniqueWordsScaled = (length(newWords) - length(newWordsOld)) / length(words)))
}

plot(creativity[,c(1,3)])
