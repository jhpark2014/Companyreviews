review <- readLines("c:/Users/Jae/Desktop/MSDreviews.txt")

library(tm)
library(RWeka)
library(wordcloud)

provecnum <- which(review == "The good things")
provecnum <- provecnum + 1

convecnum <- which(review == "The challenges") + 1
provec <- review[provecnum]
convec <- review[convecnum]

provec_clean <- removePunctuation(provec)
provec_clean <- removeWords(provec_clean, stopwords('en'))
provec_clean <- tolower(provec_clean)
provec_clean <- stripWhitespace(provec_clean)
provec_clean[5] <- "work life balance"

cleanfunction <- function(x) {
 x <- removePunctuation(x)
 x <- removeWords(x, stopwords('en'))
 x <- tolower(x)
 x <- stripWhitespace(x)
}
convec_clean <- cleanfunction(convec)

provec_corpus <- VCorpus(VectorSource(provec_clean))
convec_corpus <- VCorpus(VectorSource(convec_clean))

bitrigram <- function(x) { NGramTokenizer(x, Weka_control(min = 1, max = 3))}

pros_tdm_m <- as.matrix(TermDocumentMatrix(provec_corpus, control = list(tokenize = bitrigram)))
cons_tdm_m <- as.matrix(TermDocumentMatrix(convec_corpus, control = list(tokenize = bitrigram)))

proswordfreq <- data.frame(word = rownames(pros_tdm_m), freq = rowSums(pros_tdm_m))
conswordfreq <- data.frame(word = rownames(cons_tdm_m), freq = rowSums(cons_tdm_m))

wordcloud(conswordfreq$word, conswordfreq$freq, min.freq = 2, color = "red")
wordcloud(proswordfreq$word, proswordfreq$freq, min.freq = 2, color = "blue")