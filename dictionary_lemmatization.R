#View(lexicon::hash_lemmas)


dictionary <- lexicon::hash_lemmas
dictionary$lemma[dictionary$token=="well"] <- "well"
dictionary$lemma[dictionary$token=="better"] <- "better"
dictionary$lemma[dictionary$token=="best"] <- "best"
dictionary$lemma[dictionary$token=="maria"] <- "maria"
dictionary$lemma[dictionary$token=="second"] <- "two"
dictionary$lemma[dictionary$token=="third"] <- "three"
dictionary$lemma[dictionary$token=="fourth"] <- "four"
dictionary$lemma[dictionary$token=="fifth"] <- "five"
dictionary$lemma[dictionary$token=="sixth"] <- "six"
dictionary$lemma[dictionary$token=="seventh"] <- "seven"
dictionary$lemma[dictionary$token=="eighth"] <- "eight"
dictionary$lemma[dictionary$token=="listing"] <- "listing"
dictionary$lemma[dictionary$token=="evening"] <- "evening"

#dictionary$lemma[dictionary$token=="quickly"]

#aggiungo righe al vocabolario per trasformare avverbi in aggettivi e altro
my.dictionary <- data.frame(token="beautifully", lemma="beautiful")
my.dictionary <- rbind(my.dictionary, c("centrally", "central"))
my.dictionary <- rbind(my.dictionary, c("conveniently", "convenient"))
my.dictionary <- rbind(my.dictionary, c("comfortable", "comfort"))
my.dictionary <- rbind(my.dictionary, c("easily", "easy"))
my.dictionary <- rbind(my.dictionary, c("enojoyable", "enjoy"))
my.dictionary <- rbind(my.dictionary, c("exactly", "exact"))
my.dictionary <- rbind(my.dictionary, c("nearby", "near"))
my.dictionary <- rbind(my.dictionary, c("nicely", "nice"))
my.dictionary <- rbind(my.dictionary, c("noisy", "noise"))
my.dictionary <- rbind(my.dictionary, c("perfectly", "perfect"))
my.dictionary <- rbind(my.dictionary, c("quickly", "quick"))
my.dictionary <- rbind(my.dictionary, c("recommendation", "recommend"))
my.dictionary <- rbind(my.dictionary, c("stylish", "style"))
my.dictionary <- rbind(my.dictionary, c("suggestion", "suggest"))
my.dictionary <- rbind(my.dictionary, c("walkable", "walk"))


dictionary <- rbind(dictionary, my.dictionary)


write.csv(dictionary, "dictionary.csv", row.names = F)

#dictionary <- read.csv("dictionary.csv")

#dictionary$token[dictionary$lemma=="con"]

