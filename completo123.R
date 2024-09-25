# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(tictoc)
library(cld3)
library(tm)
library(stringr)
library(textclean)
#library(hunspell)
library(textstem)


library(topicmodels)
library(ldatuning)
library(LDAvis)
library(tidytext)
library(ggplot2)

#per rappresentazioni cartografiche
library(sf)
library(ggspatial)
#library(raster)

#per sentiment analysis
library(sentimentr)

#per supervised lda
library(lda)


# IMPORTAZIONE ED ELIMINAZIONE DELLE RECENSIONI NON IN INGLESE -----------------------------

#dati aggiornati al 22/06/2024
reviews <- read.csv("reviews_milano2.csv")
#TOTALE RECENSIONI 830436


#analisi della lingua delle recensioni (usando il pacchetto cld3)
library(cld3)
tic()
language <- detect_language(reviews$comments)
toc()
#circa 430 secondi, ossia 7 minuti

#osservo la frequenza delle lingua
table(language)

length(which(language=="en"))/dim(reviews)[1]
#53.52% delle recensioni sono in lingua inglese

ind <- which(language=="en")
#444488 recensioni in inglese

reviews <- reviews[ind,]
#444488 recensioni rimaste

rm(ind)
rm(language)

#salvo un csv
write.csv(reviews, "reviews_milano2_ing.csv", row.names = FALSE)

# CARICAMENTO RECENSIONI (SOLO INGLESE) ---------------------------------
#la prima fase di pre-processing (eliminazione delle recensioni in lingua inglese)
#è già stata eseguita

reviews <- read.csv("reviews_milano2_ing.csv")
#444488


# PRE-PROCESSING 1 (GENERALE)--------------------------------------

#prima fase di pre-processing: elimino tutti i caratteri speciali per rendere 
#leggibili le recensioni

preprocessing1 <- function(reviews){
  
  txt <- reviews$comments #salvo un oggetto contenente solo le recensioni
  
  #rimozione caratteri HTML
  txt <- str_replace_all(txt, "<[^>]+>", " ")
  
  #rimozione \n e \r
  txt <- str_replace_all(txt, "\\s*\\n\\s*", " ")
  txt <- str_replace_all(txt, "\\s*\\r\\s*", " ")
  
  #rimuovo spazi multipli
  txt <- str_replace_all(txt, "\\s+", " ")
  
  #rimuovo altri simboli speciali HTML
  txt <- str_replace_all(txt, "&[a-zA-Z]+;", "")
  
  #converto caratteri non ascii in caratteri ascii
  txt <- str_replace_all(txt, "–", "-")
  txt <- str_replace_all(txt, "—", "-")
  txt <- str_replace_all(txt, "’", "'")
  txt <- str_replace_all(txt, "“", "\"")
  txt <- str_replace_all(txt, "”", "\"")
  txt <- str_replace_all(txt, "„", "\"")
  txt <- str_replace_all(txt, "…", "...")
  txt <- str_replace_all(txt, "！", "!")
  txt <- str_replace_all(txt, "é", "e")
  txt <- str_replace_all(txt, "è", "e")
  txt <- str_replace_all(txt, "ê", "e")
  txt <- str_replace_all(txt, "á", "a")
  txt <- str_replace_all(txt, "à", "a")
  txt <- str_replace_all(txt, "ó", "o")
  txt <- str_replace_all(txt, "ò", "o")
  txt <- str_replace_all(txt, "ì", "i")
  txt <- str_replace_all(txt, "í", "i")
  txt <- str_replace_all(txt, "ı", "i")
  txt <- str_replace_all(txt, "ú", "u")
  txt <- str_replace_all(txt, "ù", "u")
  txt <- str_replace_all(txt, "、", ",")
  txt <- str_replace_all(txt, "，", ",")
  txt <- str_replace_all(txt, "‘", "'")
  txt <- str_replace_all(txt, "ç", "c")
  
  #rimuovo caratteri non europei
  txt <- str_replace_all(txt, "[^\\x00-\\x7F]", "")
  #txt <- replace_non_ascii(txt, replacement = "", remove.nonconverted = TRUE)
  
  
  #elimino eventuali spazi in eccesso
  txt <- stripWhitespace(txt) 
  
  #rimuovo spazi bianchi all'inizio e alla fine
  txt <- gsub("^[[:space:]]+", "", txt) # remove whitespace at beginning of documents
  txt <- gsub("[[:space:]]+$", "", txt) # remove whitespace at end of documents
  
  reviews$comments <- txt
  return(reviews)
}


tic()
reviews <- preprocessing1(reviews)
toc()
#circa 50 secondi

#salvo un file csv con le recensioni "ripulite" dai caratteri speciali
write.csv(reviews, "reviews_milano2_ing_nohtml.csv", row.names = FALSE)



# PRE-PROCESSING 2 (PER LDA)-------------------------------------------------


reviews <- read.csv("reviews_milano2_ing_nohtml.csv")


#funzione che conta il numero di parole in una recensione
count_words <- function(s) {
  strsplit(s, "\\s+")[[1]] %>% length()
}


lunghezza_recensioni <- sapply(reviews$comments, count_words)
summary(lunghezza_recensioni)


#elimino tutte le recensioni con meno di 50 parole
#perdo gran parte delle recensioni a disposizione, tuttavia quelle troppo corte
#sono spesso poco informative e quindi non utili

ind_50 <- which(lunghezza_recensioni <= 50)
reviews <- reviews[-ind_50,]
#rimangono 13198 recensioni



#elimino recensioni duplicate
ind_dup <- which(duplicated(reviews$comments))

#creo dataset con tutte le recensioni che appaiono più volte
recensioni_dup <- unique(reviews$comments[ind_dup])
ind_dup_extended <- which(reviews$comments %in% recensioni_dup)
reviews_dup <- reviews[ind_dup_extended,]

#cancello tutte le recensioni duplicate tutte le volte che appaiono
reviews <- reviews[-ind_dup_extended, ]
#rimangono 43094

#join con tabella relativa alle strutture
listings <- read.csv("listings_milano.csv")
#neighbourhoods <- read.csv("neighbourhoods_milano.csv")

#faccio un join tra reviews e listings
listings <- listings %>% 
  rename(listing_id=id)

reviews_new <- left_join(reviews, listings) %>%
  select(review_id=id, listing_id, date, comments, neighbourhood_cleansed, 
         latitude, longitude, room_type)

#analizzo la frequenza delle recensioni per ogni type room
table(reviews_new$room_type) 
table(reviews_new$room_type) %>% prop.table()

#elimino hotel room e shared room (in quanto sotto-rappresentate)
reviews_new <- reviews_new %>%
  filter(room_type %in% c("Entire home/apt", "Private room"))
#rimangono 13142 recensioni

reviews <- reviews_new

rm( reviews_new, listings)
rm(ind_dup, ind_dup_extended, recensioni_dup)
rm(reviews_dup)
rm(ind_50, lunghezza_recensioni, ind)


#ora continuo il pre-processing
preprocessing2 <- function(reviews){
  
  txt <- reviews$comments
  
  #trasformo doppi e tripli puntini di sospensione, virgole e altri segni di
  #punteggiatura in spazi. Così facendo evito che due parole separate solo da tali
  #segni rimangano in seguito "attaccate" tra loro
  txt <- gsub("\\.\\.\\.", " ", txt)
  txt <- gsub("\\.\\.", " ", txt)
  txt <- gsub(",", " ", txt)
  txt <- gsub("-", " ", txt)
  txt <- gsub("/", " ", txt)
  txt <- gsub('\"', " ", txt)
  
  #rimuovo spazi bianchi in eccesso (che potrebbero essersi creati con l'eliminazione
  #dei segni di punteggiatura appena effettuata)
  txt <- stripWhitespace(txt)
  
  
  #trasformo tutto in minuscolo
  txt <- tolower(txt)
  
  #converto alcune espressioni specifiche
  txt <- gsub("check in", "checkin", txt)
  txt <- gsub("checking in", "checkin", txt)
  txt <- gsub("checked in", "checkin", txt)
  txt <- gsub("check out", "checkout", txt)
  txt <- gsub("checking out", "checkout", txt)
  txt <- gsub("checked out", "checkout", txt)
  txt <- gsub("wi fi", "wifi", txt)
  txt <- gsub("as well as", " ", txt)
  txt <- gsub("dish washer", "dishwasher", txt)
  txt <- gsub("air conditioning", "aircondition", txt)
  txt <- gsub("air conditioner\\b", "aircondition", txt)
  txt <- gsub("air conditioned\\b", "aircondition", txt)
  txt <- gsub("air condition\\b", "aircondition", txt)
  txt <- gsub("conditioner", "aircondition", txt)
  txt <- gsub("\\bac\\b", "aircondition", txt)
  txt <- gsub("a\\.c\\.", "aircondition", txt)
  
  #rimuovo le stopwords classiche della lingua inglese
  #(con l'esclusione di again)
  stopwords <- stopwords("en")
  stopwords <- setdiff(stopwords, "again")
  txt <- removeWords(txt, stopwords)
  #txt <- removeWords(txt, stopwords("SMART"))
  
  #converto alcune parole specifiche (rilevate da analisi esplorativa)
  #alcune sono relative alla differenza tra inglese britannico e americano
  txt <- gsub("\\bmin\\b", "minute", txt)
  txt <- gsub("\\bmins\\b", "minute", txt)
  txt <- gsub("\\bcozy\\b", "cosy", txt)
  txt <- gsub("\\bdome\\b", "duomo", txt)
  txt <- gsub("\\bcathedral\\b", "duomo", txt)
  txt <- gsub("centrale", "central", txt)
  txt <- gsub("stazione", "station", txt)
  txt <- gsub("appartement", "apartment", txt)
  txt <- gsub("appartment", "apartment", txt)
  txt <- gsub("\\bapart\\b", "apartment", txt)
  txt <- gsub("\\bapart\\.", "apartment", txt)
  txt <- gsub("\\bappart\\b", "apartment", txt)
  txt <- gsub("\\bappart\\.\\b", "apartment", txt)
  txt <- gsub("\\bflat\\b", "apartment", txt)
  txt <- gsub("\\bapt\\b", "apartment", txt)
  txt <- gsub("\\bapt\\.", "apartment", txt)
  txt <- gsub("neighborhood", "neighbourhood", txt)
  txt <- gsub("\\bneighbor\\b", "neighbour", txt)
  txt <- gsub("transportation", "transport", txt)
  txt <- gsub("\\belevator\\b", "lift", txt)
  txt <- gsub("vacation", "holiday", txt)
  txt <- gsub("\\bcenter\\b", "centre", txt)
  txt <- gsub("milano", "milan", txt)
  txt <- gsub("mailand", "milan", txt)
  txt <- gsub("\\bcomfy\\b", "comfortable", txt)
  txt <- gsub("\\bcouch\\b", "sofa", txt)
  txt <- gsub("\\bgrazie\\b", "thank", txt)
  txt <- gsub("\\bok\\b", "okay", txt)
  txt <- gsub("airbnbs", "airbnb", txt)
  txt <- gsub("\\bsubway\\b", "metro", txt)
  txt <- gsub("\\bunderground\\b", "metro", txt)
  txt <- gsub("\\btraveller\\b", "traveler", txt)
  
  
  #converto l'apostrofo in spazio per separare, ad esempio, le s del genitivo sassone
  #dai nomi
  txt <- gsub("'"," ", txt)
  
  
  #rimuovo punteggiatura e numeri
  txt <- removePunctuation(txt,preserve_intra_word_dashes = FALSE)
  txt <- removeNumbers(txt)
  
  #converto nuovamente min e mins in minutes (dopo aver eliminato i numeri)
  txt <- gsub("\\bmin\\b", "minute", txt)
  txt <- gsub("\\bmins\\b", "minute", txt)
  
  #rimuovo altri caratteri NON alfanumerici (e NON spazi) rimasti (come le emoji)
  txt <- gsub('[^[:alnum:]\\s]', ' ', txt)
  
  #rimuovo eventuali parole rimaste giudicate non utili per l'analisi
  words_to_eliminate <- c("etc", "milan", "also", "can", "will", "really",
                          "many", "lot", "lots", "just", "highly", "bit", 
                          "either", "else", "us")
  txt <- removeWords(txt, words_to_eliminate)
  
  
  #LEMMATIZATION
  dictionary <- read.csv("dictionary.csv") #dizionario standard di lemmatiazation
  #con alcune modifiche fatte da me (vedi codice a parte)
  txt <- lemmatize_strings(txt, dictionary = dictionary)
  
  #rimuovo altre parole dopo la LEMMATIZATION
  words_to_eliminate <- c("apartment", "great", "place", "stay", "everything")
  txt <- removeWords(txt, words_to_eliminate)
  
  
  #rimuovo spazi bianchi in eccesso
  txt <- stripWhitespace(txt)
  
  #rimuovo spazi bianchi all'inizio e alla fine
  txt <- gsub("^[[:space:]]+", "", txt) # remove whitespace at beginning of documents
  txt <- gsub("[[:space:]]+$", "", txt) # remove whitespace at end of documents
  
  reviews$comments_preproc <- txt
  return(reviews)
}

tic()
reviews <- preprocessing2(reviews)
toc()



#creazione della document term matrix considerando solo parole con almeno due
#lettere (ossia eliminando parole date da lettere singole)
txt <- reviews$comments_preproc

corpus <- Corpus(VectorSource(txt))
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths=c(2, Inf)))

#rimuovo i termini sparsi
dtm <- removeSparseTerms(dtm, 0.99)


#SCELTA NUMERO TOPICS (SEED 123)
tic()
ftn <- FindTopicsNumber(dtm, 
                        topics=2:30, 
                        metrics=c("Deveaud2014"), 
                        control=list(iter=2700, burnin=500, thin=200,
                                     seed=123),
                        return_models = TRUE)
FindTopicsNumber_plot(ftn)
toc()

save.image("completo123.RData")

load("completo123.RData")


#ANALISI TEMPORALE DELLE RECENSIONI
#analizzo anni
data <- as.Date(reviews$date)
anno <- format(data, "%Y")


reviews$date <- anno

reviews_per_year <- reviews %>%
  group_by(date) %>%
  count()

ggplot(data=reviews_per_year, mapping = aes(x=date, y=n, group = 1)) +
  theme_bw() +
  geom_line() +
  geom_point() +
  labs(x="Year", y="Number of reviews")



#grafico di confronto numero topics
#FindTopicsNumber_plot(ftn)


#considero solo topics da 2 a 20
ftn_ridotto <- ftn[11:29, c(1,3)]

FindTopicsNumber_plot(ftn_ridotto)

val.y <- ftn_ridotto$Deveaud2014[ftn_ridotto$topics==10]

plot_deveaud <- ggplot(data=ftn_ridotto, aes(x=topics, y=Deveaud2014)) + 
  theme_bw() +
  geom_point() + 
  geom_line() +
  geom_point(data=ftn_ridotto[ftn_ridotto$topics %in% c(6,10,13,16),], color="red") +
  scale_x_continuous(breaks = seq(0,20,1)) +
  #scale_y_continuous(breaks = seq(3,5,0.1)) +
  #geom_vline(xintercept = 10, linetype="dashed", color="red") +
  geom_segment(x = 10, xend = 10, y = 2.5, yend = val.y, 
              color = "red", linetype="dashed") +
  labs(x="Number of topics", y="Deveaud criterion")

print(plot_deveaud)

#modello migliore con 10 topics

lda_model <- ftn$LDA_model[[21]]


ap_topics <- tidy(lda_model, matrix = "beta")

top_terms <- ap_topics  %>% # take the topics data frame and..
  group_by(topic) %>% # treat each topic as a different group
  top_n(30, beta) %>% # get the top most informative words
  ungroup() %>% # ungroup
  arrange(topic, -beta) #

wide_top_terms <- top_terms %>%
  select(topic, term) %>%
  group_by(topic) %>%
  mutate(term_id = row_number()) %>%   # Aggiungi un identificatore per i termini
  pivot_wider(names_from = topic, values_from = term, names_sort = TRUE) %>%
  select(-term_id)  # Rimuovi l'identificatore temporaneo


documents <- tidy(lda_model, matrix = "gamma") %>%
  arrange(document, topic)

documents_wide <- documents %>% spread(topic, gamma)
documents_wide <- documents_wide[order(as.integer(documents_wide$document)),]

#aggiungo topic massimo
documents_wide$max <- apply(documents_wide[2:11], 1, which.max)

#interpretazione dei topics (guardando alle parole più frequenti)
#TOPIC 1: APARTMENT (ROOMS AND AMENITIES)
#TOPIC 2: NEIGHBOURHOOD
#TOPIC 3: DESIRE TO COME BACK/FEELING AT HOME
#TOPIC 4: 
#TOPIC 5: APARTMENT (OVERALL)
#TOPIC 6: CHECK-IN
#TOPIC 7:
#TOPIC 8: ACCESSIBILITY TO PUBLIC TRANSPORT
#TOPIC 9: ISSUES
#TOPIC 10: HOST


#osservo le recensioni più frequenti
apply(documents_wide[,-1], 2, max)

#TOPIC 1
ind1 <- which(documents_wide[,2]>0.33)
reviews$comments[ind1]
documents_wide[ind1,]
#ok

#TOPIC 2
ind2 <- which(documents_wide[,3]>0.28)
reviews$comments[ind2]
documents_wide[ind2,]


#TOPIC 3
ind3 <- which(documents_wide[,4]>0.273)
reviews$comments[ind3]
documents_wide[ind3,]

#TOPIC 4
ind4 <- which(documents_wide[,5]>0.30)
reviews$comments[ind4]
documents_wide[ind4,]

#TOPIC 5
ind5 <- which(documents_wide[,6]>0.255)
reviews$comments[ind5]
documents_wide[ind5,]

#TOPIC 6
ind6 <- which(documents_wide[,7]>0.37)
reviews$comments[ind6]
documents_wide[ind6,]


#TOPIC 7
ind7 <- which(documents_wide[,8]>0.35)
reviews$comments[ind7]
documents_wide[ind7,]

#TOPIC 8
ind8 <- which(documents_wide[,9]>0.30)
reviews$comments[ind8]
documents_wide[ind8,]

#TOPIC 9
ind9 <- which(documents_wide[,10]>0.35)
reviews$comments[ind9]
documents_wide[ind9,]

#TOPIC 10
ind10 <- which(documents_wide[,11]>0.245)
reviews$comments[ind10]
documents_wide[ind10,]


#visualizzazione del modello

#this function is needed to visualize LDA through LDAvis package
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}
serVis(topicmodels2LDAvis(lda_model))


#analisi geografica

#importo zone di milano
OMI<-st_read("MI_Zone_OMI corretto_region.shp",quiet=TRUE)
OMI= st_set_crs(OMI,3003)  # Gauss-Boaga


#importo fermate della metro
MM<-st_read("MM_FERMATE.shp",quiet=TRUE)
MM= st_set_crs(MM,3003)  # Gauss-Boaga


#mappa di Milano
g0=ggplot()
g1=annotation_map_tile("thunderforestoutdoors",  zoomin=0)
g2=geom_sf(data=st_boundary(OMI))

map1=g0+g1+g2
print(map1)

#mappa di Milano e fermate della metro (intersezione)
MMinMI <- st_intersection(OMI, MM)
map2= map1 + geom_sf(data=MMinMI, color = "blue", size = 1.5)  + 
  ggspatial::annotation_north_arrow(which_north = "true") +
  ggspatial::annotation_scale(location="br") +
  geom_sf() 
print(map2)


#mappa di tutte le strutture airbnb
listings <- reviews %>%
  select(listing_id, neighbourhood_cleansed, latitude, longitude) %>%
  unique()

listings <- sf::st_as_sf(listings, coords = c("longitude", "latitude"), crs=4326)
listings <- st_transform(listings, crs=3003)

map3= map1 + geom_sf(data=listings, color = "red", size = 0.05, shape=3)  + 
  ggspatial::annotation_north_arrow(which_north = "true") +
  ggspatial::annotation_scale(location="br") +
  geom_sf()
print(map3)

#mappa delle strutture con topic prevalente comodità mezzi pubblici
ind <- which(documents_wide$max==8)
listings_small <- reviews
listings_small$transport <- 0
listings_small$transport[ind] <- 1

count_transport <- listings_small %>%
  group_by(listing_id) %>%
  count(wt=transport) %>%
  arrange(desc(n))

ind <- count_transport %>%
  filter(n>5) %>%
  .$listing_id

listings_small <- listings_small %>%
  filter(listing_id %in% ind)

listings_small <- listings_small %>%
  select(listing_id, neighbourhood_cleansed, latitude, longitude) %>%
  unique()


listings_small <- sf::st_as_sf(listings_small, coords = c("longitude", "latitude"), crs=4326)
listings_small <- st_transform(listings_small, crs=3003)

map4= map1 + geom_sf(data=listings_small, color = "red", size = 0.5, shape=3)  + 
  ggspatial::annotation_north_arrow(which_north = "true") +
  ggspatial::annotation_scale(location="br") +
  geom_sf()
print(map4)

#grafico con entrambi
map5 = map1 + geom_sf(data=listings, color = "yellow", size = 0.05, shape=3) +
  geom_sf(data=listings_small, color = "red", size = 0.5, shape=3)  + 
  ggspatial::annotation_north_arrow(which_north = "true") +
  ggspatial::annotation_scale(location="br") +
  geom_sf()
print(map5)


#mappa di milano e fermate della metro  e cinque principali stazioni 
stations <- geo(street = c("Piazza Duca d'Aosta", 
                           "Piazza Sigmund Freud",
                           "Piazzale Luigi Cadorna",
                           "Via Cassinis, 3",
                           "Piazza Enrico Bottini"), 
                city = rep("Milan", 5))
stations <- sf::st_as_sf(stations, coords = c("long","lat"),crs=4326)
stations <- st_transform(stations, crs = 3003)

#con tutte le strutture
map6 <- map1 +
  geom_sf(data=listings, mapping=aes(color="Listings"), size = 0.05, shape=3) +
  geom_sf(data=MMinMI, mapping = aes(color="Metro Stops"), size = 1) +
  geom_sf(data=stations, mapping = aes(color="Train Stations"), size = 1) +
  ggspatial::annotation_north_arrow(which_north = "true") +
  ggspatial::annotation_scale(location="br") +
  scale_color_manual(values = c("Metro Stops" = "blue", 
                                "Listings" = "red",
                                "Train Stations"="green")) +
  labs(color = "Legend") +
  theme(legend.position="inside", legend.position.inside = c(0.35,0.1),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.1, "cm"),
        legend.title = element_blank(),
        legend.key.spacing.y = unit(0.1, "cm"),
        #legend.title = element_text(size=8, face="bold"),
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.1))
print(map6)

#con le strutture ristrette
map7 <- map1 +
  geom_sf(data=listings_small, mapping=aes(color="Listings"), size = 0.5, shape=3) +
  geom_sf(data=MMinMI, mapping = aes(color="Metro Stops"), size = 1) +
  geom_sf(data=stations, mapping = aes(color="Train Stations"), size = 1) +
  ggspatial::annotation_north_arrow(which_north = "true") +
  ggspatial::annotation_scale(location="br") +
  scale_color_manual(values = c("Metro Stops" = "blue", 
                                "Listings" = "red",
                                "Train Stations"="green")) +
  labs(color = "Legend") +
  theme(legend.position="inside", legend.position.inside = c(0.35,0.1),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.1, "cm"),
        legend.title = element_blank(),
        legend.key.spacing.y = unit(0.1, "cm"),
        #legend.title = element_text(size=8, face="bold"),
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.1))
print(map7)

#altro modo di filtrare le strutture (non usato)
listings_small <- reviews
listings_small$proportion <- documents_wide$'8'
summary(listings_small$proportion)
ind <- which(listings_small$proportion>0.20)
listings_small <- listings_small[ind,]


listings_small <- listings_small %>%
  select(listing_id, neighbourhood_cleansed, latitude, longitude) %>%
  unique()

listings_small <- sf::st_as_sf(listings_small, coords = c("longitude", "latitude"), crs=4326)
listings_small <- st_transform(listings_small, crs=3003)



map8 <- map1 +
  geom_sf(data=listings_small, mapping=aes(color="Listings"), size = 0.5, shape=3) +
  geom_sf(data=MMinMI, mapping = aes(color="Metro Stops"), size = 1) +
  geom_sf(data=stations, mapping = aes(color="Train Stations"), size = 1) +
  ggspatial::annotation_north_arrow(which_north = "true") +
  ggspatial::annotation_scale(location="br") +
  scale_color_manual(values = c("Metro Stops" = "blue", 
                                "Listings" = "red",
                                "Train Stations"="green")) +
  labs(color = "Legend") +
  theme(legend.position="inside", legend.position.inside = c(0.35,0.1),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.1, "cm"),
        legend.title = element_blank(),
        legend.key.spacing.y = unit(0.1, "cm"),
        #legend.title = element_text(size=8, face="bold"),
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.1))
print(map8)


# SENTIMENT ANALYSIS ------------------------------------------------------
load("completo123.RData")


txt_sa <- reviews$comments

#converto in minuscolo
txt_sa <- tolower(txt_sa)

#uso polarity_dt=lexicon::hash_sentiment_huliu (testato essere il migliore)
tic()
sentences <- get_sentences(txt_sa)
sentiment_score <- sentiment_by(sentences, polarity_dt=lexicon::hash_sentiment_huliu)
toc()

mean(sentiment_score$ave_sentiment>0)
#96% di recensioni positive

mean(sentiment_score$ave_sentiment<0)
#4% di recensioni negative


#aggiungo il sentiment score a reviews
reviews$sentiment_score <- sentiment_score$ave_sentiment
rm(sentiment_score)

ind_pos <- which(reviews$sentiment_score>0)
reviews_pos <- reviews[ind_pos,]

ind_neg <- which(reviews$sentiment_score<0)
reviews_neg <- reviews[ind_neg,]

ind_nul <- which(reviews$sentiment_score==0)
#ci sono anche 44 recensioni nulle



# LDA SU RECENSIONI NEGATIVE ----------------------------------------------


#creazione della document term matrix considerando solo parole con almeno due
#lettere (ossia eliminando parole date da lettere singole)
txt <- reviews_neg$comments_preproc

corpus <- Corpus(VectorSource(txt))
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths=c(2, Inf)))

#rimuovo i termini sparsi
dtm <- removeSparseTerms(dtm, 0.99)


#SCELTA NUMERO TOPICS (SEED 123)
tic()
ftn <- FindTopicsNumber(dtm, 
                        topics=2:30, 
                        metrics=c("Deveaud2014"), 
                        control=list(iter=2700, burnin=500, thin=200,
                                     seed=123),
                        return_models = TRUE)
FindTopicsNumber_plot(ftn)
toc()

save.image("negative123.RData")

load("negative123.RData")

#considero solo topics da 2 a 20
ftn_ridotto <- ftn[11:29, c(1,3)]

FindTopicsNumber_plot(ftn_ridotto)

val.y <- ftn_ridotto$Deveaud2014[ftn_ridotto$topics==3]

plot_deveaud <- ggplot(data=ftn_ridotto, aes(x=topics, y=Deveaud2014)) + 
  theme_bw() +
  geom_point() + 
  geom_line() +
  geom_point(data=ftn_ridotto[ftn_ridotto$topics==3,], color="red") +
  scale_x_continuous(breaks = seq(0,20,1)) +
  #scale_y_continuous(breaks = seq(3,5,0.1)) +
  #geom_vline(xintercept = 10, linetype="dashed", color="red") +
  geom_segment(x = 3, xend = 3, y = 3, yend = val.y, 
               color = "red", linetype="dashed") +
  labs(x="Number of topics", y="Deveaud criterion")

print(plot_deveaud)


lda_model <- ftn$LDA_model[[28]]
lda_model



ap_topics <- tidy(lda_model, matrix = "beta")

top_terms <- ap_topics  %>% # take the topics data frame and..
  group_by(topic) %>% # treat each topic as a different group
  top_n(30, beta) %>% # get the top most informative words
  ungroup() %>% # ungroup
  arrange(topic, -beta) #

wide_top_terms <- top_terms %>%
  select(topic, term) %>%
  group_by(topic) %>%
  mutate(term_id = row_number()) %>%   # Aggiungi un identificatore per i termini
  pivot_wider(names_from = topic, values_from = term, names_sort = TRUE) %>%
  select(-term_id)  # Rimuovi l'identificatore temporaneo


documents <- tidy(lda_model, matrix = "gamma") %>%
  arrange(document, topic)

documents_wide <- documents %>% spread(topic, gamma)
documents_wide <- documents_wide[order(as.integer(documents_wide$document)),]


apply(documents_wide[,-1], 2, max)

#topic 1
ind1 <- which(documents_wide[,2]>0.62)
reviews_neg$comments[ind1]


#topic 2
ind2 <- which(documents_wide[,3]>0.54)
reviews_neg$comments[ind2]
#documents_wide[ind2,]

#topic 3
ind3 <- which(documents_wide[,4]>0.52)
reviews_neg$comments[ind3]


topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}
serVis(topicmodels2LDAvis(lda_model))


apply(documents_wide[,-1], 2, mean)

# LDA SU RECENSIONI POSITIVE ----------------------------------------------

#creazione della document term matrix considerando solo parole con almeno due
#lettere (ossia eliminando parole date da lettere singole)
txt <- reviews_pos$comments_preproc

corpus <- Corpus(VectorSource(txt))
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths=c(2, Inf)))

#rimuovo i termini sparsi
dtm <- removeSparseTerms(dtm, 0.99)


#SCELTA NUMERO TOPICS (SEED 123)
tic()
ftn <- FindTopicsNumber(dtm, 
                        topics=2:30, 
                        metrics=c("Deveaud2014"), 
                        control=list(iter=2700, burnin=500, thin=200,
                                     seed=123),
                        return_models = TRUE)
FindTopicsNumber_plot(ftn)
toc()

save.image("positive123.RData")

load("positive123.RData")

#considero solo topics da 2 a 20
ftn_ridotto <- ftn[11:29, c(1,3)]

FindTopicsNumber_plot(ftn_ridotto)


#modello con 8 topics
lda_model <- ftn$LDA_model[[23]]
lda_model

ap_topics <- tidy(lda_model, matrix = "beta")

top_terms <- ap_topics  %>% # take the topics data frame and..
  group_by(topic) %>% # treat each topic as a different group
  top_n(30, beta) %>% # get the top most informative words
  ungroup() %>% # ungroup
  arrange(topic, -beta) #

wide_top_terms <- top_terms %>%
  select(topic, term) %>%
  group_by(topic) %>%
  mutate(term_id = row_number()) %>%   # Aggiungi un identificatore per i termini
  pivot_wider(names_from = topic, values_from = term, names_sort = TRUE) %>%
  select(-term_id)  # Rimuovi l'identificatore temporaneo


documents <- tidy(lda_model, matrix = "gamma") %>%
  arrange(document, topic)

documents_wide <- documents %>% spread(topic, gamma)
documents_wide <- documents_wide[order(as.integer(documents_wide$document)),]

for(i in 1:8){
  print(wide_top_terms[,i] |> as.vector())
}


topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}
serVis(topicmodels2LDAvis(lda_model))



#modello con 13 topics
lda_model <- ftn$LDA_model[[18]]
lda_model

ap_topics <- tidy(lda_model, matrix = "beta")

top_terms <- ap_topics  %>% # take the topics data frame and..
  group_by(topic) %>% # treat each topic as a different group
  top_n(30, beta) %>% # get the top most informative words
  ungroup() %>% # ungroup
  arrange(topic, -beta) #

wide_top_terms <- top_terms %>%
  select(topic, term) %>%
  group_by(topic) %>%
  mutate(term_id = row_number()) %>%   # Aggiungi un identificatore per i termini
  pivot_wider(names_from = topic, values_from = term, names_sort = TRUE) %>%
  select(-term_id)  # Rimuovi l'identificatore temporaneo


documents <- tidy(lda_model, matrix = "gamma") %>%
  arrange(document, topic)

documents_wide <- documents %>% spread(topic, gamma)
documents_wide <- documents_wide[order(as.integer(documents_wide$document)),]

#write.csv(wide_top_terms, "nomi_topics.csv", row.names = F)
for(i in 1:13){
  print(wide_top_terms[,i] |> as.vector())
}


topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}
serVis(topicmodels2LDAvis(lda_model))


apply(documents_wide[,-1], 2, max)

#TOPIC 1
ind1 <- which(documents_wide[,2]>0.30)
reviews_pos$comments[ind1]

#TOPIC 2
ind2 <- which(documents_wide[,3]>0.24)
reviews_pos$comments[ind2]

#TOPIC 3
ind3 <- which(documents_wide[,4]>0.24)
reviews_pos$comments[ind3]

#TOPIC 4
ind4 <- which(documents_wide[,5]>0.23)
reviews_pos$comments[ind4]

#TOPIC 5
ind5 <- which(documents_wide[,6]>0.27)
reviews_pos$comments[ind5]

#TOPIC 6
ind6 <- which(documents_wide[,7]>0.27)
reviews_pos$comments[ind6]

#TOPIC 7
ind7 <- which(documents_wide[,8]>0.32)
reviews_pos$comments[ind7]

#TOPIC 8
ind8 <- which(documents_wide[,9]>0.33)
reviews_pos$comments[ind8]

#TOPIC 9
ind9 <- which(documents_wide[,10]>0.25)
reviews_pos$comments[ind9]

#TOPIC 10
ind10 <- which(documents_wide[,11]>0.22)
reviews_pos$comments[ind10]

#TOPIC 11
ind11 <- which(documents_wide[,12]>0.3)
reviews_pos$comments[ind11]

#TOPIC 12
ind12 <- which(documents_wide[,13]>0.3)
reviews_pos$comments[ind12]

#TOPIC 13
ind13 <- which(documents_wide[,14]>0.24)
reviews_pos$comments[ind13]





# supervised LDA ----------------------------------------------------------

load("negative123.RData")

#rimuovo tutto quello che non mi serve
rm(corpus, dtm, ftn, reviews_neg, reviews_pos, sentences)
rm(ind_neg, ind_nul, ind_pos, txt, txt_sa)

#a partire dal dataset reviews, che contiene tutte le recensioni e anche il 
#sentiment_score, ricreo la document term matrix completa
txt <- reviews$comments_preproc

corpus <- Corpus(VectorSource(txt))
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths=c(2, Inf)))

#rimuovo i termini sparsi
dtm <- removeSparseTerms(dtm, 0.99)


#conversione document term matrix nel formato richiesto da slda
dtm_m <- as.matrix(dtm)


documents <- lapply(1:nrow(dtm_m), function(i) {
  word_indices <- which(dtm_m[i, ] > 0) - 1  # 0-indexed word identifiers
  word_counts <- dtm_m[i, word_indices + 1]  # Frequencies of those words
  matrix_data <- rbind(word_indices, word_counts)
  # Ensure the matrix is of integer type
  matrix_data <- matrix(as.integer(matrix_data), nrow = 2)
  return(matrix_data)
})

#documents[[1]]

vocab <- colnames(dtm)  # Assuming dtm has column names as vocabulary


response <- reviews$sentiment_score

#iperparametri (li considero fissati)
alpha <- 1 #iperparametro dirichlet per le topic proportions
eta <- 0.1 #iperparametro dirichlet per le topic multinomials
sigma2 <- 0.25

#numero iterazione (testate su un campione)
num.e.iterations <- 100
num.m.iterations <- 50

#1
#esecuzione modello con numero topics=5,6,7
num_topics <- 5:7
slda_list <- list()

for(i in 1:length(num_topics)){
  print(paste("Modello con", num_topics[i], "topics", sep=" "))
  set.seed(123)
  tic()
  slda_model <- slda.em(documents=documents,
                        K=num_topics[i], 
                        vocab=vocab, 
                        num.e.iterations=num.e.iterations, 
                        num.m.iterations=num.m.iterations, 
                        alpha=alpha,
                        eta=eta, 
                        annotations=response, 
                        params=rep(0, num_topics[i]), #inizializzo coefficienti a 0 
                        variance=sigma2, 
                        logistic = FALSE, 
                        #lambda = 10, #serve solo se regularise=TRUE
                        regularise = FALSE, 
                        method = "sLDA", 
                        trace = 0L, 
                        MaxNWts=3000,
                        initial = NULL)
  toc()
  slda_list[[i]] <- slda_model
  save.image("supervisedLDA123.RData")
}


save.image("supervisedLDA123.RData")
#load("supervisedLDA123.RData")


#2
#esecuzione modello con numero topics=5,6,7
num_topics <- 8:11
slda_list <- list()

for(i in 1:length(num_topics)){
  print(paste("Modello con", num_topics[i], "topics", sep=" "))
  set.seed(123)
  tic()
  slda_model <- slda.em(documents=documents,
                        K=num_topics[i], 
                        vocab=vocab, 
                        num.e.iterations=num.e.iterations, 
                        num.m.iterations=num.m.iterations, 
                        alpha=alpha,
                        eta=eta, 
                        annotations=response, 
                        params=rep(0, num_topics[i]), #inizializzo coefficienti a 0 
                        variance=sigma2, 
                        logistic = FALSE, 
                        #lambda = 10, #serve solo se regularise=TRUE
                        regularise = FALSE, 
                        method = "sLDA", 
                        trace = 0L, 
                        MaxNWts=3000,
                        initial = NULL)
  toc()
  slda_list[[i]] <- slda_model
  save.image("supervisedLDA456.RData")
}


load("supervisedLDA456.RData")


#MODELLO CON 9 TOPICS RISULTA ESSERE IL MIGLIORE
slda_model <- slda_list[[2]]


#visualizzo parole più probabili per ciascun topic
Topics <- apply(top.topic.words(slda_model$topics, 30, by.score=TRUE),
                2, paste, collapse=" ")

Topics <- c("Proximity to transport", "Feeling and experience", "Issues",
            "Unit amenities", "Host welcoming", "Unit evaluation",
            "Neighbourhood", "Check-in/out", "Host evaluation")

#creo matrice con le percentuali di topic per ogni documento
document_topics <- slda_model$document_sums
tot <- apply(document_topics, 2, sum)

document_topics <- rbind(document_topics, tot)
rownames(document_topics) <- c(paste("Topic", 1:9, sep=" "), "Sum")

for(i in 1:ncol(document_topics)){
  for(j in 1:(nrow(document_topics)-1)){
    document_topics[j,i] <- document_topics[j,i]/document_topics[nrow(document_topics),i]
  }
}

apply(document_topics, 1, max)

#topic 8 - check-in/out
ind8 <- which(document_topics[8,]>0.95)
reviews$comments[ind8]

#visualizzo coefficienti
coefs <- data.frame(coef(summary(slda_model$model)))

theme_set(theme_bw())

coefs <- cbind(coefs, Topics=factor(Topics, Topics[order(coefs$Estimate)]))

coefs <- coefs[order(coefs$Estimate),]

ggplot(coefs, aes(x=Topics, y=Estimate, colour=Estimate, size=abs(t.value))) +
  geom_errorbar(width=0.5, aes(ymin=Estimate-Std..Error,ymax=Estimate+Std..Error)) + 
  coord_flip()

