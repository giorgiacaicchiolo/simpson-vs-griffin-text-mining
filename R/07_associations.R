source('Pulizia.R')

# Creazione DTM
dtm_s_all <- create_dtm(simpson, stop_words$word, 'S')
dtm_g_all <- create_dtm(griffin, stop_words$word, 'G')
dtm_s     <- create_dtm(simpson_clean_2, c(stop_words$word, stopwords_nostre), 'S')
dtm_g     <- create_dtm(griffin_clean_2, c(stop_words$word, stopwords_nostre), 'G')

gc()


### Associazioni

# Parole d'interesse
specifiche_s <- c(
  "homer", "bart", "marge", "lisa", "mr", "moe", "ned", "krusty", "milhouse",
  "apu", "wiggum", "skinner", "flanders", "smithers", "maggie", "patty",
  "selma", "grandpa", "springfield", "kwik", "plant", "tavern", "duff",
  "krustyburger", "leftorium", "donut", "doh", "excellent", "hidely-ho",
  "nuclear", "mayor", "church"
)

specifiche_g <- c(
  "peter", "lois", "stewie", "brian", "quagmire", "cleveland", "meg", "chris",
  "chicken", "fight", "flashback", "cutaway", "television", "death", "dog",
  "baby", "alcohol", "joe"
)

generiche <- c(
  "school", "job", "work", "family", "money", "love", "problem", "beer", "hate",
  "funny", "stupid", "crazy", "help", "home", "bar", "election", "environment",
  "holiday", "halloween", "drink", "eat", "sleep", "dream", "argue", "laugh",
  "friend", "neighbor", "boss", "student", "teacher", "marriage", "parent",
  "child", "brother", "sister", "enemy", "competition", "angry", "happy", "sad",
  "jealous", "proud", "embarrassed", "smart", "dumb", "genius", "success",
  "failure", "god", "tv", "show", "italy", "plan", "joke", "memory", "battle",
  "friendship", "kill", "sex"
)

parole_interessanti_s <- c(specifiche_s, generiche)
parole_interessanti_g <- c(specifiche_g, generiche)

# Cerca associazioni nell'episodio: è eccessivo!!!
findAssocs(dtm_s_all, "beer", 0.3)
findAssocs(dtm_s, "beer", 0.3)
findAssocs(dtm_s_all, "homer", 0.3)

findAssocs(dtm_g_all, "beer", 0.3)
findAssocs(dtm_g, "beer", 0.2)
findAssocs(dtm_g_all, "peter", 0.3)


# Segmentazione del testo
segmenta_testo_sliding <- function(testo, finestra = 80, passo = 30) {
  parole <- unlist(tokenize_words(testo))
  n <- length(parole)
  segmenti <- lapply(seq(1, n - finestra + 1, by = passo), function(i) {
    paste(parole[i:(i + finestra - 1)], collapse = " ")
  })
  return(segmenti)
}


# Segmentazione e creazione DTM
crea_dtm_segmenti <- function(df) {
  df_seg <- df %>%
    mutate(testo = str_remove_all(testo, "\\d+")) %>%  # Rimuove numeri
    rowwise() %>%
    mutate(segmenti = list(segmenta_testo_sliding(testo))) %>%
    unnest(segmenti) %>%
    mutate(segmento_id = row_number()) %>%
    ungroup()
  
  corpus <- VCorpus(VectorSource(df_seg$segmenti))
  rm(df_seg)
  gc()
  return(DocumentTermMatrix(corpus))
}

# DTM segmentato per FindAssocs
dtm_s_s <- crea_dtm_segmenti(simpson_clean_2)
dtm_g_s <- crea_dtm_segmenti(griffin_clean_2)


# Trova associazioni con soglia minima
trova_associazioni <- function(dtm, parole, corlimit = 0.25) {
  assoc_list <- list()
  for (i in seq_along(parole)) {
    parola <- parole[i]
    cat(sprintf("'%s', parola %d/%d \n", parola, i, length(parole)))
    if (parola %in% Terms(dtm)) {
      assoc <- findAssocs(dtm, parola, corlimit)
      if (length(assoc[[1]]) > 0) assoc_list[[parola]] <- assoc[[1]]
    }
  }
  return(assoc_list)
}

#Estrai le associazioni interessanti

associazioni_s <- trova_associazioni(dtm_s_s, parole_interessanti_s, corlimit = 0.25)
associazioni_g <- trova_associazioni(dtm_g_s, parole_interessanti_g, corlimit = 0.25)

associazioni_s
associazioni_g

rm(
  dtm_s, dtm_g,
  dtm_s_s, dtm_g_s,
  dtm_s_all, dtm_g_all,
  specifiche_s, specifiche_g, generiche,
  parole_interessanti_s, parole_interessanti_g,
  segmenta_testo_sliding, trova_associazioni,
  crea_dtm_segmenti
)
gc()