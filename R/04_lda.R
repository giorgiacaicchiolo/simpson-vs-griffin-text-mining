source('Pulizia.R')

# preparazione DTM ####
# DTM
dtm_s = create_dtm(simpson_clean, prefix = 'S')
dtm_g = create_dtm(griffin_clean, prefix = 'G')
dtm_s2 = create_dtm(simpson_clean_2, prefix = 'S')
dtm_g2 = create_dtm(griffin_clean_2, prefix = 'G')

# Aggiungi il prefisso e unisci
simpson_clean <- simpson_clean %>% mutate(prefix = "S")
griffin_clean <- griffin_clean %>% mutate(prefix = "G")
dati_dtm_tot <- bind_rows(simpson_clean, griffin_clean)

simpson_clean_2 <- simpson_clean_2 %>% mutate(prefix = "S")
griffin_clean_2 <- griffin_clean_2 %>% mutate(prefix = "G")
dati_dtm_tot2 <- bind_rows(simpson_clean_2, griffin_clean_2)

# Crea la DTM
dtm_tot <- create_dtm(dati_dtm_tot)
dtm_tot2 <- create_dtm(dati_dtm_tot2)

rm(dati_dtm_tot, dati_dtm_tot2)
gc()


### LDA con unico df con simposn e griffin ####

# Imposta seed globale
set.seed(42)

params <- list(k = 2:10)

# Imposta parallelizzazione (Windows: multisession)
plan(multisession, workers = future::availableCores()/2)

run_lda <- function(k, dtm) {
  cat("Avvio stima modello con k =", k, "\n")
  modello <- topicmodels::LDA(dtm,
                              k = k,
                              method = "Gibbs",
                              control = list(seed = 42,   # Fisso dentro per sicurezza
                                             burnin = 5000,
                                             iter = 10000,
                                             thin = 500,
                                             verbose = 0))
  cat("Finito modello con k =", k, "\n")
  return(modello)
}

# Esegui parallelamente. Lista con elemento k = lda con k topic
lda_k <- c(
  list(NULL),
  future_lapply(params$k, function(k) run_lda(k, dtm_tot), future.seed = TRUE)
)

lda_k2 <- c(
  list(NULL),
  future_lapply(params$k, function(k) run_lda(k, dtm_tot2), future.seed = TRUE)
)

rm(params)
gc()


lda_results <- analyze_lda(lda_k, lda_k2, k = 2)
plot_topics(data = tot2_topics_2, num_topics = 2, prefix = 'S')

lda_results <- analyze_lda(lda_k, lda_k2, k = 3)
plot_topics(data = tot3_topics_2, num_topics = 2, prefix = 'S')

lda_results <- analyze_lda(lda_k, lda_k2, k = 4)
plot_topics(data = tot4_topics_2, num_topics = 3, prefix = 'S')

lda_results <- analyze_lda(lda_k, lda_k2, k = 5)
plot_topics(data = tot5_topics_2, num_topics = 4, prefix = 'S')

lda_results <- analyze_lda(lda_k, lda_k2, k = 6)
plot_topics(data = tot6_topics_2, num_topics = 5, prefix = 'S')

lda_results <- analyze_lda(lda_k, lda_k2, k = 7)
plot_topics(data = tot7_topics_2, num_topics = 6, prefix = 'S')

lda_results <- analyze_lda(lda_k, lda_k2, k = 8)
plot_topics(data = tot8_topics_2, num_topics = 7, prefix = 'S')

lda_results <- analyze_lda(lda_k, lda_k2, k = 9)
plot_topics(data = tot9_topics_2, num_topics = 8, prefix = 'S')

lda_results <- analyze_lda(lda_k, lda_k2, k = 10)
plot_topics(data = tot10_topics_2, num_topics = 9, prefix = 'S')

# Trova il numero ottimale di topic, prova da 5 a 70 topic con passo 5
result_tot <- FindTopicsNumber(
  dtm_tot2,
  topics = seq(5, 70, by = 5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = detectCores()/2,
  verbose = TRUE
)

# Visualizza i risultati
FindTopicsNumber_plot(result_tot)

# 25. Ma sono troppi, quindi valutiamo


rm(result, lda_results)
gc()

### Solo per i Simpson ####
# Imposta seed globale
set.seed(42)

params <- list(k = 2:10)

# Imposta parallelizzazione (Windows: multisession)
plan(multisession, workers = future::availableCores()/2)

# Esegui parallelamente
lda_k_s <- c(
  list(NULL),
  future_lapply(params$k, function(k) run_lda(k, dtm_s), future.seed = TRUE)
)

lda_k_s2 <- c(
  list(NULL),
  future_lapply(params$k, function(k) run_lda(k, dtm_s2), future.seed = TRUE)
)

save(lda_k_s, file="data/lda_k_s.RData")
save(lda_k_s2, file="data/lda_k_s2.RData")

rm(params)
gc()

# Risultati Simpson ####
lda_results <- analyze_lda_serie(lda_k_s, lda_k_s2, k = 2, n_top_terms = 20, serie_nome = 'Simpson')
plot_topics(data = tot2_topics_2, num_topics = 2)

lda_results <- analyze_lda_serie(lda_k_s, lda_k_s2, k = 3, n_top_terms = 20, serie_nome = 'Simpson')
plot_topics(data = tot3_topics_2, num_topics = 3)

lda_results <- analyze_lda_serie(lda_k_s, lda_k_s2, k = 4, n_top_terms = 20, serie_nome = 'Simpson')
plot_topics(data = tot4_topics_2, num_topics = 4)

lda_results <- analyze_lda_serie(lda_k_s, lda_k_s2, k = 5, n_top_terms = 20, serie_nome = 'Simpson')
plot_topics(data = tot5_topics_2, num_topics = 5)

lda_results <- analyze_lda_serie(lda_k_s, lda_k_s2, k = 6, n_top_terms = 20, serie_nome = 'Simpson')
plot_topics(data = tot6_topics_2, num_topics = 6)

lda_results <- analyze_lda_serie(lda_k_s, lda_k_s2, k = 7, n_top_terms = 20, serie_nome = 'Simpson')
plot_topics(data = tot7_topics_2, num_topics = 7)

lda_results <- analyze_lda_serie(lda_k_s, lda_k_s2, k = 8, n_top_terms = 20, serie_nome = 'Simpson')
plot_topics(data = tot8_topics_2, num_topics = 8)

lda_results <- analyze_lda_serie(lda_k_s, lda_k_s2, k = 9, n_top_terms = 20, serie_nome = 'Simpson')
plot_topics(data = tot9_topics_2, num_topics = 9)

lda_results <- analyze_lda_serie(lda_k_s, lda_k_s2, k = 10, n_top_terms = 20, serie_nome = 'Simpson')
plot_topics(data = tot10_topics_2, num_topics = 10)

# Trova il numero ottimale di topic, prova da 5 a 70 topic con passo 5
result_s <- FindTopicsNumber(
  dtm_s2,
  topics = seq(5, 70, by = 5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = detectCores()/2,
  verbose = TRUE
)

# Visualizza i risultati
FindTopicsNumber_plot(result_s)

# Anche qui 25 ma sono probabilmente troppi

rm(result, lda_results)
gc()

### Solo per i Griffin ####
# Imposta seed globale
set.seed(42)

params <- list(k = 2:10)

# Imposta parallelizzazione (Windows: multisession)
plan(multisession, workers = future::availableCores()/2)

# Esegui parallelamente
lda_k_g <- c(
  list(NULL),
  future_lapply(params$k, function(k) run_lda(k, dtm_g), future.seed = TRUE)
)

lda_k_g2 <- c(
  list(NULL),
  future_lapply(params$k, function(k) run_lda(k, dtm_g2), future.seed = TRUE)
)

save(c(lda_k_g, lda_k_g2), file="data/LDA_Griffin_Risultati.RData")

rm(params)
gc()

# Risultati griffin ####
lda_results <- analyze_lda_serie(lda_k_g, lda_k_g2, k = 2, n_top_terms = 20, serie_nome = 'Griffin')
plot_topics(data = tot2_topics_2, num_topics = 2)

lda_results <- analyze_lda_serie(lda_k_g, lda_k_g2, k = 3, n_top_terms = 20, serie_nome = 'Griffin')
plot_topics(data = tot3_topics_2, num_topics = 3)

lda_results <- analyze_lda_serie(lda_k_g, lda_k_g2, k = 4, n_top_terms = 20, serie_nome = 'Griffin')
plot_topics(data = tot4_topics_2, num_topics = 4)

lda_results <- analyze_lda_serie(lda_k_g, lda_k_g2, k = 5, n_top_terms = 20, serie_nome = 'Griffin')
plot_topics(data = tot5_topics_2, num_topics = 5)

lda_results <- analyze_lda_serie(lda_k_g, lda_k_g2, k = 6, n_top_terms = 20, serie_nome = 'Griffin')
plot_topics(data = tot6_topics_2, num_topics = 6)

lda_results <- analyze_lda_serie(lda_k_g, lda_k_g2, k = 7, n_top_terms = 20, serie_nome = 'Griffin')
plot_topics(data = tot7_topics_2, num_topics = 7)

lda_results <- analyze_lda_serie(lda_k_g, lda_k_g2, k = 8, n_top_terms = 20, serie_nome = 'Griffin')
plot_topics(data = tot8_topics_2, num_topics = 8)

lda_results <- analyze_lda_serie(lda_k_g, lda_k_g2, k = 9, n_top_terms = 20, serie_nome = 'Griffin')
plot_topics(data = tot9_topics_2, num_topics = 9)

lda_results <- analyze_lda_serie(lda_k_g, lda_k_g2, k = 10, n_top_terms = 20, serie_nome = 'Griffin')
plot_topics(data = tot10_topics_2, num_topics = 10)

# Trova il numero ottimale di topic, prova da 5 a 70 topic con passo 5
result_g <- FindTopicsNumber(
  dtm_g2,
  topics = seq(5, 70, by = 5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = detectCores()/2,
  verbose = TRUE
)

# Visualizza i risultati
FindTopicsNumber_plot(result_g)

# Già 15 sembra buono, conferma la minore varietà dei Griffin

rm(lda_results)

rm(dtm_g, dtm_g2, dtm_s, dtm_s2, dtm_tot, dtm_tot2)
gc()

# analisi degli episodi più significativi di ogni topic
#scelgo se usare simpson e griffin insieme o separati,
#il numero di topic e se usare la lda con o senza i personaggi in base alle parole più significative
#salvo su un oggetto
lda_results_s7 <- analyze_lda_serie(lda_k_s, lda_k_s2, k = 7, n_top_terms = 20, serie_nome = 'Simpson')
plot_topics(data = tot7_topics_2, num_topics = 7)

#creo id documento su imdb
imdb$doc_id <- paste0(imdb$serie,"_S", ifelse(imdb$stagione < 10, paste0("0", imdb$stagione), imdb$stagione),
                      "_E", ifelse(imdb$episodio < 10, paste0("0", imdb$episodio), imdb$episodio))

#faccio inner join con i riassunti di imbd
lda_imdb_s7 <- lda_results_s7$topics_2 %>% inner_join(imdb, by = c("document" = "doc_id"))

#guardo i riassunti per ogni gruppo (--> da cambiare il filter)
lda_imdb_s7 %>% filter(topic == 7) %>% arrange(desc(gamma)) %>% select(riassunto) %>% print()

#cronaca, scuola (lisa), relazioni coppia (marge), bart, televisione, lavoro, homer tempo libero
