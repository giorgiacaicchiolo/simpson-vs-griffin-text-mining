library(patchwork)
# nome risultati lda ####
# Totale ####
tot9_topics <- analyze_lda(lda_k, lda_k2, k = 9, n_top_terms = 40)
tot9_topics$topics_2 <- tot9_topics$topics_2 %>% inner_join(imdb, by = c("document" = "doc_id"))
tot9_topics$topics_1 <- tot9_topics$topics_1 %>% inner_join(imdb, by = c("document" = "doc_id"))

titoli_topic_tot <- c(
  "Famiglia", 
  "Vita domestica e festività", 
  "Relazioni sociali", 
  "Cronaca", 
  "Relazione matrimoniale", 
  "Istruzione e scuola", 
  "Tempo libero", 
  "Conversazioni", 
  "Lavoro e finanza")

titoli_topic_tot_2 <- c(
  "Esclamazioni",
  "Famiglia Griffin", 
  "Dinamiche familiari (generale)",  
  "Amore, relazioni e felicità", 
  "Vita quotidiana e attività domestiche",
  "Attività generiche",
  "Scuola", 
  "Finanza",  
  "Famiglia Simpson") 

tot9_topics$top_terms_1$titolo_topic <- titoli_topic_tot[tot9_topics$top_terms_1$topic]
tot9_topics$top_terms_2$titolo_topic <- titoli_topic_tot_2[tot9_topics$top_terms_2$topic]
tot9_topics$top_docs_1$titolo_topic <- titoli_topic_tot[tot9_topics$top_docs_1$topic]
tot9_topics$top_docs_2$titolo_topic <- titoli_topic_tot_2[tot9_topics$top_docs_2$topic]
tot9_topics$topics_1$titolo_topic <- titoli_topic_tot[tot9_topics$topics_1$topic]
tot9_topics$topics_2$titolo_topic <- titoli_topic_tot_2[tot9_topics$topics_2$topic]

save(tot9_topics, file="data/tot9_topic.RData")

# Simpson ####
tot7_topics <- analyze_lda_serie(lda_k_s, lda_k_s2, k = 7, n_top_terms = 40, serie_nome = "Simpson")

#tot7_topics$topics_2 %>% filter(topic == 1) %>% arrange(desc(gamma)) %>% select(riassunto) %>% print()

tot7_topics$topics_2 <- tot7_topics$topics_2 %>% inner_join(imdb, by = c("document" = "doc_id"))
tot7_topics$topics_1 <- tot7_topics$topics_1 %>% inner_join(imdb, by = c("document" = "doc_id"))

titoli_topic_s <- c("Fede e Vita Quotidiana",
                    "Amore e Relazioni",  #okk
                    "Amicizia e tempo libero", #okk
                    "Vita domestica e festività", #okk
                    "Famiglia", #okk
                    "Scuola", #okk
                    "Economia e Finanza")

titoli_topic_s2 <- c("Conversazioni",
                     "Lisa e scuola", #2
                     "Relazione matrimoniale tra Homer e Marge", #3
                     "Bart", #4
                     "Televisione e spettacolo", #5
                     "Homer e lavoro", 
                     "Homer e tempo libero")

tot7_topics$top_terms_1$titolo_topic <- titoli_topic_s[tot7_topics$top_terms_1$topic]
tot7_topics$top_terms_2$titolo_topic <- titoli_topic_s2[tot7_topics$top_terms_2$topic]
tot7_topics$top_docs_1$titolo_topic <- titoli_topic_s[tot7_topics$top_docs_1$topic]
tot7_topics$top_docs_2$titolo_topic <- titoli_topic_s2[tot7_topics$top_docs_2$topic]
tot7_topics$topics_1$titolo_topic <- titoli_topic_s[tot7_topics$topics_1$topic]
tot7_topics$topics_2$titolo_topic <- titoli_topic_s2[tot7_topics$topics_2$topic]

save(tot7_topics, file="data/tot7_topic.RData")

# Griffin ####
tot5_topics <- analyze_lda_serie(lda_k_g, lda_k_g2, k = 5, n_top_terms = 40, serie_nome = "Griffin")

tot5_topics$topics_2 <- tot5_topics$topics_2 %>% inner_join(imdb, by = c("document" = "doc_id"))
tot5_topics$topics_1 <- tot5_topics$topics_1 %>% inner_join(imdb, by = c("document" = "doc_id"))

titoli_topic_g <- c("Famiglia", 
                    "Società e news", 
                    "Relazioni", 
                    "Tempo libero", 
                    "Violenza")
titoli_topic_g2 <- c("Personaggi secondari", 
                     "Peter e Lois", 
                     "Dinamiche familiari", 
                     "Media e società", 
                     "Brian e Stewie")

tot5_topics$top_terms_1$titolo_topic <- titoli_topic_g[tot5_topics$top_terms_1$topic]
tot5_topics$top_terms_2$titolo_topic <- titoli_topic_g2[tot5_topics$top_terms_2$topic]
tot5_topics$top_docs_1$titolo_topic <- titoli_topic_g[tot5_topics$top_docs_1$topic]
tot5_topics$top_docs_2$titolo_topic <- titoli_topic_g2[tot5_topics$top_docs_2$topic]
tot5_topics$topics_1$titolo_topic <- titoli_topic_g[tot5_topics$topics_1$topic]
tot5_topics$topics_2$titolo_topic <- titoli_topic_g2[tot5_topics$topics_2$topic]

save(tot5_topics, file="data/tot5_topic.RData")

# Analisi Sentiment per topic ####
# totale ####
tot_clean <- bind_rows(simpson_clean %>% mutate(serie = rep("S", nrow(simpson_clean))), 
                       griffin_clean %>% mutate(serie = rep("G", nrow(griffin_clean))))
tot_clean_2 <- bind_rows(simpson_clean_2 %>% mutate(serie = rep("S", nrow(simpson_clean_2))), 
                         griffin_clean_2 %>% mutate(serie = rep("G", nrow(griffin_clean_2))))

token_tot <- tot_clean %>% unnest_tokens(word, testo)
token_tot_2 <- tot_clean_2 %>% unnest_tokens(word, testo)

sentiment_tot <- token_tot %>%
  left_join(lexicon, by = "word") %>%
  group_by(serie, stagione, episodio) %>%
  summarise(
    sentiment = mean(value, na.rm = TRUE),
    n_words = n()
  ) %>%
  filter(n_words > 10) 

sentiment_tot_2 <- token_tot_2 %>%
  left_join(lexicon, by = "word") %>%
  group_by(serie, stagione, episodio) %>%
  summarise(
    sentiment = mean(value, na.rm = TRUE),
    n_words = n()
  ) %>%
  filter(n_words > 10) 

sentiment_tot$doc_id <- paste0(sentiment_tot$serie, "_S", ifelse(sentiment_tot$stagione < 10, paste0("0", sentiment_tot$stagione), sentiment_tot$stagione), 
                             "_E", ifelse(sentiment_tot$episodio < 10, paste0("0", sentiment_tot$episodio), sentiment_tot$episodio))
sentiment_tot_2$doc_id <- paste0(sentiment_tot_2$serie, "_S", ifelse(sentiment_tot_2$stagione < 10, paste0("0", sentiment_tot_2$stagione), sentiment_tot_2$stagione), 
                              "_E", ifelse(sentiment_tot_2$episodio < 10, paste0("0", sentiment_tot_2$episodio), sentiment_tot_2$episodio))

lda_imdb_tot <- tot9_topics$topics_1 %>% inner_join(imdb %>% select(c("rating", "doc_id")), by = c("document" = "doc_id"))
lda_imdb_tot_2 <- tot9_topics$topics_2 %>% inner_join(imdb %>% select(c("rating", "doc_id")), by = c("document" = "doc_id"))

sentiment_lda_imdb_tot <- sentiment_tot %>% inner_join(lda_imdb_tot %>% select(-c(stagione, episodio)), by = c("doc_id" = "document"))
sentiment_lda_imdb_tot_2 <- sentiment_tot_2 %>% inner_join(lda_imdb_tot_2 %>% select(-c(stagione, episodio)), by = c("doc_id" = "document"))

sentiment_lda_imdb_tot$titolo_topic <- titoli_topic_tot[sentiment_lda_imdb_tot$topic]
sentiment_lda_imdb_tot_2$titolo_topic <- titoli_topic_tot_2[sentiment_lda_imdb_tot_2$topic]

#sentiment medio dei topic
sentiment_lda_imdb_tot %>%
  group_by(topic) %>%
  summarise(sentiment = mean(sentiment))

sentiment_lda_imdb_tot_2 %>%
  group_by(topic) %>%
  summarise(sentiment = mean(sentiment))

# boxplot topic ####
ggplot(sentiment_lda_imdb_tot, aes(x = factor(titolo_topic), y = sentiment)) +
  geom_boxplot(fill = "#2ECC71", color = "black", alpha = 0.7) +
  labs(
    title = "Distribuzione del sentiment per topic senza personaggi",
    x = "Topic",
    y = "Sentiment medio (episodi)"
  ) +
  theme_minimal()

ggplot(sentiment_lda_imdb_tot_2, aes(x = factor(titolo_topic), y = sentiment)) +
  geom_boxplot(fill = "#2ECC71", color = "black", alpha = 0.7) +
  labs(
    title = "Distribuzione del sentiment per topic con personaggi",
    x = "Topic",
    y = "Sentiment medio (episodi)"
  ) +
  theme_minimal()

# andamento per anno ####
anno = c(1989:2024, 1999, 1999, 2001, 2005:2024)
epi_per_stag = c(
  rle(simpson_clean$stagione)$lengths,
  rle(griffin_clean$stagione)$lengths
)

anni <- data.frame(anno = rep(anno, epi_per_stag), 
                   serie = c(rep("S", nrow(simpson_clean)), rep("G", nrow(griffin_clean))),
                   stagione = tot_clean$stagione,
                   episodio = tot_clean$episodio)

anni$doc_id <- paste0(anni$serie, "_S", ifelse(anni$stagione < 10, paste0("0", anni$stagione), anni$stagione), 
                                 "_E", ifelse(anni$episodio < 10, paste0("0", anni$episodio), anni$episodio))

sentiment_lda_imdb_tot <- sentiment_lda_imdb_tot %>% inner_join(anni, by = c("doc_id" = "doc_id"))
sentiment_lda_imdb_tot_2 <- sentiment_lda_imdb_tot_2 %>% inner_join(anni, by = c("doc_id" = "doc_id"))


sentiment_lda_imdb_tot %>%
  group_by(titolo_topic, anno) %>%
  summarise(sentiment_medio = mean(sentiment), .groups = "drop") %>%
  ggplot(aes(x = anno, y = sentiment_medio)) +
  geom_line(color = "#2ECC71", size = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ titolo_topic, ncol = 3) +
  labs(
    title = "Andamento del sentiment per anno, diviso per topic",
    x = "Anno",
    y = "Sentiment medio"
  ) +
  theme_minimal()

sentiment_lda_imdb_tot_2 %>%
  group_by(titolo_topic, anno) %>%
  summarise(sentiment_medio = mean(sentiment), .groups = "drop") %>%
  ggplot(aes(x = anno, y = sentiment_medio)) +
  geom_line(color = "#2ECC71", size = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ titolo_topic, ncol = 3, labeller = labeller(topic = titoli_topic_s2)) +
  labs(
    title = "Andamento del sentiment per anno, diviso per topic",
    x = "Anno",
    y = "Sentiment medio"
  ) +
  theme_minimal()

#praticamente non ci sono differenze tra con e senza personaggi, bene!!

# Simpson ####
token_simpson <- simpson_clean %>% unnest_tokens(word, testo)
token_simpson_2 <- simpson_clean_2 %>% unnest_tokens(word, testo)

sentiment_s <- token_simpson %>%
  left_join(lexicon, by = "word") %>%
  group_by(stagione, episodio) %>%
  summarise(
    sentiment = mean(value, na.rm = TRUE),
    n_words = n()
  ) %>%
  filter(n_words > 10) 

sentiment_s2 <- token_simpson_2 %>%
  left_join(lexicon, by = "word") %>%
  group_by(stagione, episodio) %>%
  summarise(
    sentiment = mean(value, na.rm = TRUE),
    n_words = n()
  ) %>%
  filter(n_words > 10) 

sentiment_s$doc_id <- paste0("S_S", ifelse(sentiment_s$stagione < 10, paste0("0", sentiment_s$stagione), sentiment_s$stagione), 
                             "_E", ifelse(sentiment_s$episodio < 10, paste0("0", sentiment_s$episodio), sentiment_s$episodio))
sentiment_s2$doc_id <- paste0("S_S", ifelse(sentiment_s2$stagione < 10, paste0("0", sentiment_s2$stagione), sentiment_s2$stagione), 
                              "_E", ifelse(sentiment_s2$episodio < 10, paste0("0", sentiment_s2$episodio), sentiment_s2$episodio))

lda_imdb_s <- tot7_topics$topics_1 %>% inner_join(imdb %>% select(c("rating", "doc_id")), by = c("document" = "doc_id"))
lda_imdb_s2 <- tot7_topics$topics_2 %>% inner_join(imdb %>% select(c("rating", "doc_id")), by = c("document" = "doc_id"))
sentiment_lda_imdb_s <- sentiment_s %>% inner_join(lda_imdb_s %>% select(-c(stagione, episodio)), by = c("doc_id" = "document"))
sentiment_lda_imdb_s2 <- sentiment_s2 %>% inner_join(lda_imdb_s2 %>% select(-c(stagione, episodio)), by = c("doc_id" = "document"))

#sentiment medio dei topic
sentiment_lda_imdb_s %>%
  group_by(topic) %>%
  summarise(sentiment = mean(sentiment))

sentiment_lda_imdb_s2 %>%
  group_by(topic) %>%
  summarise(sentiment = mean(sentiment))

# boxplot topic ####
ggplot(sentiment_lda_imdb_s, aes(x = factor(titolo_topic), y = sentiment)) +
  geom_boxplot(fill = "#FFD700", color = "black", alpha = 0.7) +
  labs(
    title = "Distribuzione del sentiment per topic senza personaggi",
    x = "Topic",
    y = "Sentiment medio (episodi)"
  ) +
  theme_minimal()

ggplot(sentiment_lda_imdb_s2, aes(x = factor(titolo_topic), y = sentiment)) +
  geom_boxplot(fill = "#FFD700", color = "black", alpha = 0.7) +
  labs(
    title = "Distribuzione del sentiment per topic con personaggi",
    x = "Topic",
    y = "Sentiment medio (episodi)"
  ) +
  theme_minimal()

# andamento per stagione ####
sentiment_lda_imdb_s %>%
  group_by(titolo_topic, stagione) %>%
  summarise(sentiment_medio = mean(sentiment), .groups = "drop") %>%
  ggplot(aes(x = stagione, y = sentiment_medio)) +
  geom_line(color = "#FFD700", size = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ titolo_topic, ncol = 3) +
  labs(
    title = "Andamento del sentiment per stagione, diviso per topic",
    x = "Stagione",
    y = "Sentiment medio"
  ) +
  theme_minimal()

sentiment_lda_imdb_s2 %>%
  group_by(titolo_topic, stagione) %>%
  summarise(sentiment_medio = mean(sentiment), .groups = "drop") %>%
  ggplot(aes(x = stagione, y = sentiment_medio)) +
  geom_line(color = "#FFD700", size = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ titolo_topic, ncol = 3, labeller = labeller(topic = titoli_topic_s2)) +
  labs(
    title = "Andamento del sentiment per stagione, diviso per topic",
    x = "Stagione",
    y = "Sentiment medio"
  ) +
  theme_minimal()

#praticamente non ci sono differenze tra con e senza personaggi, bene!!
# Griffin ####
token_griffin <- griffin_clean %>% unnest_tokens(word, testo)
token_griffin_2 <- griffin_clean_2 %>% unnest_tokens(word, testo)

sentiment_g <- token_griffin %>%
  left_join(lexicon, by = "word") %>%
  group_by(stagione, episodio) %>%
  summarise(
    sentiment = mean(value, na.rm = TRUE),
    n_words = n()
  ) %>%
  filter(n_words > 10) 

sentiment_g2 <- token_griffin_2 %>%
  left_join(lexicon, by = "word") %>%
  group_by(stagione, episodio) %>%
  summarise(
    sentiment = mean(value, na.rm = TRUE),
    n_words = n()
  ) %>%
  filter(n_words > 10) 

sentiment_g$doc_id <- paste0("G_S", ifelse(sentiment_g$stagione < 10, paste0("0", sentiment_g$stagione), sentiment_g$stagione), 
                             "_E", ifelse(sentiment_g$episodio < 10, paste0("0", sentiment_g$episodio), sentiment_g$episodio))
sentiment_g2$doc_id <- paste0("G_S", ifelse(sentiment_g2$stagione < 10, paste0("0", sentiment_g2$stagione), sentiment_g2$stagione), 
                              "_E", ifelse(sentiment_g2$episodio < 10, paste0("0", sentiment_g2$episodio), sentiment_g2$episodio))

lda_imdb_g <- tot5_topics$topics_1 %>% inner_join(imdb %>% select(c("rating", "doc_id")), by = c("document" = "doc_id"))
lda_imdb_g2 <- tot5_topics$topics_2 %>% inner_join(imdb %>% select(c("rating", "doc_id")), by = c("document" = "doc_id"))
sentiment_lda_imdb_g <- sentiment_g %>% inner_join(lda_imdb_g %>% select(-c(stagione, episodio)), by = c("doc_id" = "document"))
sentiment_lda_imdb_g2 <- sentiment_g2 %>% inner_join(lda_imdb_g2 %>% select(-c(stagione, episodio)), by = c("doc_id" = "document"))

#sentiment medio dei topic
sentiment_lda_imdb_g %>%
  group_by(topic) %>%
  summarise(sentiment = mean(sentiment))

sentiment_lda_imdb_g2 %>%
  group_by(topic) %>%
  summarise(sentiment = mean(sentiment))

# boxplot topic ####
ggplot(sentiment_lda_imdb_g, aes(x = factor(titolo_topic), y = sentiment)) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribuzione del sentiment per topic",
    x = "Topic",
    y = "Sentiment medio (episodi)"
  ) +
  theme_minimal()

ggplot(sentiment_lda_imdb_g2, aes(x = factor(titolo_topic), y = sentiment)) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribuzione del sentiment per topic",
    x = "Topic",
    y = "Sentiment medio (episodi)"
  ) +
  theme_minimal()

# andamento per stagione ####
sentiment_lda_imdb_g %>%
  group_by(titolo_topic, stagione) %>%
  summarise(sentiment_medio = mean(sentiment), .groups = "drop") %>%
  ggplot(aes(x = stagione, y = sentiment_medio)) +
  geom_line(color = "lightblue", size = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ titolo_topic, ncol = 3) +
  labs(
    title = "Andamento del sentiment per stagione, diviso per topic",
    x = "Stagione",
    y = "Sentiment medio"
  ) +
  theme_minimal()

sentiment_lda_imdb_g2 %>%
  group_by(titolo_topic, stagione) %>%
  summarise(sentiment_medio = mean(sentiment), .groups = "drop") %>%
  ggplot(aes(x = stagione, y = sentiment_medio)) +
  geom_line(color = "lightblue", size = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ titolo_topic, ncol = 3) +
  labs(
    title = "Andamento del sentiment per stagione, diviso per topic",
    x = "Stagione",
    y = "Sentiment medio"
  ) +
  theme_minimal()

#praticamente non ci sono differenze tra con e senza personaggi, bene!!
# Confronto Topic simili Simpson e Griffin ####
titoli_topic_s <- c("Fede e Vita Quotidiana", "Amore e Relazioni", "Economia e Finanza", 
                    "Vita domestica e festività", "Famiglia", "Scuola", "Amicizia e tempo libero")
titoli_topic_s2 <- c("Conversazioni", "Lisa e scuola", "Relazione matrimoniale tra Homer e Marge", 
                     "Bart", "Televisione e spettacolo", "Homer e lavoro", "Homer e tempo libero")
titoli_topic_g <- c("Famiglia", "Società e news", "Relazioni", "Tempo libero", "Violenza")
titoli_topic_g2 <- c("Personaggi secondari", "Peter e Lois", "Dinamiche familiari", "Media e società", 
                     "Brian e Stewie")

# Famiglia: S 5, G 1, (G2 3) ####
b1 <- ggplot(bind_rows(sentiment_lda_imdb_s %>% filter(topic == 5), sentiment_lda_imdb_g %>% filter(topic == 1)),
             aes(x = serie, y = sentiment, fill = serie)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Confronto sentiment: Topic \"Famiglia\"",
    x = "Serie",
    y = "Sentiment"
  ) +
  scale_fill_manual(values = c("#FFD700", "#1E90FF")) +  # giallo e blu
  theme_minimal()

a1 <- bind_rows(sentiment_lda_imdb_s %>% filter(topic == 5), 
                sentiment_lda_imdb_g %>% filter(topic == 1) %>% mutate(stagione = stagione + 13)) %>%
  group_by(serie, stagione) %>%
  summarise(sentiment_medio = mean(sentiment, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = stagione, y = sentiment_medio, color = factor(serie), group = serie)) +
  geom_line(size = 0.6) +
  geom_point(size = 2) +
  labs(
    title = "Andamento del sentiment: Topic \"Famiglia\"",
    x = "Stagione",
    y = "Sentiment medio",
    color = "Serie"
  ) +
  scale_color_manual(values = c("S" = "#FFD700", "G" = "#1E90FF")) +  # Oro e Blu
  theme_minimal()

b1 + a1 + plot_layout(ncol = 2)

# Coppia: S2, G3 ####
b1 <- ggplot(bind_rows(sentiment_lda_imdb_s %>% filter(topic == 2), sentiment_lda_imdb_g %>% filter(topic == 3)),
             aes(x = serie, y = sentiment, fill = serie)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Confronto sentiment senza personaggi: Topic \"Coppia\"",
    x = "Serie",
    y = "Sentiment"
  ) +
  scale_fill_manual(values = c("#FFD700", "#1E90FF")) +  # giallo e blu
  theme_minimal()

a1 <- bind_rows(sentiment_lda_imdb_s %>% filter(topic == 2), 
                sentiment_lda_imdb_g %>% filter(topic == 3) %>% mutate(stagione = stagione + 13)) %>%
  group_by(serie, stagione) %>%
  summarise(sentiment_medio = mean(sentiment, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = stagione, y = sentiment_medio, color = factor(serie), group = serie)) +
  geom_line(size = 0.6) +
  geom_point(size = 2) +
  labs(
    title = "Andamento del sentiment senza personaggi: Topic \"Coppia\"",
    x = "Stagione",
    y = "Sentiment medio",
    color = "Serie"
  ) +
  scale_color_manual(values = c("S" = "#FFD700", "G" = "#1E90FF")) +  # Oro e Blu
  theme_minimal()

b1 + a1 + plot_layout(ncol = 2)

# Homer e Marge vs Peter e Lois S2 3, G2 2####
b1 <- ggplot(bind_rows(sentiment_lda_imdb_s2 %>% filter(topic == 3), sentiment_lda_imdb_g2 %>% filter(topic == 2)),
             aes(x = serie, y = sentiment, fill = serie)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Confronto sentiment senza personaggi: Topic \"Coppia\"",
    x = "Serie",
    y = "Sentiment"
  ) +
  scale_fill_manual(values = c("#FFD700", "#1E90FF")) +  # giallo e blu
  theme_minimal()

a1 <- bind_rows(sentiment_lda_imdb_s2 %>% filter(topic == 3), 
                sentiment_lda_imdb_g2 %>% filter(topic == 2) %>% mutate(stagione = stagione + 13)) %>%
  group_by(serie, stagione) %>%
  summarise(sentiment_medio = mean(sentiment, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = stagione, y = sentiment_medio, color = factor(serie), group = serie)) +
  geom_line(size = 0.6) +
  geom_point(size = 2) +
  labs(
    title = "Andamento del sentiment senza personaggi: Topic \"Coppia\"",
    x = "Stagione",
    y = "Sentiment medio",
    color = "Serie"
  ) +
  scale_color_manual(values = c("S" = "#FFD700", "G" = "#1E90FF")) +  # Oro e Blu
  theme_minimal()

b1 + a1 + plot_layout(ncol = 2)

# Spettacolo/Media: S2 2, G2 4 ####
b1 <- ggplot(bind_rows(sentiment_lda_imdb_s2 %>% filter(topic == 2), sentiment_lda_imdb_g2 %>% filter(topic == 4)),
             aes(x = serie, y = sentiment, fill = serie)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Confronto sentiment: Topic \"Personaggi dal mondo reale / Spettacolo\"",
    x = "Serie",
    y = "Sentiment"
  ) +
  scale_fill_manual(values = c("#FFD700", "#1E90FF")) +  # giallo e blu
  theme_minimal()

a1 <- bind_rows(sentiment_lda_imdb_s2 %>% filter(topic == 2), 
                sentiment_lda_imdb_g2 %>% filter(topic == 4) %>% mutate(stagione = stagione + 13)) %>%
  group_by(serie, stagione) %>%
  summarise(sentiment_medio = mean(sentiment, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = stagione, y = sentiment_medio, color = factor(serie), group = serie)) +
  geom_line(size = 0.6) +
  geom_point(size = 2) +
  labs(
    title = "Andamento del sentiment: Topic \"Personaggi dal mondo reale / Spettacolo\"",
    x = "Stagione",
    y = "Sentiment medio",
    color = "Serie"
  ) +
  scale_color_manual(values = c("S" = "#FFD700", "G" = "#1E90FF")) +  # Oro e Blu
  theme_minimal()

b1 + a1 + plot_layout(ncol = 2)

# Bart vs Brian e Stewie: S2 4, G2 5 ####
b1 <- ggplot(bind_rows(sentiment_lda_imdb_s2 %>% filter(topic == 4), sentiment_lda_imdb_g2 %>% filter(topic == 5)),
             aes(x = serie, y = sentiment, fill = serie)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Confronto sentiment senza personaggi: Topic \"Bart vs Brian e Stewie\"",
    x = "Serie",
    y = "Sentiment"
  ) +
  scale_fill_manual(values = c("#FFD700", "#1E90FF")) +  # giallo e blu
  theme_minimal()

a1 <- bind_rows(sentiment_lda_imdb_s2 %>% filter(topic == 4), 
                sentiment_lda_imdb_g2 %>% filter(topic == 5) %>% mutate(stagione = stagione + 13)) %>%
  group_by(serie, stagione) %>%
  summarise(sentiment_medio = mean(sentiment, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = stagione, y = sentiment_medio, color = factor(serie), group = serie)) +
  geom_line(size = 0.6) +
  geom_point(size = 2) +
  labs(
    title = "Andamento del sentiment senza personaggi: Topic \"Bart vs Brian e Stewie\"",
    x = "Stagione",
    y = "Sentiment medio",
    color = "Serie"
  ) +
  scale_color_manual(values = c("S" = "#FFD700", "G" = "#1E90FF")) +  # Oro e Blu
  theme_minimal()

b1 + a1 + plot_layout(ncol = 2)

#Tempo libero: S 7, S2 7, G 4
b1 <- ggplot(bind_rows(sentiment_lda_imdb_s %>% filter(topic == 7), sentiment_lda_imdb_g %>% filter(topic == 4)),
             aes(x = serie, y = sentiment, fill = serie)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Confronto sentiment senza personaggi: Topic \"Tempo Libero\"",
    x = "Serie",
    y = "Sentiment"
  ) +
  scale_fill_manual(values = c("#FFD700", "#1E90FF")) +  # giallo e blu
  theme_minimal()

a1 <- bind_rows(sentiment_lda_imdb_s %>% filter(topic == 7), 
                sentiment_lda_imdb_g %>% filter(topic == 4) %>% mutate(stagione = stagione + 13)) %>%
  group_by(serie, stagione) %>%
  summarise(sentiment_medio = mean(sentiment, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = stagione, y = sentiment_medio, color = factor(serie), group = serie)) +
  geom_line(size = 0.6) +
  geom_point(size = 2) +
  labs(
    title = "Andamento del sentiment senza personaggi: Topic \"Tempo libero\"",
    x = "Stagione",
    y = "Sentiment medio",
    color = "Serie"
  ) +
  scale_color_manual(values = c("S" = "#FFD700", "G" = "#1E90FF")) +  # Oro e Blu
  theme_minimal()

b1 + a1 + plot_layout(ncol = 2)
