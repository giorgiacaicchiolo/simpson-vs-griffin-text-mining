source('Pulizia.R')

# ANALISI FREQUENZE ####

# Statistiche di base
frequenze_simpson %>% select(n) %>% boxplot(main = "Boxplot Frequenze")
frequenze_simpson %>% pull(n) %>% barplot(main = "Barplot Frequenze Assolute")
frequenze_simpson %>% select(n) %>% summary()

# Parole con frequenza > 1000
frequenze_simpson %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Parole con frequenza > 1000", x = NULL, y = "Frequenza")

# Rimozione outlier sopra soglia
frequenze_simpson <- frequenze_simpson %>%
  filter(n < 3400)

# Parole di due lettere (si tengono solo tv, mr, dr)
frequenze_simpson %>%
  filter(nchar(word) == 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Parole di 2 lettere", x = NULL, y = "Frequenza")

# Parole di tre lettere (identifica rumori da rimuovere)
frequenze_simpson %>%
  filter(nchar(word) == 3) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Parole di 3 lettere", x = NULL, y = "Frequenza")

# Rimozione parole troppo corte o inutili
frequenze_simpson <- frequenze_simpson %>%
  filter(nchar(word) > 2 | word %in% c("tv", "dr", "mr"))

# Istogramma distribuzione frequenze
frequenze_simpson$n %>% hist(breaks = 100, main = "Distribuzione Frequenze", xlab = "Frequenza")

# Parole con frequenza > 1200
frequenze_simpson %>%
  filter(n > 1200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(title = "Parole con frequenza > 1200", x = NULL, y = "Frequenza")

# ANALISI N-GRAMMI ####

# Bigrammi più frequenti
bigrammi_simpson <- simpson %>%
  unnest_tokens(ngram, testo, token = "ngrams", n = 2) %>%
  count(ngram, sort = TRUE)

head(bigrammi_simpson, 20)

# Trigrammi più frequenti
trigrammi_simpson <- simpson %>%
  unnest_tokens(ngram, testo, token = "ngrams", n = 3) %>%
  count(ngram, sort = TRUE)

head(trigrammi_simpson, 20)

# Quadrigrammi più frequenti
quadrigrammi_simpson <- simpson %>%
  unnest_tokens(ngram, testo, token = "ngrams", n = 4) %>%
  count(ngram, sort = TRUE)

head(quadrigrammi_simpson, 20)

rm(bigrammi_simpson, trigrammi_simpson, quadrigrammi_simpson)
gc()

# ANALISI GRIFFIN ####

# FREQUENZE ####

# Statistiche di base
frequenze_griffin %>% select(n) %>% boxplot(main = "Boxplot Frequenze Griffin")
frequenze_griffin %>% pull(n) %>% barplot(main = "Barplot Frequenze Assolute Griffin")
frequenze_griffin %>% select(n) %>% summary()

# Parole con frequenza > 1000
frequenze_griffin %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Parole con frequenza > 1000 (Griffin)", x = NULL, y = "Frequenza")

# Rimozione outlier sopra soglia
frequenze_griffin <- frequenze_griffin %>%
  filter(n < 2000)

# Parole di due lettere
frequenze_griffin %>%
  filter(nchar(word) == 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Parole di 2 lettere (Griffin)", x = NULL, y = "Frequenza")

# Parole di tre lettere
frequenze_griffin %>%
  filter(nchar(word) == 3) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Parole di 3 lettere (Griffin)", x = NULL, y = "Frequenza")

# Rimozione parole brevi o inutili
frequenze_griffin <- frequenze_griffin %>%
  filter(nchar(word) > 2 | word %in% c("tv", "dr", "mr"))

# Istogramma distribuzione frequenze
frequenze_griffin$n %>% hist(breaks = 100, main = "Distribuzione Frequenze Griffin", xlab = "Frequenza")

# Parole con frequenza > 900
frequenze_griffin %>%
  filter(n > 900) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(title = "Parole con frequenza > 900 (Griffin)", x = NULL, y = "Frequenza")

# N-GRAMMI ####

# Bigrammi
bigrammi_griffin <- griffin %>%
  unnest_tokens(ngram, testo, token = "ngrams", n = 2) %>%
  count(ngram, sort = TRUE)

head(bigrammi_griffin, 20)

# Trigrammi
trigrammi_griffin <- griffin %>%
  unnest_tokens(ngram, testo, token = "ngrams", n = 3) %>%
  count(ngram, sort = TRUE)

head(trigrammi_griffin, 20)

# Quadrigrammi
quadrigrammi_griffin <- griffin %>%
  unnest_tokens(ngram, testo, token = "ngrams", n = 4) %>%
  count(ngram, sort = TRUE)

head(quadrigrammi_griffin, 20)

rm(bigrammi_griffin, trigrammi_griffin, quadrigrammi_griffin)
gc()


# N-GRAMMI SENZA STOPWORDS ####

# BIGRAMMI ####

# Simpson - Bigrammi senza stopword
bigrammi_simpson_sw <- simpson %>%
  select(testo) %>%
  unnest_tokens(ngram, testo, token = "ngrams", n = 2) %>%
  separate(ngram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(ngram, word1, word2, sep = " ") %>%
  count(ngram, sort = TRUE)

bigrammi_simpson_sw %>% print(n = 40)

# Griffin - Bigrammi senza stopword
bigrammi_griffin_sw <- griffin %>%
  select(testo) %>%
  unnest_tokens(ngram, testo, token = "ngrams", n = 2) %>%
  separate(ngram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(ngram, word1, word2, sep = " ") %>%
  count(ngram, sort = TRUE)

bigrammi_griffin_sw %>% print(n = 40)

# TRIGRAMMI ####

# Simpson - Trigrammi senza stopword
trigrammi_simpson_sw <- simpson %>%
  select(testo) %>%
  unnest_tokens(ngram, testo, token = "ngrams", n = 3) %>%
  separate(ngram, into = c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word, !word3 %in% stop_words$word) %>%
  unite(ngram, word1, word2, word3, sep = " ") %>%
  count(ngram, sort = TRUE)

trigrammi_simpson_sw %>% print(n = 40)

# Griffin - Trigrammi senza stopword
trigrammi_griffin_sw <- griffin %>%
  select(testo) %>%
  unnest_tokens(ngram, testo, token = "ngrams", n = 3) %>%
  separate(ngram, into = c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word, !word3 %in% stop_words$word) %>%
  unite(ngram, word1, word2, word3, sep = " ") %>%
  count(ngram, sort = TRUE)

trigrammi_griffin_sw %>% print(n = 40)


rm(bigrammi_simpson_sw, trigrammi_simpson_sw, bigrammi_griffin_sw, trigrammi_griffin_sw)
gc()


### GRAFICO COMPARATIVO
# Rinomina colonne per evitare conflitti
frequenze_simpson <- frequenze_simpson %>% rename(freq_simpson = n)
frequenze_griffin <- frequenze_griffin %>% rename(freq_griffin = n)

# Join sulle parole comuni
words_compare <- inner_join(frequenze_simpson, frequenze_griffin, by = "word")

# Normalizzazione
normalize <- function(x) (x - min(x) + 0.001) / (max(x) - min(x))
words_compare_n <- words_compare %>%
  mutate(
    freq_simpson = normalize(freq_simpson),
    freq_griffin = normalize(freq_griffin)
  )

# Top 100 parole più frequenti in entrambi
words_compare_f <- words_compare_n %>%
  mutate(total = freq_simpson + freq_griffin) %>%
  slice_max(order_by = total, n = 100)

# Grafico 1: scala lineare
ggplot(words_compare_f, aes(x = freq_simpson, y = freq_griffin, label = word)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_text(check_overlap = TRUE, hjust = 0, vjust = 1, size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Top 100 parole comuni: Simpson vs Griffin",
    x = "Frequenza normalizzata nei Simpson",
    y = "Frequenza normalizzata nei Griffin"
  ) +
  theme_minimal()

# Grafico 2: scala logaritmica su tutte le parole
ggplot(words_compare_f %>% filter(!word %in% c('fam', 'homie')), aes(x = freq_simpson, y = freq_griffin, label = word)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_text(check_overlap = TRUE, hjust = 0, vjust = 1, size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Frequenze logaritmiche delle parole comuni",
    x = "Frequenza normalizzata nei Simpson (log scale)",
    y = "Frequenza normalizzata nei Griffin (log scale)"
  ) +
  theme_minimal()


rm(words_compare, words_compare_f, words_compare_n, normalize)
gc()


### PAROLE PIU FREQUENTI PER STAGIONE

# Simpson
# Estrazione e filtraggio top parole per stagione
top_words_simpson <- simpson %>%
  unnest_tokens(word, testo) %>%
  filter(
    !word %in% c(stop_words$word, stopwords_nostre, personaggi),
    nchar(word) > 2,
    str_detect(word, "^[a-z]+$")
  ) %>%
  filter(nchar(word) > 2 | word %in% c("tv", "dr", "mr"))
count(stagione, word, sort = TRUE) %>%
  group_by(stagione) %>%
  slice_max(n, n = 500) %>%
  ungroup()

# Inner join parole comuni tra stagione 1 e 36
words_compare_simpson <- inner_join(
  top_words_simpson %>% filter(stagione == 1),
  top_words_simpson %>% filter(stagione == 36),
  by = "word"
)

# Grafico log-log confronto stagione 1 vs 36
ggplot(words_compare_simpson, aes(x = n.x, y = n.y, label = word)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_text(check_overlap = TRUE, hjust = 0, vjust = 1, size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Confronto parole più frequenti: Simpson1 vs Simpson36",
    x = "Frequenza nei Simpson1 (log scale)",
    y = "Frequenza nei Simpson36 (log scale)"
  ) +
  theme_minimal()

rm(words_compare_simpson, top_words_simpson)
gc()


# Griffin
# Estrazione e filtraggio top parole per stagione
top_words_griffin <- griffin %>%
  unnest_tokens(word, testo) %>%
  filter(
    !word %in% c(stop_words$word, stopwords_nostre, personaggi),
    nchar(word) > 2,
    str_detect(word, "^[a-z]+$")
  ) %>%
  filter(nchar(word) > 2 | word %in% c("tv", "dr", "mr"))
count(stagione, word, sort = TRUE) %>%
  group_by(stagione) %>%
  slice_max(n, n = 500) %>%
  ungroup()

# Inner join parole comuni tra stagione 1 e 36
words_compare_griffin <- inner_join(
  top_words_griffin %>% filter(stagione == 1),
  top_words_griffin %>% filter(stagione == 23),
  by = "word"
)

# Grafico log-log confronto stagione 1 vs 36
ggplot(words_compare_griffin, aes(x = n.x, y = n.y, label = word)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_text(check_overlap = TRUE, hjust = 0, vjust = 1, size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Confronto parole più frequenti: Griffin1 vs Griffin23",
    x = "Frequenza nei Griffin1 (log scale)",
    y = "Frequenza nei Griffin23 (log scale)"
  ) +
  theme_minimal()

rm(words_compare_griffin, top_words_griffin)
gc()


### WORDCLOUD
library(wordcloud)
library(RColorBrewer)
library(ggwordcloud)

# Wordcloud per le 60 parole più frequenti nei Simpson
frequenze_simpson %>%
  arrange(desc(freq_simpson)) %>%
  slice_head(n = 60) %>%
  ggplot(aes(label = word, size = freq_simpson, color = freq_simpson)) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 15) +
  labs(title = "Wordcloud parole più frequenti - Simpson") +
  theme_minimal()

# Wordcloud per le 60 parole più frequenti nei Griffin
frequenze_griffin %>%
  arrange(desc(freq_griffin)) %>%
  slice_head(n = 60) %>%
  ggplot(aes(label = word, size = freq_griffin, color = freq_griffin)) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 15) +
  labs(title = "Wordcloud parole più frequenti - Griffin") +
  theme_minimal()

gc()
