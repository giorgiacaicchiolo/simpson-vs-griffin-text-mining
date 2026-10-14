# Creazione df adatto con covariate ####
tot_clean <- bind_rows(simpson_clean, griffin_clean)

# simpson = 0, griffin = 1
tot_clean$serie = c(rep("S", 784), rep("G", 432))

anno = c(1989:2024, 1999, 1999, 2001, 2005:2024)
epi_per_stag = c(
  rle(simpson_clean$stagione)$lengths,
  rle(griffin_clean$stagione)$lengths
)

tot_clean$anni = rep(anno, epi_per_stag)

presidenti <- tibble(anno = 1989:2025,
                     presidente = c(
                       rep('George H.W. Bush', 4),
                       rep('Bill Clinton', 8),
                       rep('George W. Bush', 8),
                       rep('Barack Obama', 8),
                       rep('Donald Trump', 4),
                       rep('Joe Biden', 4),
                       'Donald Trump'))

tot_clean <- tot_clean %>% left_join(presidenti, by = c("anni" = "anno"))

produttori = matrix(NA, 1216, 13)
colnames(produttori) = c(
  'Groening_Brooks_Simon',
  'Jean',
  'Reiss',
  'Mirkin',
  'Oakley_Weinstein',
  'Scully',
  'MacFarlane_Zuckerman',
  'Palladino',
  'Goodman_Sheridan',
  'Hentemann',
  'Callaghan',
  'Sulkin',
  'Appel'
)

somma = function(x,y){
  return((cumsum(epi_per_stag[1:x])[x]+1):cumsum(epi_per_stag[1:y])[y])
}

produttori[1:sum(epi_per_stag[1:2]), 1] = 1
produttori[somma(2,4), 2:3] = 1
produttori[somma(4,6), 4] = 1
produttori[somma(6,8), 5] = 1
produttori[somma(8,12), 6] = 1
produttori[somma(10,36), 2] = 1
produttori[somma(36,39), 7] = 1
produttori[somma(38,39), 8] = 1
produttori[somma(39,43), 9] = 1
produttori[somma(43,46), 10] = 1
produttori[somma(43,51), 11] = 1
produttori[c(somma(46,49),somma(51,59)), 12] = 1
produttori[somma(49,59), 13] = 1

produttori <- as_tibble(produttori)

produttori = ifelse(is.na(produttori), 0, 1)

tot_clean <- bind_cols(tot_clean, produttori)

tot_clean$doc_id <- paste0(tot_clean$serie,
                           "_S", ifelse(tot_clean$stagione < 10, paste0("0", tot_clean$stagione), tot_clean$stagione),
                           "_E", ifelse(tot_clean$episodio < 10, paste0("0", tot_clean$episodio), tot_clean$episodio))

tot_clean_processed <- textProcessor(documents = tot_clean$testo, metadata = tot_clean)
tot_clean_out <- prepDocuments(tot_clean_processed$documents, tot_clean_processed$vocab, tot_clean_processed$meta)

tot_clean_out$meta$serie <- as.factor(tot_clean_out$meta$serie)

# anno ####
stm_anni <- stm(documents = tot_clean_out$documents,
                  vocab = tot_clean_out$vocab,
                  data = tot_clean_out$meta %>% select(c("anni", "serie")),
                  K = 9,  
                  prevalence = ~ anni,
                  content = ~ serie)

labelTopics(stm_anni)

# Distribuzione parola-topic ####
stm_anni_topics <- tidy(stm_anni, matrix = "beta")

# Crea il grafico delle prime 20 parole per topic
stm_anni_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL)
#Vita familiare
#Relazioni sentimentali
#Intrattenimento e media
#Vita domestica e crimine
#Animali e comportamenti estremi
#Sfera lavorativa
#Scuola / Infanzia
#Vita familiare vista dai bambini
#Festività natalizie

effect_anni <- estimateEffect(1:9 ~ anni, stmobj = stm_anni, metadata = tot_clean_out$meta %>% select(c("anni", "serie")))
summary(effect_anni)

par(mfrow = c(3,3))
for(i in 1:9){
  plot(effect_anni, covariate = "anni", method = "continuous", topics = i, ylim = c(0, 0.22), linecol = i, printlegend = F, main=paste0("Topic ", i))
}

# Presidente ####
tot_clean_out$meta$presidente = factor(tot_clean_out$meta$presidente, levels = c("George H.W. Bush", "Bill Clinton", 
                                                                                 "George W. Bush", "Barack Obama","Donald Trump", "Joe Biden"))

stm_presidente <- stm(documents = tot_clean_out$documents,
                        vocab = tot_clean_out$vocab,
                        data = tot_clean_out$meta %>% select(c("presidente", "serie")),
                        K = 9,  # numero di topic
                        prevalence = ~ -1 + presidente,
                        content = ~ serie)
labelTopics(stm_presidente)

# Distribuzione parola-topic) ####
stm_presidente_topics <- tidy(stm_presidente, matrix = "beta")

# Crea il grafico delle prime 20 parole per topic
stm_presidente_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL)

#famiglia e paternità
#relazioni amorose e matrimonio
#media, libri e intrattenimento
#denaro violenza e società
#animali casa e caos
#lavoro
#scuola e gioventù
#maternità e bambini
#natale

effect_presidente <- estimateEffect(1:9 ~ -1 + presidente, stmobj = stm_presidente, metadata = tot_clean_out$meta %>% select(c("presidente", "serie")))
summary(effect_presidente_c)

par(mfrow = c(3,3))
for(i in 1:9){
  plot(effect_presidente, covariate = "presidente", method = "pointestimate", topics = i,
       labeltype = 'custom', custom.labels = c("H.W. Bush", "Clinton", 
                                               "W. Bush", "Obama","Trump", "Biden"), 
       cex.lab = 0.9, cex = 0.9, cex.axis = 0.9, xlim = c(0, 0.2))
}

#cambiamento
for(i in 1:9){
  plot(effect_presidente, covariate = "presidente", method = "pointestimate", topics = i,
       labeltype = 'custom', custom.labels = "", 
       cex.lab = 0.9, cex = 0.9, cex.axis = 0.9, xlim = c(0, 0.2))
  text(-0.05, seq(6, 1, by = -1), 
       labels = c("H.W. Bush", "Clinton", "W. Bush", "Obama","Trump", "Biden"), 
       xpd = T, adj=0.7)
}

# Produttori ####
f_prod <- paste0("~ - 1 + ", paste0(colnames(tot_clean)[7:19], collapse=" + "))
stm_produttori <- stm(documents = tot_clean_out$documents,
                        vocab = tot_clean_out$vocab,
                        data = tot_clean_out$meta %>% select(c(colnames(tot_clean)[7:19], "serie")),
                        K = 9, 
                        prevalence = as.formula(f_prod),
                        content = ~ serie)
labelTopics(stm_produttori)

# Riorganizza la matrice beta (distribuzione parola-topic) ####
stm_produttori_topics <- tidy(stm_produttori, matrix = "beta")

# Crea il grafico delle prime 20 parole per topic
stm_produttori_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL)
#1 famiglia e paternità
#relazioni amorose e matrimonio
#famiglia e tempo libero
#denaro violenza e società
#animali casa e caos
#lavoro
#scuola e gioventù
#maternità e bambini
#natale

effect_produttori <- estimateEffect(as.formula(paste0("1:9 ", f_prod)), stmobj = stm_produttori, metadata = tot_clean_out$meta %>% select(c(colnames(tot_clean)[7:19], "serie")) %>% as.data.frame())
summary(effect_produttori)

par(mfrow = c(3,3))

for(i in 1:9){
  # Plot vuoto per impostare gli assi
  plot(1, type = "n", xlim = c(-0.15, 0.28), ylim = c(0.5, 13.5),
       xaxt = "n", yaxt = "n", xlab = "Effetto stimato", ylab = "", main = paste("Topic", i))
  
  axis(1, cex.axis = 0.8)
  axis(2, at = 1:13, labels = colnames(produttori), las = 2, cex.axis = 0.5)
  
  for(j in seq_along(colnames(produttori))){
    cov_name <- colnames(produttori)[j]
    eff <- summary(effect_produttori)$tables[[i]]
    
    # Trova la riga corrispondente alla covariata j
    if (cov_name %in% rownames(eff)) {
      est <- eff[cov_name, 1]
      se <- eff[cov_name, 2]
      
      y_pos <- j
      
      # Punto + errore standard
      points(est, y_pos, pch = 16, cex = 0.9)
      segments(est - 1.96 * se, y_pos, est + 1.96 * se, y_pos, lwd =  0.7)
    }
  }
}

#cambiamenti
for(i in 1:9){
  # Plot vuoto per impostare gli assi
  plot(1, type = "n", xlim = c(-0.15, 0.28), ylim = c(0.5, 13.5),
       xaxt = "n", yaxt = "n", xlab = "Effetto stimato", ylab = "", main = paste("Topic", i))
  
  axis(1, cex.axis = 0.8)

  for(j in seq_along(colnames(produttori))){
    cov_name <- colnames(produttori)[j]
    eff <- summary(effect_produttori)$tables[[i]]
    
    # Trova la riga corrispondente alla covariata j
    if (cov_name %in% rownames(eff)) {
      est <- eff[cov_name, 1]
      se <- eff[cov_name, 2]
      
      y_pos <- j
      
      # Punto + errore standard
      points(est, y_pos, pch = 16, cex = 0.9)
      segments(est - 1.96 * se, y_pos, est + 1.96 * se, y_pos, lwd =  0.7)
    }
  }
  text(-0.18, seq(1, 6), labels = colnames(produttori), xpd = T, adj=1, cex = 0.5)
}

for(i in 1:9){
  for(j in seq_along(colnames(produttori))){
    cov_name <- colnames(produttori)[j]
    eff <- summary(effect_produttori)$tables[[i]]
    
    # Trova la riga corrispondente alla covariata j
    if (cov_name %in% rownames(eff)) {
      est <- eff[cov_name, 1]
      se <- eff[cov_name, 2]

      y_pos <- j
      print(y_pos)
      
      # Punto + errore standard
      #points(est, y_pos, pch = 16, cex = 0.9)
      print(c(est - 1.96 * se, est + 1.96 * se))
    }
  }
  #text(-0.18, seq(1, 6), labels = colnames(produttori), xpd = T, adj=1, cex = 0.5)
}

# anno fattore####
meta2 <- tot_clean_out$meta
meta2$anni <- factor(meta2$anni, levels = 1989:2024)
stm_anni_fattore <- stm(documents = tot_clean_out$documents,
                vocab = tot_clean_out$vocab,
                data = meta2 %>% select(c("anni", "serie")),
                K = 9,  
                prevalence = ~ anni,
                content = ~ serie)

labelTopics(stm_anni_fattore)

# Distribuzione parola-topic ####
stm_anni_fattore_topics <- tidy(stm_anni_fattore, matrix = "beta")

# Crea il grafico delle prime 20 parole per topic
stm_anni_fattore_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL)
# Famiglia
# Relazioni sentimentali
# Intrattenimento e media
# Vita domestica
# Eventi anomali
# Sfera lavorativa
# Scuola
# Maternità e personaggi femminili
# Natale

effect_anni_fattore <- estimateEffect(1:9 ~ anni, stmobj = stm_anni_fattore, metadata = meta2 %>% select(c("anni", "serie")))
summary(effect_anni_fattore)

par(mfrow = c(3,3))
for(i in 1:9){
  plot(effect_anni_fattore, covariate = "anni", method = "pointestimate", topics = i, 
       cex = 0.75, cex.axis = 0.8, xlim = c(-0.05, 0.3), verbose.labels = F, yaxt = "n", labeltype='custom',
       custom.labels = "", main = paste0("Topic ", i))
  text(-0.05, seq(36, 1, by = -5), labels = c("89", "94", "99", "04", "09", "14", "19", "24"), xpd = T, adj=1, cex = 0.7)
}


# anno fattore####
meta2 <- tot_clean_out$meta
meta2$anni <- factor(meta2$anni, levels = 1989:2024)
stm_anni_fattore <- stm(documents = tot_clean_out$documents,
                vocab = tot_clean_out$vocab,
                data = meta2 %>% select(c("anni", "serie")),
                K = 9,  
                prevalence = ~ anni,
                content = ~ serie)

labelTopics(stm_anni_fattore)

# Distribuzione parola-topic ####
stm_anni_fattore_topics <- tidy(stm_anni_fattore, matrix = "beta")

# Crea il grafico delle prime 20 parole per topic
stm_anni_fattore_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL)
# Famiglia
# Relazioni sentimentali
# Intrattenimento e media
# Vita domestica
# Eventi anomali
# Sfera lavorativa
# Scuola
# Maternità e personaggi femminili
# Natale

effect_anni_fattore <- estimateEffect(1:9 ~ anni, stmobj = stm_anni_fattore, metadata = meta2 %>% select(c("anni", "serie")))
summary(effect_anni_fattore)

par(mfrow = c(3,3))
for(i in 1:9){
  plot(effect_anni_fattore, covariate = "anni", method = "pointestimate", topics = i, 
       cex = 0.75, cex.axis = 0.8, xlim = c(-0.05, 0.3), verbose.labels = F, yaxt = "n", labeltype='custom',
       custom.labels = "", main = paste0("Topic ", i))
  text(-0.05, seq(36, 1, by = -5), labels = c("89", "94", "99", "04", "09", "14", "19", "24"), xpd = T, adj=1, cex = 0.7)
}

# anni lisciato ####
stm_anni_liscio <- stm(documents = tot_clean_out$documents,
                vocab = tot_clean_out$vocab,
                data = tot_clean_out$meta %>% select(c("anni", "serie")),
                K = 9,  
                prevalence = ~ s(anni),
                content = ~ serie)

labelTopics(stm_anni_liscio)

# Distribuzione parola-topic ####
stm_anni_liscio_topics <- tidy(stm_anni_liscio, matrix = "beta")

# Crea il grafico delle prime 20 parole per topic
stm_anni_liscio_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL)

effect_anni_liscio <- estimateEffect(1:9 ~ s(anni), stmobj = stm_anni_liscio, metadata = tot_clean_out$meta %>% select(c("anni", "serie")))
summary(effect_anni_liscio)

par(mfrow = c(3,3))
for(i in 1:9){
  plot(effect_anni_liscio, covariate = "anni", method = "continuous", topics = i, ylim = c(-0.05, 0.25), linecol = i, printlegend = F, main=paste0("Topic ", i))
}


library(dplyr)
library(ggplot2)

# Prepara il data frame in formato tidy
df_plot <- bind_rows(lapply(1:9, function(i) {
  eff <- summary(effect_produttori)$tables[[i]]
  eff_df <- as.data.frame(eff)
  eff_df$covariate <- rownames(eff_df)
  eff_df$topic <- paste("Topic", i)
  names(eff_df)[1:2] <- c("estimate", "se")
  eff_df %>% mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  )
}))

# Ordina covariate (in ordine inverso per allineamento)
df_plot$covariate <- factor(df_plot$covariate,
                            levels = rev(colnames(produttori)))


# Costruisci il grafico
ggplot(df_plot, aes(x = estimate, y = covariate)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0, size = 0.6) +
  facet_wrap(~ topic, ncol = 3, scales = "free_y") +
  scale_x_continuous(name = "Effetto stimato") +
  ylab(NULL) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_blank(),  
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    panel.spacing = unit(1, "lines")
  )


