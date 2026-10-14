library(tidytext)
library(tidyverse)
library(textstem)
library(tm)
library(dplyr)
library(stringr)
library(tokenizers)
library(knitr)
library(lubridate)
library(reshape2)
library(topicmodels)
library(stringr)
library(SnowballC)
library(LDAvis)
library(tsne)
library(wordcloud)
library(ldatuning)
library(stm)
library(lda)
library(textstem)
library(parallel)
library(future)
library(future.apply)
library(cowplot)
library(gridExtra)

load("data/Data_frame_Griffin.RData")
load("data/Data_frame_Simpson.RData")

# Pulizia generale
griffin1 <- griffin_df %>%
  mutate(testo = str_replace_all(testo, "\\\\+", " ")) %>% #sostituisce uno o piu backslash
  mutate(testo = gsub("-", " ", testo)) %>% #toglie trattini
  mutate(testo = str_replace_all(testo, "\\[.*?\\]", " ")) %>% #rimuove tutto il contenuto tra []
  mutate(testo = str_replace_all(testo, "\\(.*?\\)", " ")) %>% #e tra tonde
  mutate(testo = str_replace_all(testo, '\"', " ")) %>% #toglie virgolette
  mutate(testo = str_replace_all(testo, "\\|", " ")) %>% #toglie |
  mutate(testo = str_squish(testo)) #toglie spazi superflui


# Togliere la sigla e nome puntata
griffin2 <- griffin1 %>%
  mutate(testo = str_replace_all(testo, regex("It seems today that all you see.*?He's a family guy"), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("It seems today that all you see.*?He 's a family guy"), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("1 It seems today that all you see.*?He's a fam ily guy", ignore_case = TRUE), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("1 It seems today that all you see.*?He's a fam ily guy!", ignore_case = TRUE), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("1 It seems today that all you see.*?He's a Fam ily Guy", ignore_case = TRUE), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("1 It seems today that all you see.*?He's a Fam ily Guy!", ignore_case = TRUE), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("It seems today that all you see.*?He's a Fam ily Guy!", ignore_case = TRUE), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("It seems today that all you see.*?He's a Fam ily Guy !", ignore_case = TRUE), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("It seems today that all you see.*?He's aFam ily Guy!", ignore_case = TRUE), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("It seems today that all you see.*?He 's our family guy !", ignore_case = TRUE), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("It seems today that all you see.*?He's our family guy", ignore_case = TRUE), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("Family Guy.*?@elderman"), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("^1"), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("^!"), " "))

# Togliere la sigla tagliata a metà
griffin2 <- griffin2 %>%
  mutate(testo = str_replace_all(testo, regex("It seems today that all you see.*?Lucky there's a man who positively can do All the things that make us", ignore_case = TRUE), " "))

# Togliere voci fuori campo (parole maiuscole seguite da :)
griffin3 <- griffin2 %>%
  mutate(testo = str_replace_all(testo, "\\s*[A-Z]+\\s*\\d*\\s*:", " "))

# Togliere "Family Guy" all'inizio
griffin4 <- griffin3 %>%
  mutate(testo = str_replace_all(testo, regex("^Family Guy Episode"), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("^Family Guy episode"), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("^Family Guy"), " ")) %>%
  mutate(testo = str_replace_all(testo, regex("^Family guy"), " ")) %>%
  mutate(testo = str_replace_all(testo, "'", " ")) %>% #Rimozione degli apostrofo
  mutate(testo = str_replace_all(testo, regex("iike", ignore_case = TRUE), "like")) #sostituzione di un errore di trascrizione frequente

griffin4 <- griffin4 %>%
  mutate(testo = str_replace_all(testo, regex("\\b(didn|shouldn|couldn|wouldn|haven|hasn|isn|aren|wasn|weren|can|don)\\b", ignore_case=TRUE), ""))

# Rimozione riga, episodio problematico
griffin <- griffin4[-144, ] %>%
  mutate(testo = str_squish(testo)) #toglie spazi superflui

################################################################################
# Pulizia Simpson
simpson <- simpson_df %>%
  mutate(testo = str_replace_all(testo, "\\\\", " ")) %>%
  mutate(testo = gsub("-", " ", testo)) %>%
  mutate(testo = str_replace_all(testo, "\\[.*?\\]", " ")) %>%
  mutate(testo = str_replace_all(testo, "\\(.*?\\)", " ")) %>%
  mutate(testo = str_replace_all(testo, "#", " ")) %>%
  mutate(testo = str_replace_all(testo, '\"', " ")) %>%
  mutate(testo = str_replace(testo, '^\\s*["“”]?Ah+h!?,?\\s*', " ")) %>%
  mutate(testo = str_replace(testo, '^\\s*["“”]?[Tt]he Simpsons\\s*["“”]?', " ")) %>%
  mutate(testo = str_replace(testo, '^\\s*["“”]*[Tt]he Simpsons["“”]*\\s*', " ")) %>%
  mutate(testo = str_replace(testo, '^\\s*["“”]?D\'oh!?["“”]?\\s*', " ")) %>%
  mutate(testo = str_replace(testo, '^\\s*["“”]*\\s*The Simpsons\\s*["“”]*\\s*["“”]*\\s*D\'oh!?\\s*["“”]*\\s*', " ")) %>%
  mutate(testo = str_replace(testo, "\\s*(\\d{1,2}x\\d{1,3}|[Ss]\\d{1,2}[Ee]\\d{1,2})\\b\\s*", " ")) %>%
  mutate(testo = str_replace(testo, "\\s*\\d+\\s*:", " ")) %>%
  mutate(testo = str_replace_all(testo, "\\s*[A-Z]+\\s*\\d*\\s*:", " ")) %>%
  mutate(testo = str_replace_all(testo, "'", " ")) %>%
  mutate(testo = str_replace_all(testo, "Ã\u0082", "")) %>%
  mutate(testo = str_replace_all(testo, "Ã\u0083â\u0080\u009aÃ¢â\u0084¢Âª", "")) %>%
  mutate(testo = str_replace_all(testo, "iike", "like")) %>%
  mutate(testo = str_replace_all(testo, regex("\\b(didn|shouldn|couldn|wouldn|haven|hasn|isn|aren|wasn|weren|can|don)\\b", ignore_case=TRUE), ""))  %>%
  mutate(testo = str_squish(testo))

rm(griffin_df, griffin1, griffin2, griffin3, griffin4, simpson_df)
gc()


# Bigrammi da unire nei testi
bigrammi_da_unire <- c(
  "chief wiggum","sideshow bob","sideshow mel","groundskeeper willie","principal skinner",
  "blue-haired lawyer","rich texan","mayor quimby","fat tony","lionel hutz","troy mcclure",
  "artie ziff","rainier wolfcastle","santa's little helper","snowball ii","snowball v",
  "crazy cat lady","cookie kwan","lindsey naegle","bernice hibbert","sarah wiggum",
  "johnny tightlips","mr burns","el barto","agnes skinner","peter griffin","lois griffin",
  "chris griffin","stewie griffin","brian griffin","glenn quagmire","cleveland brown",
  "joe swanson","bonnie swanson","kevin swanson","susie swanson","mort goldman",
  "muriel goldman","neil goldman","carter pewterschmidt","barbara pewterschmidt",
  "carol pewterschmidt","evil monkey","ernie the giant chicken","adam west","dr. hartman",
  "tom tucker","jake tucker","diane simmons","ollie williams","tricia takanawa",
  "king butt","human rupert","brain damaged horse","felicia day","ron perlman",
  "patrick stewart","denise robertson","evil stewie","jesus christ","santa claus"
)

# Sostituzione con underscore nei testi
for (bigram in bigrammi_da_unire) {
  monogram <- gsub(" ", "_", bigram)
  simpson$testo <- gsub(bigram, monogram, simpson$testo, ignore.case = TRUE)
  griffin$testo <- gsub(bigram, monogram, griffin$testo, ignore.case = TRUE)
}
rm(bigram, monogram, bigrammi_da_unire)
gc()

#ACCORPAMENTO DEI PERSONAGGI SOTTO UNICO NOME-----------------------------------
# Dizionario: varianti da sostituire per ogni personaggio
nomi_personaggi_simpson <- list(
  homer = c("Homer Simpson", "Homer","Homie","Homey", "homer J"),
  marge = c("Marge Simpson", "Marge", "marjorie"),
  bart = c("Bart Simpson", "Bart"),
  lisa = c("Lisa Simpson", "Lisa","Lise"),
  maggie = c("Maggie Simpson", "Maggie"),
  abe = c("Abe Simpson", "Grandpa Simpson", "Abe", "abraham", "grampa"),
  mrburns = c("mr_burns","Mr Burns", "Burns","Mr. Burns" ,"Montgomery Burns"),
  smithers = c("Waylon Smithers", "Smithers","Waylon"),
  ned = c("Ned Flanders", "Ned"),
  maude = c("Maude Flanders", "Maude","Mrs Flanders"),
  moe = c("Moe Szyslak", "Moe", "\\bMo\\b"),
  barney = c("Barney Gumble", "Barney"),
  lenny = c("Lenny Leonard", "Lenny"),
  carl = c("Carl Carlson", "Carl"),
  apu = c("Apu Nahasapeemapetilon", "Apu"),
  wiggum = c("chief_wiggum","Chief Wiggum", "Wiggum", "Clancy"),
  ralph = c("Ralph Wiggum", "Ralph"),
  skinner = c("principal_skinner","Principal Skinner", "Seymour Skinner", "Skinner","Seymour"),
  krusty = c("Krusty the Clown", "Krusty", "Krusty Klaw","Krusty Clown"),
  bob = c("sideshow_bob","Sideshow Bob", "Bob"),
  mel = c("sideshow_mel","Sideshow Mel", "Mel", "melvin"),
  troy = c("troy_mcclure","Troy McClure", "Troy"),
  lionel = c("lionel_hutz","Lionel Hutz", "Hutz"),
  comicguy = c("Comic Book Guy", "Comic Guy"),
  willie = c("groundskeeper_willie","Groundskeeper Willie", "Willie"),
  quimby = c("mayor_quimby","Mayor Quimby", "Quimby","Joe quimby"),
  hibbert = c("Dr. Hibbert","Dr Hibbert", "Hibbert", "julius hibbert"),
  nick = c("Dr.Nick","Dr Nick", "Nick","Dr. NIck Riviera"),
  patty = c("Patty Bouvier", "Patty"),
  selma = c("Selma Bouvier", "Selma"),
  television = c("television", "\\btv\\b")
)

# Funzione per accorpare i nomi del dizionario
sostituisci_nomi <- function(testo, dizionario) {
  reduce(names(dizionario), .init = testo, .f = function(testo, nome_standard) {
    parole <- str_replace_all(dizionario[[nome_standard]], "(\\W)", "\\\\\\1")
    pattern <- paste0("\\b(", paste(parole, collapse = "|"), ")\\b")
    
    # Gestione speciale per mr_burns (case-sensitive)
    if(nome_standard == "mrburns") {
      # Crea un pattern case-sensitive per Burns per non confonderlo con il verbo 'burn'
      burns_pattern <- paste0("\\b(", paste(parole, collapse = "|"), ")\\b")
      str_replace_all(testo, regex(burns_pattern, ignore_case = FALSE), nome_standard)
    } else {
      str_replace_all(testo, regex(pattern, ignore_case = TRUE), nome_standard)
    }
  })
}

# Applicazione
simpson <- simpson %>%
  mutate(testo = sostituisci_nomi(testo, nomi_personaggi_simpson))

#GRIFFIN
nomi_personaggi_griffin <- list(
  peter = c("Peter Griffin", "peter_griffin", "Peter"),
  lois = c("Lois Griffin", "lois_griffin", "Lois"),
  meg = c("Meg Griffin", "meg_griffin", "Meg", "Megan"),
  chris = c("Chris Griffin", "chris_griffin", "Chris"),
  stewie = c("Stewie Griffin", "stewie_griffin", "Stewie"),
  brian = c("Brian Griffin", "brian_griffin", "Brian"),
  quagmire = c("Glenn Quagmire", "glenn_quagmire", "Quagmire","glenn"),
  cleveland = c("Cleveland Brown", "cleveland_brown", "Cleveland"),
  joe = c("Joe Swanson", "joe_swanson", "Joe"),
  bonnie = c("Bonnie Swanson", "bonnie_swanson", "Bonnie"),
  mort = c("Mort Goldman", "mort_goldman", "Mort"),
  herbert = c("John Herbert", "Herbert", "the pervert"),
  tom = c("Tom Tucker", "tom_tucker"),
  carter = c("Carter Pewterschmidt", "carter_pewterschmidt", "Mr. Pewterschmidt", "Carter"),
  barbara = c("Barbara Pewterschmidt", "barbara_pewterschmidt", "Barbara"),
  adam = c("Mayor West", "Adam West", "mayor_west", "Mayor Adam West", "West"),
  consuela = c("Consuela", "maid"),
  tricia = c("Tricia Takanawa", "tricia_takanawa", "Tricia"),
  television = c("television", "\\btv\\b")
)

# Applicazione
griffin <- griffin %>%
  mutate(testo = sostituisci_nomi(testo, nomi_personaggi_griffin))

################################################################################


simpson$testo = lemmatize_strings(simpson$testo)
griffin$testo = lemmatize_strings(griffin$testo)

# Funzione per tokenizzazione, rimuovere stopwords standard e conteggio frequenze lemmi
elabora_testo <- function(df) {
  df %>%
    unnest_tokens(word, testo) %>%            # Tokenizzazione: suddivide il testo in token
    filter(!word %in% stop_words$word) %>%    # Rimuove le stopwords standard
    filter(str_detect(word, "^[a-z]+$")) %>%  #Filtra solo parole composte da lettere minuscole (esclude numeri e punteggiatura)
    mutate(lemma = lemmatize_words(word)) %>% # Lemmatizzazione: riduce ogni parola alla sua forma base (es. "running" → "run")
    count(lemma, sort = TRUE) %>%             # Conta la frequenza di ciascun lemma, ordinando per frequenza decrescente
    filter(!lemma %in% stop_words$word)       # Rimuove nuovamente le stopwords sui lemmi (in caso di forme diverse rimaste)
}


frequenze_simpson <- elabora_testo(simpson) %>% rename(word = lemma, n = n)
frequenze_griffin <- elabora_testo(griffin) %>% rename(word = lemma, n = n)

rm(elabora_testo)
gc()

# Unione frequenze
merged_df <- full_join(frequenze_simpson, frequenze_griffin, by = "word", suffix = c("_1", "_2")) %>%
  replace_na(list(n_1 = 0, n_2 = 0))

# Stopword personalizzate (poco frequenti, irrilevanti e lista dei personaggi)
stopwords_nostre = c(
  merged_df$word[merged_df$n_1 < 200 & merged_df$n_2 < 170],
  'huh', 'hmm', 'ooh', 'mmm', 'shh', 'hoo', 'aah', 'ugh', 'hmmm', 'hey',
  'time', 'gonna', 'gasp', 'whoa', 'yea', 'heh', 'yay', 'ohh', 'woo', 'aye',
  'ahh', 'wee', 'hee', 'aaah', 'whew', 'aha', 'wha', 'boo', 'doo', 'dee', 'arr',
  'whoo'
)

personaggi = c('homer','marge','bart','lisa','maggie','abe','mona','mrburns',
               'herb','milhouse','kirk','luann','ned','maude','rod','todd',
               'agnes','seymour','patty','selma','jacqueline','moe','barney',
               'lenny','carl','apu','manjula','ralph','wiggum','lou',
               'eddie','krusty','bob','mel',
               'groundskeeper_willie','skinner','edna','martin',
               'nelson','sherri','terri','janey','database','kearney',
               'dolph','jimbo','miss_hoover','dr_hibbert','nick',
               'professor_frink','comicguy','duffman','snake',
               'bumblebee_man','disco_stu','sea_captain','gil','agnes_skinner',
               'otto','kent_brockman','reverend_lovejoy','helen_lovejoy',
               'judge_snyder','jasper','lunchlady_doris','agnes',
               'blue-haired_lawyer','rich_texan','quimby','fat_tony',
               'legs','louie','lionel','troy','artie_ziff',
               'rainier_wolfcastle','kodos','kang','itchy','scratchy',
               "santa's_little_helper",'snowball_ii','snowball_v','grumple',
               'crazy_cat_lady','cookie_kwan','lindsey_naegle','hibbert',
               'sarah_wiggum','üters','lewis','wendell','shauna','brandine',
               'cletus','julio','johnny_tightlips','smithers',
               'matt_groening','el_barto','hugo','amber_simpson','sara_sloane',
               'rachel_jordan','bonnie_flanders','connie_flanders',
               'agnes_flanders','agnes_turnipseed','agnes_flanders',
               'agnes_turnipseed','agnes_flanders','agnes_turnipseed',
               
               "peter", "lois", "meg", "chris", "stewie", "brian",
               "quagmire", "cleveland", "joe", "bonnie",
               "kevin_swanson", "susie_swanson", "mort", "muriel_goldman", "neil_goldman",
               "carter", "barbara", "carol_pewterschmidt",
               "herbert", "consuela", "tom", "jake_tucker", "diane_simmons", "ollie_williams",
               "tricia", "seamus", "dr._hartman", "jerome", "bruce", "connie_d'amico",
               "vinny", "mayor_west", "adam", "ernie_the_giant_chicken", "evil_monkey",
               "bertram", "rupert", "loretta_brown", "cleveland_brown_jr.", "roberta_tubbs",
               "rallo_tubbs", "angela", "death", "buzz_killington", "paddy_tanniger",
               "al_harrington", "dr._amanda_rebecca", "evil_stewie", "retep", "king_butt",
               "human_rupert", "brain_damage_horse", "felicia_day", "ron_perlman", "patrick_stewart",
               "jillian_russell", "denise_robertson", "connie", "shauna", "alice", "babs",
               "vinny_griffin", "carol", "derek_wilcox", "jesus_christ", "god", "santa_claus",
               'joe', 'griffin', 'simpson', 'quagmire', 'springfield', 'tom', 'flanders', 'willie',
               'simpsons', 'griffin', 'glenn', 'bonnie'
)

rm(merged_df)
gc()

# Rimozione delle stopword personalizzate
frequenze_simpson <- frequenze_simpson %>% filter(!word %in% c(stopwords_nostre, personaggi))
frequenze_griffin <- frequenze_griffin %>% filter(!word %in% c(stopwords_nostre, personaggi))

# Funzione per pulizia testo
clean_text <- function(df, remove_personaggi = TRUE) {
  sw <- if (remove_personaggi) c(stop_words$word, stopwords_nostre, personaggi)
  else c(stop_words$word, stopwords_nostre)
  
  df %>%
    unnest_tokens(word, testo) %>%
    filter(!word %in% sw) %>%
    filter(str_detect(word, "^[a-z]+$")) %>% #rimuove numeri
    filter(nchar(word) > 2 | word %in% c("tv", "dr")) %>%
    group_by(stagione, episodio) %>%
    summarise(testo = str_c(word, collapse = " "), .groups = "drop")
}

# Pulizia testi
simpson_clean     <- clean_text(simpson, TRUE)
griffin_clean     <- clean_text(griffin, TRUE)
simpson_clean_2   <- clean_text(simpson, FALSE)
griffin_clean_2   <- clean_text(griffin, FALSE)
################################################################################


## DA QUI ALLA FINE DA METTERE NEL REPORT
# Funzione per creare DTM da testo
create_dtm <- function(df, stoplist = NULL, prefix = NULL) {
  df %>%
    mutate(
      doc_prefix = if (!is.null(prefix)) {
        prefix
      } else if ("prefix" %in% colnames(df)) {
        prefix  # usa la colonna esistente
      } else {
        "X"  # fallback
      },
      doc_id = paste0(
        doc_prefix, "_S", ifelse(stagione < 10, paste0("0", stagione), stagione),
        "_E", ifelse(episodio < 10, paste0("0", episodio), episodio)
      )
    ) %>%
    unnest_tokens(word, testo) %>%
    mutate(word = tolower(word)) %>%
    filter(str_detect(word, "^[a-z]+$")) %>%
    mutate(word = lemmatize_words(word)) %>%
    filter(str_detect(word, "^[a-z]+$")) %>%
    filter(!word %in% stoplist) %>%
    filter(nchar(word) > 2 | word %in% c("tv", "dr", "mr")) %>%
    count(doc_id, word) %>%
    cast_dtm(document = doc_id, term = word, value = n)
}



# Input: oggetti totk_topics_2
# num_topics da passare: k, se dà errore provare k-1, etc
plot_topics <- function(data, num_topics = 3, prefix = NULL) {
  
  # Generazione automatica dei nomi dei topic
  nam <- paste0("Topic ", 1:num_topics)
  
  # Filtro opzionale sui documenti
  if (!is.null(prefix)) {
    data <- data %>%
      filter(substr(document, 1, 1) == prefix)
  }
  
  # Estrazione delle stagioni e dei topic
  seasons <- data %>%
    pull(document) %>%
    substr(start = 4, stop = 5)
  
  topics <- data %>%
    pull(topic)
  
  # Creazione delle tabelle
  ta <- table(topics, seasons)
  tab <- c(ta)
  somme <- colSums(ta)
  tab2 <- c(ta / matrix(rep(somme, num_topics), nrow = num_topics, byrow = TRUE))
  
  # Numero di stagioni effettive
  stagioni_disponibili <- sort(as.integer(unique(seasons)))
  
  # Creazione del dataframe
  dati <- data.frame(
    Season = rep(stagioni_disponibili, each = num_topics),
    Topic = rep(nam, times = length(stagioni_disponibili)),
    Count = tab,
    Perc = tab2
  )
  
  dati$Topic <- factor(dati$Topic, levels = nam)
  dati$Season <- factor(dati$Season, levels = stagioni_disponibili)
  
  # Ordinamento dei dati
  arranged_data <- dati %>%
    group_by(Season) %>%
    arrange(Topic)
  
  arranged_data$Topic <- factor(arranged_data$Topic, levels = unique(arranged_data$Topic))
  
  # Grafico 1 - Conteggio assoluto
  p1 <- ggplot(arranged_data, aes(x = as.numeric(Season), y = Count, fill = Topic)) +
    geom_area() +
    labs(title = "Episodi per topic, nelle stagioni",
         x = "Stagione",
         y = "Numero di episodi",
         fill = "Topic") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = 'none'
    ) +
    scale_x_continuous(breaks = stagioni_disponibili, labels = levels(arranged_data$Season))
  
  # Grafico 2 - Percentuali
  p2 <- ggplot(arranged_data, aes(x = as.numeric(Season), y = Perc, fill = Topic)) +
    geom_area() +
    labs(title = "Percentuale di episodi per Topic, nelle stagioni",
         x = "Stagione",
         y = "Percentuale di episodi",
         fill = "Topic") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = 'none'
    ) +
    scale_x_continuous(breaks = stagioni_disponibili, labels = levels(arranged_data$Season))
  
  # Legenda
  p_legend <- ggplot(arranged_data, aes(x = as.numeric(Season), y = Perc, fill = Topic)) +
    geom_area() +
    guides(fill = guide_legend(override.aes = list(size = 5)))
  
  leg <- get_legend(p_legend)
  p5 <- ggdraw(leg)
  
  # Unione dei grafici
  grid.arrange(p1, p2, p5, nrow = 1, widths = c(1, 1, 0.22))
}

analyze_lda <- function(lda_list_1 = lda_k, lda_list_2 = lda_k2, k, n_top_terms = 50, n_top_docs = 15) {
  
  # Estrazione delle matrici beta e gamma
  word_topics_1 <- tidy(lda_list_1[[k]], matrix = "beta")
  doc_topics_1 <- tidy(lda_list_1[[k]], matrix = "gamma")
  
  word_topics_2 <- tidy(lda_list_2[[k]], matrix = "beta")
  doc_topics_2 <- tidy(lda_list_2[[k]], matrix = "gamma")
  
  # Estrazione dei termini principali
  top_terms_1 <- word_topics_1 %>%
    group_by(topic) %>%
    slice_max(beta, n = n_top_terms) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  top_terms_2 <- word_topics_2 %>%
    group_by(topic) %>%
    slice_max(beta, n = n_top_terms) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  # Visualizzazione dei termini principali
  print(
    top_terms_1 %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(beta, term, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      scale_y_reordered() +
      labs(title = paste("Top", n_top_terms, "terms per topic (LDA 1)"))
  )
  
  print(
    top_terms_2 %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(beta, term, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      scale_y_reordered() +
      labs(title = paste("Top", n_top_terms, "terms per topic (LDA 2)"))
  )
  
  # Estrazione dei documenti principali
  top_docs_1 <- doc_topics_1 %>%
    group_by(topic) %>%
    slice_max(gamma, n = n_top_docs) %>%
    ungroup() %>%
    arrange(topic, -gamma)
  
  top_docs_2 <- doc_topics_2 %>%
    group_by(topic) %>%
    slice_max(gamma, n = n_top_docs) %>%
    ungroup() %>%
    arrange(topic, -gamma)
  
  # Classificazione dei documenti per topic principale
  topics_1 <- doc_topics_1 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1) %>%
    ungroup()
  
  topics_2 <- doc_topics_2 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1) %>%
    ungroup()
  
  # Nell'ambiente globale
  assign(paste0("tot", k, "_topics"), topics_1, envir = .GlobalEnv)
  assign(paste0("tot", k, "_topics_2"), topics_2, envir = .GlobalEnv)
  
  # Tabelle di controllo
  cat("\n========== CLASSIFICAZIONE SENZA PERSONAGGI ==========\n")
  print(table(substr(topics_1$document, 1, 1), topics_1$topic))
  cat("\n=========== CLASSIFICAZIONE CON PERSONAGGI ===========\n")
  print(table(substr(topics_2$document, 1, 1), topics_2$topic))
  
  # Output dei dati utili
  list(
    top_terms_1 = top_terms_1,
    top_terms_2 = top_terms_2,
    top_docs_1 = top_docs_1,
    top_docs_2 = top_docs_2,
    topics_1 = topics_1,
    topics_2 = topics_2
  )
}

analyze_lda_serie <- function(lda_list_1 = lda_k, lda_list_2 = lda_k2, k, n_top_terms = 50, n_top_docs = 15, serie_nome = "Serie") {
  
  # Estrazione delle matrici beta e gamma
  word_topics_1 <- tidy(lda_list_1[[k]], matrix = "beta")
  doc_topics_1 <- tidy(lda_list_1[[k]], matrix = "gamma")
  
  word_topics_2 <- tidy(lda_list_2[[k]], matrix = "beta")
  doc_topics_2 <- tidy(lda_list_2[[k]], matrix = "gamma")
  
  # Estrazione dei termini principali
  top_terms_1 <- word_topics_1 %>%
    group_by(topic) %>%
    slice_max(beta, n = n_top_terms) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  top_terms_2 <- word_topics_2 %>%
    group_by(topic) %>%
    slice_max(beta, n = n_top_terms) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  # Grafici con titoli specifici
  print(
    top_terms_1 %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(beta, term, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      scale_y_reordered() +
      labs(title = paste("Top", n_top_terms, "terms per topic (LDA 1) -", serie_nome))
  )
  
  print(
    top_terms_2 %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(beta, term, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      scale_y_reordered() +
      labs(title = paste("Top", n_top_terms, "terms per topic (LDA 2) -", serie_nome))
  )
  
  # Estrazione documenti principali
  top_docs_1 <- doc_topics_1 %>%
    group_by(topic) %>%
    slice_max(gamma, n = n_top_docs) %>%
    ungroup() %>%
    arrange(topic, -gamma)
  
  top_docs_2 <- doc_topics_2 %>%
    group_by(topic) %>%
    slice_max(gamma, n = n_top_docs) %>%
    ungroup() %>%
    arrange(topic, -gamma)
  
  # Classificazione per topic dominante
  topics_1 <- doc_topics_1 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1) %>%
    ungroup()
  
  topics_2 <- doc_topics_2 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1) %>%
    ungroup()
  
  # Classificazione per topic dominante
  topics_1 <- doc_topics_1 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1) %>%
    ungroup()
  
  topics_2 <- doc_topics_2 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1) %>%
    ungroup()
  
  # Stampa semplice delle frequenze per topic (come table)
  cat("\n========== NUMEROSITÀ TOPIC -Senza Personaggi ==========\n")
  print(table(topics_1$topic))
  
  cat("\n=========== NUMEROSITÀ TOPIC - Con Personaggi ===========\n")
  print(table(topics_2$topic))
  
  # Output utile
  list(
    top_terms_1 = top_terms_1,
    top_terms_2 = top_terms_2,
    top_docs_1 = top_docs_1,
    top_docs_2 = top_docs_2,
    topics_1 = topics_1,
    topics_2 = topics_2
  )
}

analyze_lda_titolati <- function(lda_list_1 = lda_k, lda_list_2 = lda_k2, k,
                                 n_top_terms = 50, n_top_docs = 15,
                                 custom_titles_1 = NULL, custom_titles_2 = NULL) {
  
  # Estrazione delle matrici beta e gamma
  word_topics_1 <- tidy(lda_list_1[[k]], matrix = "beta")
  doc_topics_1 <- tidy(lda_list_1[[k]], matrix = "gamma")
  
  word_topics_2 <- tidy(lda_list_2[[k]], matrix = "beta")
  doc_topics_2 <- tidy(lda_list_2[[k]], matrix = "gamma")
  
  # Creazione dei titoli personalizzati
  if (is.null(custom_titles_1)) {
    custom_titles_1 <- paste("Topic", 1:k)
  }
  if (is.null(custom_titles_2)) {
    custom_titles_2 <- paste("Topic", 1:k)
  }
  
  # Verifica che il numero di titoli corrisponda al numero di topic
  if (length(custom_titles_1) != k) {
    stop(paste("Il numero di titoli per LDA 1 (", length(custom_titles_1),
               ") non corrisponde al numero di topic (", k, ")"))
  }
  if (length(custom_titles_2) != k) {
    stop(paste("Il numero di titoli per LDA 2 (", length(custom_titles_2),
               ") non corrisponde al numero di topic (", k, ")"))
  }
  
  # Estrazione dei termini principali
  top_terms_1 <- word_topics_1 %>%
    group_by(topic) %>%
    slice_max(beta, n = n_top_terms) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(topic_title = custom_titles_1[topic])
  
  top_terms_2 <- word_topics_2 %>%
    group_by(topic) %>%
    slice_max(beta, n = n_top_terms) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(topic_title = custom_titles_2[topic])
  
  # Visualizzazione dei termini principali con titoli personalizzati
  plot1 <- top_terms_1 %>%
    mutate(term = reorder_within(term, beta, topic_title)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic_title, scales = "free") +
    scale_y_reordered() +
    labs(title = paste("Top", n_top_terms, "terms per topic (LDA 1 - Senza Personaggi)")) +
    theme(strip.text = element_text(size = 8))
  
  plot2 <- top_terms_2 %>%
    mutate(term = reorder_within(term, beta, topic_title)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic_title, scales = "free") +
    scale_y_reordered() +
    labs(title = paste("Top", n_top_terms, "terms per topic (LDA 2 - Con Personaggi)")) +
    theme(strip.text = element_text(size = 8))
  
  print(plot1)
  print(plot2)
  
  # Estrazione dei documenti principali
  top_docs_1 <- doc_topics_1 %>%
    group_by(topic) %>%
    slice_max(gamma, n = n_top_docs) %>%
    ungroup() %>%
    arrange(topic, -gamma) %>%
    mutate(topic_title = custom_titles_1[topic])
  
  top_docs_2 <- doc_topics_2 %>%
    group_by(topic) %>%
    slice_max(gamma, n = n_top_docs) %>%
    ungroup() %>%
    arrange(topic, -gamma) %>%
    mutate(topic_title = custom_titles_2[topic])
  
  # Classificazione dei documenti per topic principale
  topics_1 <- doc_topics_1 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1) %>%
    ungroup() %>%
    mutate(topic_title = custom_titles_1[topic])
  
  topics_2 <- doc_topics_2 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1) %>%
    ungroup() %>%
    mutate(topic_title = custom_titles_2[topic])
  
  # Nell'ambiente globale
  assign(paste0("tot", k, "_topics"), topics_1, envir = .GlobalEnv)
  assign(paste0("tot", k, "_topics_2"), topics_2, envir = .GlobalEnv)
  
  # Tabelle di controllo con titoli
  cat("\n========== CLASSIFICAZIONE SENZA PERSONAGGI ==========\n")
  print(table(substr(topics_1$document, 1, 1), topics_1$topic_title))
  cat("\n=========== CLASSIFICAZIONE CON PERSONAGGI ===========\n")
  print(table(substr(topics_2$document, 1, 1), topics_2$topic_title))
  
  # Output dei dati utili
  list(
    top_terms_1 = top_terms_1,
    top_terms_2 = top_terms_2,
    top_docs_1 = top_docs_1,
    top_docs_2 = top_docs_2,
    topics_1 = topics_1,
    topics_2 = topics_2,
    custom_titles_1 = custom_titles_1,
    custom_titles_2 = custom_titles_2
  )
}

analyze_lda_titolati_table <- function(lda_list_1 = lda_k, lda_list_2 = lda_k2, k,
                                       n_top_terms = 50, n_top_docs = 15,
                                       custom_titles_1 = NULL, custom_titles_2 = NULL) {
  
  # Estrazione delle matrici beta e gamma
  word_topics_1 <- tidy(lda_list_1[[k]], matrix = "beta")
  doc_topics_1 <- tidy(lda_list_1[[k]], matrix = "gamma")
  
  word_topics_2 <- tidy(lda_list_2[[k]], matrix = "beta")
  doc_topics_2 <- tidy(lda_list_2[[k]], matrix = "gamma")
  
  # Creazione dei titoli personalizzati
  if (is.null(custom_titles_1)) {
    custom_titles_1 <- paste("Topic", 1:k)
  }
  if (is.null(custom_titles_2)) {
    custom_titles_2 <- paste("Topic", 1:k)
  }
  
  # Verifica che il numero di titoli corrisponda al numero di topic
  if (length(custom_titles_1) != k) {
    stop(paste("Il numero di titoli per LDA 1 (", length(custom_titles_1),
               ") non corrisponde al numero di topic (", k, ")"))
  }
  if (length(custom_titles_2) != k) {
    stop(paste("Il numero di titoli per LDA 2 (", length(custom_titles_2),
               ") non corrisponde al numero di topic (", k, ")"))
  }
  
  # Estrazione dei termini principali
  top_terms_1 <- word_topics_1 %>%
    group_by(topic) %>%
    slice_max(beta, n = n_top_terms) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(topic_title = custom_titles_1[topic])
  
  top_terms_2 <- word_topics_2 %>%
    group_by(topic) %>%
    slice_max(beta, n = n_top_terms) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(topic_title = custom_titles_2[topic])
  
  # Visualizzazione dei termini principali con titoli personalizzati
  plot1 <- top_terms_1 %>%
    mutate(term = reorder_within(term, beta, topic_title)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic_title, scales = "free") +
    scale_y_reordered() +
    labs(title = paste("Top", n_top_terms, "terms per topic (LDA 1 - Senza Personaggi)")) +
    theme(strip.text = element_text(size = 8))
  
  plot2 <- top_terms_2 %>%
    mutate(term = reorder_within(term, beta, topic_title)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic_title, scales = "free") +
    scale_y_reordered() +
    labs(title = paste("Top", n_top_terms, "terms per topic (LDA 2 - Con Personaggi)")) +
    theme(strip.text = element_text(size = 8))
  
  print(plot1)
  print(plot2)
  
  # Estrazione dei documenti principali
  top_docs_1 <- doc_topics_1 %>%
    group_by(topic) %>%
    slice_max(gamma, n = n_top_docs) %>%
    ungroup() %>%
    arrange(topic, -gamma) %>%
    mutate(topic_title = custom_titles_1[topic])
  
  top_docs_2 <- doc_topics_2 %>%
    group_by(topic) %>%
    slice_max(gamma, n = n_top_docs) %>%
    ungroup() %>%
    arrange(topic, -gamma) %>%
    mutate(topic_title = custom_titles_2[topic])
  
  # Classificazione dei documenti per topic principale
  topics_1 <- doc_topics_1 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1) %>%
    ungroup() %>%
    mutate(topic_title = custom_titles_1[topic])
  
  topics_2 <- doc_topics_2 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1) %>%
    ungroup() %>%
    mutate(topic_title = custom_titles_2[topic])
  
  # Nell'ambiente globale
  assign(paste0("tot", k, "_topics"), topics_1, envir = .GlobalEnv)
  assign(paste0("tot", k, "_topics_2"), topics_2, envir = .GlobalEnv)
  
  # Tabelle di controllo con titoli in formato Markdown-friendly
  # cat("\n\n### Classificazione per topic - LDA 1 (Senza Personaggi)\n\n")
  tab1 <- as.data.frame.matrix(table(substr(topics_1$document, 1, 1), topics_1$topic_title))
  tab1 <- tab1[, c(ncol(tab1), 1:(ncol(tab1)-1))]
  cat(knitr::kable(tab1, caption = "Distribuzione dei documenti per topic - LDA 1 (Senza Personaggi)", format = "html"))
  
  # cat("\n\n### Classificazione per topic - LDA 2 (Con Personaggi)\n\n")
  tab2 <- as.data.frame.matrix(table(substr(topics_2$document, 1, 1), topics_2$topic_title))
  tab2$Gruppo <- rownames(tab2)
  tab2 <- tab2[, c(ncol(tab2), 1:(ncol(tab2)-1))]
  cat(knitr::kable(tab2, caption = "Distribuzione dei documenti per topic - LDA 2 (Con Personaggi)", format = "html"))
  
  
  # Output dei dati utili
  list(
    top_terms_1 = top_terms_1,
    top_terms_2 = top_terms_2,
    top_docs_1 = top_docs_1,
    top_docs_2 = top_docs_2,
    topics_1 = topics_1,
    topics_2 = topics_2,
    custom_titles_1 = custom_titles_1,
    custom_titles_2 = custom_titles_2
  )
}

analyze_lda_serie_titolati <- function(lda_list_1 = lda_k, lda_list_2 = lda_k2, k,
                                       n_top_terms = 50, n_top_docs = 15, serie_nome = "Serie",
                                       custom_titles_1 = NULL, custom_titles_2 = NULL) {
  
  # Estrazione delle matrici beta e gamma
  word_topics_1 <- tidy(lda_list_1[[k]], matrix = "beta")
  doc_topics_1 <- tidy(lda_list_1[[k]], matrix = "gamma")
  
  word_topics_2 <- tidy(lda_list_2[[k]], matrix = "beta")
  doc_topics_2 <- tidy(lda_list_2[[k]], matrix = "gamma")
  
  # Creazione dei titoli personalizzati
  if (is.null(custom_titles_1)) {
    custom_titles_1 <- paste("Topic", 1:k)
  }
  if (is.null(custom_titles_2)) {
    custom_titles_2 <- paste("Topic", 1:k)
  }
  
  # Verifica che il numero di titoli corrisponda al numero di topic
  if (length(custom_titles_1) != k) {
    stop(paste("Il numero di titoli per LDA 1 (", length(custom_titles_1),
               ") non corrisponde al numero di topic (", k, ")"))
  }
  if (length(custom_titles_2) != k) {
    stop(paste("Il numero di titoli per LDA 2 (", length(custom_titles_2),
               ") non corrisponde al numero di topic (", k, ")"))
  }
  
  # Estrazione dei termini principali
  top_terms_1 <- word_topics_1 %>%
    group_by(topic) %>%
    slice_max(beta, n = n_top_terms) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(topic_title = custom_titles_1[topic])
  
  top_terms_2 <- word_topics_2 %>%
    group_by(topic) %>%
    slice_max(beta, n = n_top_terms) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(topic_title = custom_titles_2[topic])
  
  # Grafici con titoli personalizzati
  plot1 <- top_terms_1 %>%
    mutate(term = reorder_within(term, beta, topic_title)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic_title, scales = "free") +
    scale_y_reordered() +
    labs(title = paste("Top", n_top_terms, "terms per topic (LDA 1 - Senza Personaggi) -", serie_nome)) +
    theme(strip.text = element_text(size = 8))
  
  plot2 <- top_terms_2 %>%
    mutate(term = reorder_within(term, beta, topic_title)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic_title, scales = "free") +
    scale_y_reordered() +
    labs(title = paste("Top", n_top_terms, "terms per topic (LDA 2 - Con Personaggi) -", serie_nome)) +
    theme(strip.text = element_text(size = 8))
  
  print(plot1)
  print(plot2)
  
  # Estrazione documenti principali
  top_docs_1 <- doc_topics_1 %>%
    group_by(topic) %>%
    slice_max(gamma, n = n_top_docs) %>%
    ungroup() %>%
    arrange(topic, -gamma) %>%
    mutate(topic_title = custom_titles_1[topic])
  
  top_docs_2 <- doc_topics_2 %>%
    group_by(topic) %>%
    slice_max(gamma, n = n_top_docs) %>%
    ungroup() %>%
    arrange(topic, -gamma) %>%
    mutate(topic_title = custom_titles_2[topic])
  
  # Classificazione per topic dominante
  topics_1 <- doc_topics_1 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1) %>%
    ungroup() %>%
    mutate(topic_title = custom_titles_1[topic])
  
  topics_2 <- doc_topics_2 %>%
    group_by(document) %>%
    slice_max(gamma, n = 1) %>%
    ungroup() %>%
    mutate(topic_title = custom_titles_2[topic])
  
  # Nell'ambiente globale
  assign(paste0("tot", k, "_topics"), topics_1, envir = .GlobalEnv)
  assign(paste0("tot", k, "_topics_2"), topics_2, envir = .GlobalEnv)
  
  # Creazione delle tabelle con titoli personalizzati
  table_1 <- topics_1$topic_title
  names(table_1) <- topics_1$topic_title
  table_1 <- table(table_1)
  
  table_2 <- topics_2$topic_title
  names(table_2) <- topics_2$topic_title
  table_2 <- table(table_2)
  
  # Output utile
  list(
    top_terms_1 = top_terms_1,
    top_terms_2 = top_terms_2,
    top_docs_1 = top_docs_1,
    top_docs_2 = top_docs_2,
    topics_1 = topics_1,
    topics_2 = topics_2,
    custom_titles_1 = custom_titles_1,
    custom_titles_2 = custom_titles_2
  )
}

gc()
