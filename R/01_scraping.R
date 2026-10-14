library(rvest) #per manipolare HTML e XML
library(httr) 
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(purrr)
library(tidyverse)

# Estrai i link degli episodi ####
# 1. Leggi la pagina principale
url_base <- "https://www.springfieldspringfield.co.uk"
endpoint <- "/episode_scripts.php?tv-show="
programmi <- c("the-simpsons", "family-guy")

# Funzione per ottenere i link completi degli episodi di un programma
estrai_link <- function(show) {
  pagina <- read_html(paste0(url_base, endpoint, show))
  
  link_relativi <- pagina %>%
    html_elements(".season-episode-title") %>%
    html_attr("href") %>%
    na.omit()
  
  link_completi <- paste0(url_base, "/", link_relativi)
  
  return(link_completi)
}

# Esegui per ogni programma
link_episodi <- map(programmi, estrai_link) %>%
  setNames(programmi)

# Estrai i testi degli episodi ####

estrai_script <- function(url) {
  Sys.sleep(1)  # Evita sovraccarico (molto importante)
  testo <- read_html(url) %>%
    html_element(".scrolling-script-container") %>%  
    html_text(trim = TRUE)
  
  return(testo)
}

# Simpson ####
#script_simpson <- rep(NA, length(link_episodi[["the-simpsons"]]))

#for(i in 1:length(link_episodi[["the-simpsons"]])){ #sistema!!!!!
#  message("episodio ", i)
#  script_simpson[i] <- estrai_script(link_episodi[["the-simpsons"]][i])
#  save.image("data/Senza nome.RData")
#}

simpson_df <- tibble(testo=script_simpson)
simpson_df$stagione <- rep(NA, length(link_episodi[["the-simpsons"]]))
simpson_df$episodio <- rep(NA, length(link_episodi[["the-simpsons"]]))

for(i in 1:length(link_episodi[["the-simpsons"]])){
  simpson_df$stagione[i] <- substr(link_episodi[["the-simpsons"]][i], 97, 98) %>% as.numeric()
  simpson_df$episodio[i] <- substr(link_episodi[["the-simpsons"]][i], 100, 101) %>% as.numeric()
}

#save(simpson_df, file="data/Data_frame_Simpson.RData")

# Griffin ####
#script_griffin <- rep(NA, length(link_episodi[["family-guy"]]))

#for(i in 1:length(link_episodi[["family-guy"]])){
#  message("episodio ", i)
#  script_griffin[i] <- estrai_script(link_episodi[["family-guy"]][i])
#  save.image("data/Senza nome.RData")
#}

griffin_df <- tibble(testo=script_griffin)
griffin_df$stagione <- rep(NA, length(link_episodi[["family-guy"]]))
griffin_df$episodio <- rep(NA, length(link_episodi[["family-guy"]]))

#sistema lunghezza stringa
for(i in 1:length(link_episodi[["family-guy"]])){
  griffin_df$stagione[i] <- substr(link_episodi[["family-guy"]][i], 95, 96) %>% as.numeric()
  griffin_df$episodio[i] <- substr(link_episodi[["family-guy"]][i], 98, 99) %>% as.numeric()
}

save(griffin_df, file="data/Data_frame_Griffin.RData")

# Scraping IMDB ####
#giugno 2023
imdb <- tibble(rating = integer(),
               serie = character(),
               stagione = integer(),
               episodio = integer(),
               riassunto = character())

url_base_imdb <- "https://www.imdb.com"

for(i in 1:36){
  print(i)
  ratings <- read_html(paste0(url_base_imdb, paste0("/it/title/tt0096697/episodes/?season=",i,"&ref_=ttep"))) %>% 
    html_elements(".ipc-rating-star--rating") %>% 
    html_text2() %>% 
    str_replace_all(",", ".") %>%
    as.numeric()
  riassunto <- read_html(paste0(url_base_imdb, paste0("/it/title/tt0096697/episodes/?season=",i,"&ref_=ttep"))) %>% 
    html_elements(".ipc-html-content-inner-div") %>% 
    html_text2() 
  imdb <- bind_rows(imdb,
                    tibble(rating = ratings,
                    serie = rep("S", length(ratings)),
                    stagione = rep(i, length(ratings)),
                    episodio = 1:length(ratings),
                    riassunto = riassunto))
}

for(i in 1:22){
  print(i)
  ratings <- read_html(paste0(url_base_imdb, paste0("/it/title/tt0182576/episodes/?season=",i,"&ref_=ttep"))) %>% 
    html_elements(".ipc-rating-star--rating") %>% 
    html_text2() %>% 
    str_replace_all(",", ".") %>%
    as.numeric()
  riassunto <- read_html(paste0(url_base_imdb, paste0("/it/title/tt0182576/episodes/?season=",i,"&ref_=ttep"))) %>% 
    html_elements(".ipc-html-content-inner-div") %>% 
    html_text2() 
  imdb <- bind_rows(imdb,
                    tibble(rating = ratings,
                    serie = rep("G", length(ratings)),
                    stagione = rep(i, length(ratings)),
                    episodio = 1:length(ratings),
                    riassunto = riassunto))
}

ratings_g23 <- read_html(paste0(url_base_imdb, paste0("/it/title/tt0182576/episodes/?season=",23,"&ref_=ttep"))) %>% 
  html_elements(".ipc-rating-star--rating") %>% 
  html_text2() %>% 
  str_replace_all(",", ".") %>%
  as.numeric()
riassunto_g23 <- read_html(paste0(url_base_imdb, paste0("/it/title/tt0182576/episodes/?season=",23,"&ref_=ttep"))) %>% 
  html_elements(".ipc-html-content-inner-div") %>% 
  html_text2() 

imdb <- bind_rows(imdb,
                  tibble(rating = ratings_g23,
                         serie = rep("G", length(ratings_g23)),
                         stagione = rep(i, length(ratings_g23)),
                         episodio = 1:length(ratings_g23),
                         riassunto = riassunto_g23[1:13]))

imdb$doc_id <- paste0(imdb$serie,"_S", ifelse(imdb$stagione < 10, paste0("0", imdb$stagione), imdb$stagione), 
                                "_E", ifelse(imdb$episodio < 10, paste0("0", imdb$episodio), imdb$episodio))

save(imdb, file="data/Data_frame_IMDB2.RData")
