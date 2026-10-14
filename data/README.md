# Cartella `data/`

Questa cartella è **volutamente vuota** nel repository.

I dati non vengono versionati per due motivi:

1. **Copyright** — i testi analizzati sono le trascrizioni dei dialoghi degli
   episodi de *I Simpson* e *I Griffin*, materiale protetto da copyright. Il
   repository condivide **solo il codice** che li raccoglie ed elabora, non i
   testi stessi.
2. **Peso e riproducibilità** — i file `.RData` (dati grezzi e risultati dei
   modelli) sono pesanti e completamente **rigenerabili** dagli script in
   [`../R/`](../R/).

## Come rigenerare i dati

Eseguendo gli script nell'ordine indicato, i file `.RData` verranno creati in
questa cartella:

| Script | Produce (in `data/`) |
|---|---|
| `R/01_scraping.R`      | `Data_frame_Simpson.RData`, `Data_frame_Griffin.RData`, `Data_frame_IMDB2.RData` |
| `R/02_preprocessing.R` | dataframe puliti (`simpson_clean`, `griffin_clean`, …) |
| `R/04_lda.R`           | modelli LDA (`lda_k_*`, `LDA_k_*`) |
| `R/05_stm.R`           | modelli STM |
| `R/06_sentiment.R`     | `tot9_topic.RData`, `tot7_topic.RData`, `tot5_topic.RData` |

> ⚠️ Lo scraping dipende dalla struttura HTML dei siti sorgente
> (*springfieldspringfield.co.uk* per i copioni, *IMDb* per i rating), che può
> cambiare nel tempo: i selettori CSS negli script potrebbero richiedere
> aggiornamenti. Lo script rispetta un `Sys.sleep(1)` tra le richieste.
