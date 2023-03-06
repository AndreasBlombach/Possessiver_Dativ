# DTA-Abfrage, 11.01.2021
# Andreas Blombach

library(tidyverse)
library(jsonlite)
library(httr)
library(writexl)
library(lubridate)

# Basisfunktion, um Abfragen ins passende Format für URLs zu
# überführen
query_encode <- function(query) {
  URLencode(query, reserved = TRUE) |> 
    str_replace_all("%20", "+")
}

# Standardabfragen, bei denen i.d.R. eine Abfrage und ein Korpus
# angegeben werden.
# Manche Korpora (wie das Webkorpus: "ibk_web_2016c") benötigen
# eine Authentifizierung. Dafür lassen sich der Funktion Username
# (user) und Passwort (pw) übergeben. Authentifikation via
# Shibboleth wäre natürlich noch schöner ...
# Da DWDS/Kaskade maximal 5000 Treffer auf einmal zurückgibt,
# überprüft die Funktion gleich, ob es mehr gibt, und ruft ggf.
# weitere ab, um die Ergebnisse zusammenzufügen.
# Korpusnamen findet man unter http://kaskade.dwds.de/dstar/
dwds_json <- function(query, corpus = "zeit",
                      user = NULL, pw = NULL,
                      limit = 5000, start = 1,
                      all = TRUE) {
  url1 <- paste0("http://kaskade.dwds.de/dstar/", corpus,
                 "/dstar.perl?q=", query_encode(query),
                 "&fmt=json&start=")
  url2 <- paste0(start, "&limit=", limit, "&ctx=8&debug=")
  url <- paste0(url1, url2)
  if (!(is.null(user) || is.null(pw))) {
    json <- GET(url, authenticate(user, pw)) |> 
      content(as = "text") |>
      jsonlite::fromJSON()
  } else {
    json <- GET(url) |> 
      content(as = "text") |>
      jsonlite::fromJSON()
  }
  # mehr Treffer als limit:
  # nicht elegant, aber funktioniert:
  if (all == TRUE && json$end_ < json$nhits_) {
    further.queries <- ceiling(json$nhits_ / limit) - 1
    new.start <- start
    for (i in 1:further.queries) {
      new.start <- new.start + limit
      url <- paste0(url1, paste0(new.start, "&limit=", limit, "&ctx=8&debug="))
      # count-Abfrage?
      if (str_detect(query, "^count\\(")) {
        if (!(is.null(user) || is.null(pw))) {
          new.hits <- GET(url, authenticate(user, pw)) |>
            content(as = "text") |>
            jsonlite::fromJSON() %>%
            .$counts_ |>
            as_tibble()
        } else {
          new.hits <- GET(url) |>
            content(as = "text") |>
            jsonlite::fromJSON() %>%
            .$counts_ |>
            as_tibble()
        }
        json$counts_ <- jsonlite::rbind_pages(list(as_tibble(json$counts_), new.hits))
      } else {
        # keine count-Abfrage:
        if (!(is.null(user) || is.null(pw))) {
          new.hits <- GET(url, authenticate(user, pw)) |>
            content(as = "text") |>
            jsonlite::fromJSON() %>%
            .$hits_
        } else {
          new.hits <- GET(url) |>
            content(as = "text") |>
            jsonlite::fromJSON() %>%
            .$hits_
        }
        json$hits_ <- jsonlite::rbind_pages(list(json$hits_, new.hits))
      }
    }
  } else if (json$end_ < json$nhits_) {
    json$nhits_ = json$end_
  }
  return(json)
}

# Funktion, um relevante Informationen herauszufiltern (dämliches
# Listenformat der Treffer) und als tibble auszugeben -- je nach
# Bedarf anpassen:
dwds_tibble <- function(query, corpus = "zeit",
                        user = NULL, pw = NULL,
                        limit = 5000, start = 1,
                        all = TRUE) {
  # Achtung: keine Input-Kontrolle
  json <- dwds_json(query, corpus, user, pw, limit, start, all)
  if (json$nhits > 0) {
    match <- character(length = json$nhits_)
    sentence <- character(length = json$nhits_)
    tokens <- list() # Liste von tibbles, enthält insbesondere noch die Trefferhervorhebungen
    for (n in 1:json$nhits_) {
      df <- json$hits_$ctx_[[n]][[2]] |> rowid_to_column()
      if (corpus %in% c("dta", "dtak", "dtae")) {
        match[n] <- json$hits_$matches[[n]]$v |> paste(collapse = " ")
      } else {
        match[n] <- json$hits_$matches[[n]]$w |> paste(collapse = " ")
      }
      sentence[n] <- paste(df$w, collapse = " ")
      tokens[[n]] <- as_tibble(df)
    }
    # Output ggf. von Verfügbarkeit der Metadaten abhängig machen:
    output <- tibble(
      match = match,
      sentence = sentence,
      tokens = tokens,
      collection = json$hits_$meta_$collection,
      author = json$hits_$meta_$author,
      title = json$hits_$meta_$title,
      textClass = json$hits_$meta_$textClass,
      bibl = json$hits_$meta_$bibl
    )
    if (corpus %in% c("dta", "dtak", "dtae")) {
      output <- output |>
        add_column(date = parse_date(json$hits_$meta_$date_, "%Y"),
                   textClassDTA = json$hits_$meta_$textClassDTA)
    } else {
      output <- output |>
        add_column(date = parse_date(json$hits_$meta_$date_),
                   url = json$hits_$meta_$url)
    }
  } else {
    output <- tibble(
      match = character(length = 0),
      sentence = character(length = 0),
      tokens = list(),
      collection = character(length = 0),
      author = character(length = 0),
      title = character(length = 0),
      textClass = character(length = 0),
      bibl = character(length = 0),
      date = character(length = 0)
    )
    if (corpus %in% c("dta", "dtak", "dtae")) {
      output <- output |>
        add_column(textClassDTA = character(length = 0))
    } else {
      output <- output |> add_column(url = character(length = 0))
    }
  }
  return(output)
}


# DTA: Tokens pro Jahr und Genre:
cnts_url <- str_c("http://kaskade.dwds.de/dstar/dta/dstar.perl?q=",
                  query_encode("count(* #sep) #by[Date, textClass]"),
                  "&fmt=json&start=1&limit=5000&ctx=8&debug=")
cnts <- GET(cnts_url) |>
  content(as = "text") |>
  jsonlite::fromJSON()
cnts <- cnts$counts_ |> as_tibble(.name_repair = "unique")
names(cnts) <- c("Count", "Year", "Genre")
cnts <- cnts |>
  mutate(Count = as.integer(Count),
         Year = year(Year))
cnts <- cnts |>
  mutate(Genre = na_if(Genre, ""),
         First_genre = str_remove(Genre, "::.*"))
cnts <- cnts |>
  mutate(Date_50 = cut(Year, breaks = seq(1450, 2000, by = 50)))
levels(cnts$Date_50) <- c("1450-1499", "1500-1549",
                          "1550-1599", "1600-1649",
                          "1650-1699", "1700-1749",
                          "1750-1799", "1800-1849",
                          "1850-1899", "1900-1949",
                          "1950-1999")

# speichern:
cnts |> saveRDS(file = "data/dta_cnts_genre_year.RDS")

# DTA-Abfrage:
dta <- dwds_tibble('"$v=/e[ms]$/i with $p=/(^ART|AT)$/ =1 $p=N* =2 $l=/^sein/i with $p=PPOSAT =3" || "$v=/e[rn]$/i with $p=/(^ART|AT)$/ =1 $p=N* = 2 $l=/^ihr/i with $p=PPOSAT =3" #sep', corpus = "dta")
# #sep sorgt dafür, dass mehrere Treffer innerhalb eines Satzes separat gezählt werden

dta <- dta |>
  mutate(token1 = str_extract(match, "^[^\\s]+"),
         token2 = str_extract(match, "(?<=\\s)[^\\s]+(?=\\s)"),
         token3 = str_extract(match, "[^\\s]+$"))

nrow(dta) # 50454

# Satzdopplungen mit denselben Treffern ausschließen
dta <- dta |> arrange(date) |>
  distinct(sentence, match, .keep_all = TRUE)

nrow(dta) # 49769

# sortieren:
dta <- dta |> arrange(token1, token3, token2)

# POS-Tags des Wortes _vor_ dem ersten und des Wortes _nach_ dem
# letzten Trefferwort in neue Spalte schreiben:
dta <- dta |>
  mutate(
    pos_before = unlist(
      modify_depth(
        tokens, 1, ~slice(., max(1, match(TRUE, hl_ == 1) - 1)) |> pull(p)
      )
    ),
    pos_after = unlist(
      modify_depth(
        tokens, 1, ~slice(., min(n() , match(TRUE, hl_ == 3) + 1)) |> pull(p)
      )
    )
  )

# Treffer im Satz markieren (um die Annotation zu erleichtern):
dta <- dta |>
  mutate(
    tokens2 = modify_depth(
      tokens, 1, ~mutate(., w = ifelse(hl_ == 1, str_c("--->", w), w),
                         w = ifelse(hl_ == 3, str_c(w, "<---"), w))
    )
  )

dta <- dta |>
  mutate(
    sentence2 = unlist(
      modify_depth(
        tokens2, 1, ~.$w %>% str_c(collapse = " ")
      )
    )
  )


# date-Spalte aufs Jahr reduzieren:
dta <- dta |> mutate(date = year(date))

# ganze Datei speichern (inkl. Token-Liste -- enthält getaggten Satz):
dta |> saveRDS(file = "data/dta_arbeitsversion.RDS")


# Tabelle zur Annotation:
xlsx <- dta |>
  mutate(cat = character(length = n()),
         alt_cat = character(length = n()),
         genus_possessor = character(length = n()),
         genus_possessum = character(length = n()),
         numerus_possessor = character(length = n()),
         numerus_possessum = character(length = n()),
         animacy_possessor = character(length = n()),
         animacy_possessum = character(length = n())) |>
  select(match, cat, alt_cat, genus_possessor, genus_possessum,
         numerus_possessor, numerus_possessum,
         animacy_possessor, animacy_possessum,
         sentence = sentence2, pos_before, pos_after, token1,
         token2, token3, date, collection, author, title,
         textClass, bibl, textClassDTA)

# automatisch vorannotierte Kategorie bei Präposition vor match
# (muss überprüft werden, spart aber Arbeit):
xlsx <- xlsx |>
  mutate(
    alt_cat = ifelse(pos_before == "APPR", "n", alt_cat),
    cat = alt_cat
  )

# speichern (ohne die Token-Listen):
xlsx |> write_xlsx("data/annotation.xlsx")