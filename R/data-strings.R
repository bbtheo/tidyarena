# tidyarena's string data: one table, `customers`, n rows, with the kinds of
# text real data has. Runs in a fresh R process with base R and nanoparquet.
#
# * `id`: row number.
# * `name`: first and last name from made-up name lists (300 first names,
#   1,000 last names), 10% in upper case, 10% in lower case and 10% with
#   extra spaces.
# * `email`: `first.last@domain`, 20 domains ending in .com, .org or .net.
# * `code`: product codes like `"QX-0042-VOZE"`: 50 two-letter prefixes, a
#   number and 200 suffixes.
# * `text`: a sentence of 3 to 12 words from 2,000 made-up words, nearly
#   unique per row.
# * `date_str`: a date from 2020 to 2024, as `"YYYY-MM-DD"`.
# * `value`: an amount from 0 to 500.
generate_strings <- function(n, seed, paths, options = list()) {
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  n <- as.integer(n)
  syllables <- c(
    "ka", "lo", "mi", "ne", "ru", "sa", "ti", "vo", "ze", "an",
    "el", "or", "is", "um", "ar", "en", "ba", "do", "fi", "gu"
  )
  # `k` distinct words of `size` syllables.
  words <- function(k, size) {
    out <- character()
    while (length(out) < k) {
      w <- character(k)
      for (i in seq_len(size)) {
        w <- paste0(w, sample(syllables, k, replace = TRUE))
      }
      out <- unique(c(out, w))
    }
    out[seq_len(k)]
  }
  capitalise <- function(x) {
    paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  }
  first <- capitalise(words(300, 2))
  last <- capitalise(words(1000, 3))
  vocabulary <- words(2000, 3)

  fi <- sample.int(300L, n, replace = TRUE)
  la <- sample.int(1000L, n, replace = TRUE)
  name <- paste(first[fi], last[la])
  mess <- stats::runif(n)
  name[mess < 0.1] <- toupper(name[mess < 0.1])
  name[mess >= 0.1 & mess < 0.2] <- tolower(name[mess >= 0.1 & mess < 0.2])
  spaced <- mess >= 0.2 & mess < 0.3
  name[spaced] <- paste0("  ", sub(" ", "   ", name[spaced]), " ")
  rm(mess, spaced)

  domains <- c(
    sprintf("mail%02d.com", 1:10),
    sprintf("web%02d.org", 1:5),
    sprintf("corp%02d.net", 1:5)
  )
  email <- paste0(
    tolower(first[fi]), ".", tolower(last[la]), "@",
    sample(domains, n, replace = TRUE)
  )
  rm(fi, la)

  prefixes <- sample(as.vector(outer(LETTERS, LETTERS, paste0)), 50)
  suffixes <- toupper(words(200, 2))
  code <- sprintf(
    "%s-%04d-%s",
    sample(prefixes, n, replace = TRUE),
    sample.int(9999L, n, replace = TRUE),
    sample(suffixes, n, replace = TRUE)
  )

  n_words <- sample(3:12, n, replace = TRUE)
  text <- paste(
    sample(vocabulary, n, replace = TRUE),
    sample(vocabulary, n, replace = TRUE),
    sample(vocabulary, n, replace = TRUE)
  )
  for (k in 4:12) {
    more <- n_words >= k
    text[more] <- paste(text[more], sample(vocabulary, sum(more), replace = TRUE))
  }
  rm(n_words, more)

  customers <- data.frame(
    id = seq_len(n),
    name = name,
    email = email,
    code = code,
    text = text,
    date_str = format(as.Date("2020-01-01") + sample.int(1827L, n, replace = TRUE) - 1L),
    value = round(stats::runif(n, 0, 500), 2)
  )
  nanoparquet::write_parquet(customers, paths$customers, compression = "uncompressed")
  list(customers = list(
    nrow = n,
    value = sum(customers$value),
    nchar_text = sum(as.double(nchar(customers$text)))
  ))
}
