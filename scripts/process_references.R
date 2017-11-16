library(tidyverse)
library(rcrossref)
library(stringi)

try_refs <- src_sqlite("../preprocess-try/try.sqlite") %>%
  tbl("orig_citations") %>%
  collect() %>%
  right_join(read_csv("../preprocess-try/traits/references.csv"))

try_refs_proc <- try_refs %>%
  mutate(
    reference_proc = stri_enc_toascii(Reference)
  ) %>%
  select(reference_proc, n, ReferenceID) %>%
  filter(!grepl("unpub.|Unpublished literature compilation",
                reference_proc, ignore.case = TRUE))

crossref_results <- list()
for (i in seq_len(nrow(try_refs_proc))) {
  message(i, " of ", nrow(try_refs_proc))
  qry <- try_refs_proc$reference_proc[i]
  result <- cr_works(query = try_refs_proc$reference_proc[i], limit = 1)
  print(result$data$DOI)
  crossref_results[[i]] <- result
}

custom_dois <- c(
  `39` = "10.3334/ORNLDAAC/1106",
  `40` = "10.1890/ES10-00175.1",
  `43` = "10.3334/ORNLDAAC/1104",
  `113` = "10.1139/cjb-2012-0162",
  `116` = "10.3334/ORNLDAAC/1097"
)

crossref_dois <- map_chr(crossref_results, c("data", "DOI"), .default = NA)
crossref_dois[as.numeric(names(custom_dois))] <- unname(custom_dois)
keys <- sprintf("try_data_%02d", seq_along(crossref_dois))
keyrefs <- paste0("@", keys)

crossref_table_raw <- tibble(DOI = crossref_dois, key = keys)

write.table(
  crossref_table_raw,
  file = "scripts/ref_dois",
  quote = FALSE,
  sep = " -k ",
  row.names = FALSE,
  col.names = FALSE
)

crossref_table <- try_refs_proc %>%
  mutate(Reference = keyrefs) %>%
  select(Reference, Observations = n)

saveRDS(crossref_table, file = "extdata/crossref_table.rds")
