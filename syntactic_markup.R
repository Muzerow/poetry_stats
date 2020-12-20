
library(dplyr)
library(udpipe)
library(readr)
library(readxl)
library(tidyverse)
library(optparse)

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--infile"),
                     help = "Input file")
parser <- add_option(parser, c("-o", "--outdir"), 
                     help = "Output directory")
parser <- add_option(parser, c("-d", "--dictionary"), 
                     help = "Stress dictionary")
parser <- add_option(parser, c("-m", "--model"), 
                     help = "Udpipe model")
args <- parse_args(parser)

main <- function(args) {
  data <- read_excel(args$infile)
  base <- sub(".xlsx", "", basename(args$infile))
  stress_dict <- read.csv(args$dictionary)
  udmodel <- udpipe_load_model(file = args$model)
  
  x <- udpipe(data$LineText, object = udmodel) %>%
    filter(upos != "PUNCT") %>%
    dplyr::select(-paragraph_id, -sentence_id, -xpos, -feats, -deps, -misc)
  
  y <- x %>%
    mutate(v1 = regexpr('[аеёиоуыэюя]', str_to_lower(sentence)),
           t1 = str_to_lower(substr(x$sentence, start = v1 + 1, stop = str_length(sentence))),
           
           v2 = ifelse(v1 %in% c(-1,0), -1,
                       ifelse(t1 == "", -1, regexpr('[аеёиоуыэюя]', t1) + v1)),
           t2 = ifelse(t1 == "", "",
                       ifelse(str_detect(substr(t1, start = v2 - v1 + 1, stop = str_length(t1)), '[аеёиоуыэюя]') == FALSE,
                              "", substr(t1, start = v2 - v1 + 1, stop = str_length(t1)))),
           
           v3 = ifelse(v2 %in% c(-1,0), -1,
                       ifelse(t2 == "", -1, regexpr('[аеёиоуыэюя]', t2) + v2)),
           t3 = ifelse(t2 == "", "",
                       ifelse(str_detect(substr(t2, start = v3 - v2 + 1, stop = str_length(t2)), '[аеёиоуыэюя]') == FALSE,
                              "", substr(t2, start = v3 - v2 + 1, stop = str_length(t2)))),
           
           v4 = ifelse(v3 %in% c(-1,0), -1,
                       ifelse(t3 == "", -1, regexpr('[аеёиоуыэюя]', t3) + v3)),
           t4 = ifelse(t3 == "", "",
                       ifelse(str_detect(substr(t3, start = v4 - v3 + 1, stop = str_length(t3)), '[аеёиоуыэюя]') == FALSE,
                              "", substr(t3, start = v4 - v3 + 1, stop = str_length(t3)))),
           
           v5 = ifelse(v4 %in% c(-1,0), -1,
                       ifelse(t4 == "", -1, regexpr('[аеёиоуыэюя]', t4) + v4)),
           t5 = ifelse(t4 == "", "",
                       ifelse(str_detect(substr(t4, start = v5 - v4 + 1, stop = str_length(t4)), '[аеёиоуыэюя]') == FALSE,
                              "", substr(t4, start = v5 - v4 + 1, stop = str_length(t4)))),
           
           v6 = ifelse(v5 %in% c(-1,0), -1,
                       ifelse(t5 == "", -1, regexpr('[аеёиоуыэюя]', t5) + v5)),
           t6 = ifelse(t5 == "", "",
                       ifelse(str_detect(substr(t5, start = v6 - v5 + 1, stop = str_length(t5)), '[аеёиоуыэюя]') == FALSE,
                              "", substr(t5, start = v6 - v5 + 1, stop = str_length(t5)))),
           
           v7 = ifelse(v6 %in% c(-1,0), -1,
                       ifelse(t6 == "", -1, regexpr('[аеёиоуыэюя]', t6) + v6)),
           t7 = ifelse(t6 == "", "",
                       ifelse(str_detect(substr(t6, start = v7 - v6 + 1, stop = str_length(t6)), '[аеёиоуыэюя]') == FALSE,
                              "", substr(t6, start = v7 - v6 + 1, stop = str_length(t6)))),
           
           v8 = ifelse(v7 %in% c(-1,0), -1,
                       ifelse(t7 == "", -1, regexpr('[аеёиоуыэюя]', t7) + v7)),
           t8 = ifelse(t7 == "", "",
                       ifelse(str_detect(substr(t7, start = v8 - v7 + 1, stop = str_length(t7)), '[аеёиоуыэюя]') == FALSE,
                              "", substr(t7, start = v8 - v7 + 1, stop = str_length(t7)))),
           
           v9 = ifelse(v8 %in% c(-1,0), -1,
                       ifelse(t8 == "", -1, regexpr('[аеёиоуыэюя]', t8) + v8)),
           t9 = ifelse(t8 == "", "",
                       ifelse(str_detect(substr(t8, start = v9 - v8 + 1, stop = str_length(t8)), '[аеёиоуыэюя]') == FALSE,
                              "", substr(t8, start = v9 - v8 + 1, stop = str_length(t8)))),
           
           token = str_to_lower(token)) %>%
    
    left_join(stress_dict, by = c("token")) %>%
    mutate(position = regexpr("[`'`]", stress) + start - 2,
           Ictus1 = ifelse(v2 == position & !(upos %in% c("CCONJ","SCONJ","PART","ADP")) |
                             v2 == position & upos %in% c("CCONJ","SCONJ","PART","ADP") & str_count(str_to_lower(token), "[аеёиоуыэюя]") > 1,
                           upos, NA),
           Ictus2 = ifelse(v4 == position & !(upos %in% c("CCONJ","SCONJ","PART","ADP")) |
                             v4 == position & upos %in% c("CCONJ","SCONJ","PART","ADP") & str_count(str_to_lower(token), "[аеёиоуыэюя]") > 1,
                           upos, NA),
           Ictus3 = ifelse(v6 == position & !(upos %in% c("CCONJ","SCONJ","PART","ADP")) |
                             v6 == position & upos %in% c("CCONJ","SCONJ","PART","ADP") & str_count(str_to_lower(token), "[аеёиоуыэюя]") > 1,
                           upos, NA),
           Ictus4 = ifelse(v8 == position & !(upos %in% c("CCONJ","SCONJ","PART","ADP")) |
                             v8 == position & upos %in% c("CCONJ","SCONJ","PART","ADP") & str_count(str_to_lower(token), "[аеёиоуыэюя]") > 1,
                           upos, NA),
           HMS = ifelse(v1 == position & !(upos %in% c("CCONJ","SCONJ","PART","ADP","PRON","DET")) |
                          v1 == position & upos %in% c("CCONJ","SCONJ","PART","ADP","PRON","DET") &
                          str_count(str_to_lower(token), "[аеёиоуыэюя]") > 1, "1",
                        ifelse(v3 == position & !(upos %in% c("CCONJ","SCONJ","PART","ADP","PRON","DET")) |
                                 v3 == position & upos %in% c("CCONJ","SCONJ","PART","ADP","PRON","DET") &
                                 str_count(str_to_lower(token), "[аеёиоуыэюя]") > 1, "2",
                               ifelse(v5 == position & !(upos %in% c("CCONJ","SCONJ","PART","ADP","PRON","DET")) |
                                        v5 == position & upos %in% c("CCONJ","SCONJ","PART","ADP","PRON","DET") &
                                        str_count(str_to_lower(token), "[аеёиоуыэюя]") > 1, "3",
                                      ifelse(v7 == position & !(upos %in% c("CCONJ","SCONJ","PART","ADP","PRON","DET")) |
                                               v7 == position & upos %in% c("CCONJ","SCONJ","PART","ADP","PRON","DET") &
                                               str_count(str_to_lower(token), "[аеёиоуыэюя]") > 1, "4", NA)))),
           id_1 = ifelse(!is.na(Ictus1), token_id, NA),
           h_id_1 = ifelse(!is.na(Ictus1), head_token_id, NA),
           r_1 = ifelse(!is.na(Ictus1), dep_rel, NA),
           
           id_2 = ifelse(!is.na(Ictus2), token_id, NA),
           h_id_2 = ifelse(!is.na(Ictus2), head_token_id, NA),
           r_2 = ifelse(!is.na(Ictus2), dep_rel, NA),
           
           id_3 = ifelse(!is.na(Ictus3), token_id, NA),
           h_id_3 = ifelse(!is.na(Ictus3), head_token_id, NA),
           r_3 = ifelse(!is.na(Ictus3), dep_rel, NA),
           
           id_4 = ifelse(!is.na(Ictus4), token_id, NA),
           h_id_4 = ifelse(!is.na(Ictus4), head_token_id, NA),
           r_4 = ifelse(!is.na(Ictus4), dep_rel, NA)) %>% 
    
    dplyr::select(doc_id, Ictus1, Ictus2, Ictus3, Ictus4, HMS, id_1, h_id_1, r_1, id_2, h_id_2, r_2, id_3, h_id_3, r_3, id_4, h_id_4, r_4) %>%
    group_by(doc_id) %>% 
    summarise_all(funs(trimws(paste(., collapse = '')))) %>%
    mutate(Ictus1 = str_replace_all(Ictus1, "NA", ""),
           Ictus2 = str_replace_all(Ictus2, "NA", ""),
           Ictus3 = str_replace_all(Ictus3, "NA", ""),
           Ictus4 = str_replace_all(Ictus4, "NA", ""),
           
           HMS = str_replace_all(HMS, "NA", ""),
           
           id_1 = str_replace_all(id_1, "NA", ""),
           h_id_1 = str_replace_all(h_id_1, "NA", ""),
           r_1 = str_replace_all(r_1, "NA", ""),
           
           id_2 = str_replace_all(id_2, "NA", ""),
           h_id_2 = str_replace_all(h_id_2, "NA", ""),
           r_2 = str_replace_all(r_2, "NA", ""),
           
           id_3 = str_replace_all(id_3, "NA", ""),
           h_id_3 = str_replace_all(h_id_3, "NA", ""),
           r_3 = str_replace_all(r_3, "NA", ""),
           
           id_4 = str_replace_all(id_4, "NA", ""),
           h_id_4 = str_replace_all(h_id_4, "NA", ""),
           r_4 = str_replace_all(r_4, "NA", "")) %>%
    mutate(Ictus1 = ifelse(Ictus1 == "", NA, Ictus1),
           Ictus2 = ifelse(Ictus2 == "", NA, Ictus2),
           Ictus3 = ifelse(Ictus3 == "", NA, Ictus3),
           Ictus4 = ifelse(Ictus4 == "", NA, Ictus4),
           
           HMS = ifelse(HMS == "", NA, HMS),
           
           id_1 = ifelse(id_1 == "", NA, id_1),
           h_id_1 = ifelse(h_id_1 == "", NA, h_id_1),
           r_1 = ifelse(r_1 == "", NA, r_1),
           
           id_2 = ifelse(id_2 == "", NA, id_2),
           h_id_2 = ifelse(h_id_2 == "", NA, h_id_2),
           r_2 = ifelse(r_2 == "", NA, r_2),
           
           id_3 = ifelse(id_3 == "", NA, id_3),
           h_id_3 = ifelse(h_id_3 == "", NA, h_id_3),
           r_3 = ifelse(r_3 == "", NA, r_3),
           
           id_4 = ifelse(id_4 == "", NA, id_4),
           h_id_4 = ifelse(h_id_4 == "", NA, h_id_4),
           r_4 = ifelse(r_4 == "", NA, r_4)) %>%
    left_join((x %>% dplyr::count(doc_id, sentence) %>% dplyr::select(-n)), by = c("doc_id"))
  
  y[is.na(y)] <- "0"
  
  y <- y %>%
    mutate(RhythmForm = ifelse(Ictus1 != "0" & Ictus2 != "0" & Ictus3 != "0" & Ictus4 != "0", "1",
                               ifelse(Ictus1 == "0" & Ictus2 != "0" & Ictus3 != "0" & Ictus4 != "0", "2",
                                      ifelse(Ictus1 != "0" & Ictus2 == "0" & Ictus3 != "0" & Ictus4 != "0", "3",
                                             ifelse(Ictus1 != "0" & Ictus2 != "0" & Ictus3 == "0" & Ictus4 != "0", "4",
                                                    ifelse(Ictus1 != "0" & Ictus2 == "0" & Ictus3 == "0" & Ictus4 != "0", "5",
                                                           ifelse(Ictus1 == "0" & Ictus2 != "0" & Ictus3 == "0" & Ictus4 != "0", "6",
                                                                  ifelse(Ictus1 == "0" & Ictus2 == "0" & Ictus3 != "0" & Ictus4 != "0", "7",
                                                                         ifelse(Ictus1 == "0" & Ictus2 == "0" & Ictus3 == "0" & Ictus4 != "0", "8", NA)))))))),
           doc_id = as.integer(str_replace(doc_id, "doc", "")),
           `Rel_1&2` = ifelse(id_1 == h_id_2 & id_1 != "0" & h_id_2 != "0", r_2, "0"),
           `Rel_2&1` = ifelse(id_2 == h_id_1 & id_1 != "0" & h_id_2 != "0", r_1, "0"),
           `Rel_1&3` = ifelse(id_1 == h_id_3 & id_1 != "0" & h_id_3 != "0", r_3, "0"),
           `Rel_3&1` = ifelse(id_3 == h_id_1 & id_1 != "0" & h_id_3 != "0", r_1, "0"),
           `Rel_1&4` = ifelse(id_1 == h_id_4 & id_1 != "0" & h_id_4 != "0", r_4, "0"),
           `Rel_4&1` = ifelse(id_4 == h_id_1 & id_1 != "0" & h_id_4 != "0", r_1, "0"),
           `Rel_2&3` = ifelse(id_2 == h_id_3 & id_2 != "0" & h_id_3 != "0", r_3, "0"),
           `Rel_3&2` = ifelse(id_3 == h_id_2 & id_2 != "0" & h_id_3 != "0", r_2, "0"),
           `Rel_2&4` = ifelse(id_2 == h_id_4 & id_2 != "0" & h_id_4 != "0", r_4, "0"),
           `Rel_4&2` = ifelse(id_4 == h_id_2 & id_2 != "0" & h_id_4 != "0", r_2, "0"),
           `Rel_3&4` = ifelse(id_3 == h_id_4 & id_3 != "0" & h_id_4 != "0", r_4, "0"),
           `Rel_4&3` = ifelse(id_4 == h_id_3 & id_3 != "0" & h_id_4 != "0", r_3, "0")) %>%
    arrange(doc_id) %>%
    mutate(LineID = as.character(1:nrow(y))) %>%
    dplyr::select(-doc_id) %>%
    dplyr::rename(LineText = sentence) %>%
    left_join(data[,c("LineID","NoT","NoS","TextID")], by = "LineID")
  
  y <- y[,c(32,18,1,2,3,4,5,19,33:35,20:31)]
  
  y %>%
    write_csv(paste(args$outdir, "/", base, ".markup.csv", sep = "", collapse = ""))
}

main(args)