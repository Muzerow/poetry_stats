
library(dplyr)
library(udpipe)
library(readr)
library(readxl)
library(tidyverse)
library(openxlsx)
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

## Применяем модель udpipe к данным, оставляем необходимые колонки и сохраняем копию данных, чтобы потом присоединить оттуда тексты строк

udpipe_preprocess <- function(data) {
  data <- udpipe(data$LineText, object = udmodel) %>%
    filter(upos != "PUNCT") %>%
    mutate(token = str_to_lower(token)) %>%
    dplyr::select(-paragraph_id, -sentence_id, -xpos, -feats, -deps, -misc)
  
  assign("data_ud", data, envir = globalenv())
}

## Находим позиции гласных букв в строках

vowels_position <- function(data){
  for(i in 1:9){
    if (i == 1) {
      data <- data %>% mutate(v1 = regexpr('[аеёиоуыэюяaeiou]', str_to_lower(sentence)),
                              t1 = str_to_lower(substr(data$sentence, start = v1 + 1, stop = str_length(sentence))))
    } else {
      data <- data %>% mutate(!!paste0("v",i) := ifelse(!!sym(paste0("v",i-1)) %in% c(-1,0), -1,
                                                        ifelse(!!sym(paste0("t",i-1)) == "", -1,
                                                               regexpr('[аеёиоуыэюяaeiou]', !!sym(paste0("t",i-1))) + !!sym(paste0("v",i-1)))),
                              !!paste0("t",i) := ifelse(!!sym(paste0("t",i-1)) == "", "",
                                                        ifelse(str_detect(substr(!!sym(paste0("t",i-1)),
                                                                                 start = !!sym(paste0("v",i)) - !!sym(paste0("v",i-1)) + 1,
                                                                                 stop = str_length(!!sym(paste0("t",i-1)))),
                                                                          '[аеёиоуыэюяaeiou]') == FALSE, "",
                                                               substr(!!sym(paste0("t",i-1)),
                                                                      start = !!sym(paste0("v",i)) - !!sym(paste0("v",i-1)) + 1,
                                                                      stop = str_length(!!sym(paste0("t",i-1)))))))
    }
  }
  return(data)
}

## Добавляем словарь и создаем колонку с ударением

stress_dict_join <- function(data){
  data %>%
    left_join(stress_dict, by = c("token")) %>%
    mutate(position = regexpr("[`'`]", stress) + start - 2)
}

## Определяем, находятся ли слова на ударных позициях, вписываем часть речи в колонки Ictus

ictus_create <- function(data){
  for(i in 1:4){
    data <- data %>%
      mutate(!!paste0("Ictus",i) := ifelse(!!sym(paste0("v",i*2)) == position &
                                             !(upos %in% c("CCONJ","SCONJ","PART","ADP")) |
                                             !!sym(paste0("v",i*2)) == position & upos %in% c("CCONJ","SCONJ","PART","ADP") &
                                             str_count(str_to_lower(token), "[аеёиоуыэюя]") > 1, upos, NA)) 
  }
  return(data)
}

## Создаем колонку HMS

hms_create <- function(data){
  data$HMS = NA
  
  for(i in 1:4){
    data <- data %>%
      mutate(HMS = ifelse(!!sym(paste0("v",i*2-1)) == position & !(upos %in% c("CCONJ","SCONJ","PART","ADP","PRON","DET")) |
                            !!sym(paste0("v",i*2-1)) == position & upos %in% c("CCONJ","SCONJ","PART","ADP","PRON","DET") &
                            str_count(str_to_lower(token), "[аеёиоуыэюя]") > 1, i, HMS
      ))
  }
  return(data)
}

## Создаем колонки с id слова, id главного слова и связью между ними

word_rel_preparation <- function(data){
  for(i in 1:4){
    data <- data %>%
      mutate(!!paste0("id_",i) := ifelse(!is.na(!!sym(paste0("Ictus",i))), token_id, ""),
             !!paste0("h_id_",i) := ifelse(!is.na(!!sym(paste0("Ictus",i))), head_token_id, ""),
             !!paste0("r_",i) := ifelse(!is.na(!!sym(paste0("Ictus",i))), dep_rel, "")) 
  }
  return(data)
}

## Оставляем только нужные колонки, объединяем результаты предыдущих функций для каждой строки

join_results <- function(data){
  data %>%
    dplyr::select(doc_id, Ictus1, Ictus2, Ictus3, Ictus4, HMS, id_1, h_id_1, r_1, id_2, h_id_2, r_2, id_3, h_id_3, r_3, id_4, h_id_4, r_4) %>%
    group_by(doc_id) %>%
    summarise_all(funs(trimws(paste(., collapse = ''))))
}

## Чистим колонки от образовавшихся na внутри строк и заменяем пустые на NA

na_replace <- function(data){
  data %>%
    mutate_all(funs(str_replace_all(., "NA", ""))) %>%
    mutate_all(funs(ifelse(. == "", "0", .)))
}

## Присоединяем сами строки по doc_id

text_join <- function(data){
  data %>% left_join((data_ud %>%
                        select(doc_id, sentence) %>% 
                        distinct() %>%
                        group_by(doc_id) %>%
                        summarise(sentence = paste(sentence, collapse = ' '))),
                     by = c("doc_id"))
}

## Создаем колонку с ритмическими формами

rhythm_form_create <- function(data){
  data %>% mutate(RhythmForm = ifelse(Ictus1 != "0" & Ictus2 != "0" & Ictus3 != "0" & Ictus4 != "0", "1",
                                      ifelse(Ictus1 == "0" & Ictus2 != "0" & Ictus3 != "0" & Ictus4 != "0", "2",
                                             ifelse(Ictus1 != "0" & Ictus2 == "0" & Ictus3 != "0" & Ictus4 != "0", "3",
                                                    ifelse(Ictus1 != "0" & Ictus2 != "0" & Ictus3 == "0" & Ictus4 != "0", "4",
                                                           ifelse(Ictus1 != "0" & Ictus2 == "0" & Ictus3 == "0" & Ictus4 != "0", "5",
                                                                  ifelse(Ictus1 == "0" & Ictus2 != "0" & Ictus3 == "0" & Ictus4 != "0", "6",
                                                                         ifelse(Ictus1 == "0" & Ictus2 == "0" & Ictus3 != "0" & Ictus4 != "0", "7",
                                                                                ifelse(Ictus1 == "0" & Ictus2 == "0" & Ictus3 == "0" & Ictus4 != "0", "8", NA)))))))))
}

## Создаем колонки со взаимосвязями между словами на ударных позициях

relations_create <- function(data){
  for(i in list(c(1,2),c(2,1),c(1,3),c(3,1),c(1,4),c(4,1),c(2,3),c(3,2),c(2,4),c(4,2),c(3,4),c(4,3))){
    a <- i[1]
    b <- i[2]
    
    data <- data %>%
      mutate(!!paste0("Rel_",a,"&",b) := ifelse(!!sym(paste0("id_",a)) == !!sym(paste0("h_id_",b)) &
                                                  !!sym(paste0("id_",a)) != "0" &
                                                  !!sym(paste0("h_id_",b)) != "0",
                                                !!sym(paste0("r_",b)),
                                                "0"))
  }
  return(data)
}

## Финальная чистка и добавление исходных колонок NoT, NoS и TextID

final_cleaning <- function(data){
  data <- data %>%
    dplyr::mutate(doc_id = as.integer(str_replace(doc_id, "doc", "")),
                  Notes = NA) %>%
    dplyr::rename(LineID = doc_id,
                  LineText = sentence) %>%
    arrange(LineID) %>%
    left_join(mutate(data_control[,c("LineID","NoT","NoS","TextID")], LineID = as.integer(LineID)), by = "LineID")
  
  return(data[,c(1,19,2:5,6,20,34:36,21:32,33)])
}

# Загрузка данных

data <- read_excel(args$infile)
data_control <- data
base <- sub(".xlsx", "", basename(args$infile))

stress_dict <- read.csv(args$dictionary)
udmodel <- udpipe_load_model(file = args$model)

# Применение функций

data %>%
  udpipe_preprocess() %>%
  vowels_position() %>%
  stress_dict_join() %>% 
  ictus_create() %>%
  hms_create() %>%
  word_rel_preparation() %>%
  join_results() %>%
  na_replace() %>%
  text_join() %>%
  rhythm_form_create() %>%
  relations_create() %>%
  final_cleaning() %>%
  write.csv(paste(args$outdir, "/", base, ".markup.csv", sep = "", collapse = ""))


