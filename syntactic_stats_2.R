
library(dplyr)
library(readxl)
library(Rutils)
library(tibble)
library(openxlsx)
library(optparse)

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--infile"),
                     help = "Input file")
parser <- add_option(parser, c("-o", "--outdir"), 
                     help = "Output directory")
args <- parse_args(parser)

main <- function(args) {
  data <- read_excel(args$infile)
  
  # частотность частей речи на каждом икте
  
  dfi <- dplyr::select(data, Ictus1, Ictus2, Ictus3, Ictus4)
  ictus_pos <- data.frame(Ictus = integer(), part_of_speech = character(), n = integer(), props = double())
  
  for(i in 1:4){
    ictus_pos <- rbind(ictus_pos, data.frame(Ictus = i, group_proportions(dfi, dfi[,i]) %>%
                                               rename(part_of_speech = 1)))
  }
  
  remove(dfi)
  
  ictus_total <- ictus_pos[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    group_by(part_of_speech) %>%
    summarise(total = sum(n))
  
  ictus_pos <- ictus_pos[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    full_join(ictus_total, by = c("part_of_speech")) %>%
    mutate(props = n / total)
  
  ictus_pos <- ictus_pos[,c(1,2,3,5,4)] %>% filter(part_of_speech != "0")
  
  ictus_pos %>%
    write.csv(paste(args$outdir, "/", "ictus_pos.csv", sep = "", collapse = ""))
  
  # частотность частей речи по ритмическим формам (проценты по всем ритмическим формам)
  
  ## первая ритмическая форма
  
  d1 <- data %>% filter(RhythmForm == 1) %>% dplyr::select(Ictus1, Ictus2, Ictus3, Ictus4)
  rform_pos1 <- data.frame(Ictus = integer(), part_of_speech = character(), n = integer(), props = double())
  
  for(i in 1:4){
    rform_pos1 <- rbind(rform_pos1, data.frame(Ictus = i, group_proportions(d1, d1[,i]) %>%
                                                 rename(part_of_speech = 1)))
  }
  
  remove(d1)
  
  rform_pos1 <- rform_pos1[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    left_join(ictus_total, by = c("part_of_speech")) %>%
    mutate(props = n / total)
  
  ## вторая ритмическая форма
  
  d2 <- data %>% filter(RhythmForm == 2) %>% dplyr::select(Ictus1, Ictus2, Ictus3, Ictus4)
  rform_pos2 <- data.frame(Ictus = integer(), part_of_speech = character(), n = integer(), props = double())
  
  for(i in 1:4){
    rform_pos2 <- rbind(rform_pos2, data.frame(Ictus = i, group_proportions(d2, d2[,i]) %>%
                                                 rename(part_of_speech = 1)))
  }
  
  remove(d2)
  
  rform_pos2 <- rform_pos2[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    left_join(ictus_total, by = c("part_of_speech")) %>%
    mutate(props = n / total)
  
  ## третья ритмическая форма
  
  d3 <- data %>% filter(RhythmForm == 3) %>% dplyr::select(Ictus1, Ictus2, Ictus3, Ictus4)
  rform_pos3 <- data.frame(Ictus = integer(), part_of_speech = character(), n = integer(), props = double())
  
  for(i in 1:4){
    rform_pos3 <- rbind(rform_pos3, data.frame(Ictus = i, group_proportions(d3, d3[,i]) %>%
                                                 rename(part_of_speech = 1)))
  }
  
  remove(d3)
  
  rform_pos3 <- rform_pos3[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    left_join(ictus_total, by = c("part_of_speech")) %>%
    mutate(props = n / total)
  
  ## четвертая ритмическая форма
  
  d4 <- data %>% filter(RhythmForm == 4) %>% dplyr::select(Ictus1, Ictus2, Ictus3, Ictus4)
  rform_pos4 <- data.frame(Ictus = integer(), part_of_speech = character(), n = integer(), props = double())
  
  for(i in 1:4){
    rform_pos4 <- rbind(rform_pos4, data.frame(Ictus = i, group_proportions(d4, d4[,i]) %>%
                                                 rename(part_of_speech = 1)))
  }
  
  remove(d4)
  
  rform_pos4 <- rform_pos4[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    left_join(ictus_total, by = c("part_of_speech")) %>%
    mutate(props = n / total)
  
  ## пятая ритмическая форма
  
  d5 <- data %>% filter(RhythmForm == 5) %>% dplyr::select(Ictus1, Ictus2, Ictus3, Ictus4)
  rform_pos5 <- data.frame(Ictus = integer(), part_of_speech = character(), n = integer(), props = double())
  
  for(i in 1:4){
    rform_pos5 <- rbind(rform_pos5, data.frame(Ictus = i, group_proportions(d5, d5[,i]) %>%
                                                 rename(part_of_speech = 1)))
  }
  
  remove(d5)
  
  rform_pos5 <- rform_pos5[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    left_join(ictus_total, by = c("part_of_speech")) %>%
    mutate(props = n / total)
  
  ## шестая ритмическая форма
  
  d6 <- data %>% filter(RhythmForm == 6) %>% dplyr::select(Ictus1, Ictus2, Ictus3, Ictus4)
  rform_pos6 <- data.frame(Ictus = integer(), part_of_speech = character(), n = integer(), props = double())
  
  for(i in 1:4){
    rform_pos6 <- rbind(rform_pos6, data.frame(Ictus = i, group_proportions(d6, d6[,i]) %>%
                                                 rename(part_of_speech = 1)))
  }
  
  remove(d6)
  
  rform_pos6 <- rform_pos6[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    left_join(ictus_total, by = c("part_of_speech")) %>%
    mutate(props = n / total)
  
  rform_pos1 <- data.frame(RhythmForm = 1, rform_pos1)
  rform_pos2 <- data.frame(RhythmForm = 2, rform_pos2)
  rform_pos3 <- data.frame(RhythmForm = 3, rform_pos3)
  rform_pos4 <- data.frame(RhythmForm = 4, rform_pos4)
  rform_pos5 <- data.frame(RhythmForm = 5, rform_pos5)
  rform_pos6 <- data.frame(RhythmForm = 6, rform_pos6)
  
  rform_pos <- rbind(rform_pos1, rform_pos2, rform_pos3, rform_pos4, rform_pos5, rform_pos6) %>%
    filter(part_of_speech != "0")
  
  rform_pos %>%
    write.csv(paste(args$outdir, "/", "rform_pos_all_rf.csv", sep = "", collapse = ""))
  
  # частотность частей речи по ритмическим формам (проценты внутри ритмических форм)
  
  ## первая ритмическая форма
  
  d1 <- data %>% filter(RhythmForm == 1) %>% dplyr::select(Ictus1, Ictus2, Ictus3, Ictus4)
  rform_pos1 <- data.frame(Ictus = integer(), part_of_speech = character(), n = integer(), props = double())
  
  for(i in 1:4){
    rform_pos1 <- rbind(rform_pos1, data.frame(Ictus = i, group_proportions(d1, d1[,i]) %>%
                                                 rename(part_of_speech = 1)))
  }
  
  remove(d1)
  
  rform_total1 <- rform_pos1[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    group_by(part_of_speech) %>%
    summarise(total = sum(n))
  
  rform_pos1 <- rform_pos1[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    full_join(rform_total1, by = c("part_of_speech")) %>%
    mutate(props = n / total)
  
  ## вторая ритмическая форма
  
  d2 <- data %>% filter(RhythmForm == 2) %>% dplyr::select(Ictus1, Ictus2, Ictus3, Ictus4)
  rform_pos2 <- data.frame(Ictus = integer(), part_of_speech = character(), n = integer(), props = double())
  
  for(i in 1:4){
    rform_pos2 <- rbind(rform_pos2, data.frame(Ictus = i, group_proportions(d2, d2[,i]) %>%
                                                 rename(part_of_speech = 1)))
  }
  
  remove(d2)
  
  rform_total2 <- rform_pos2[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    group_by(part_of_speech) %>%
    summarise(total = sum(n))
  
  rform_pos2 <- rform_pos2[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    full_join(rform_total2, by = c("part_of_speech")) %>%
    mutate(props = n / total)
  
  ## третья ритмическая форма
  
  d3 <- data %>% filter(RhythmForm == 3) %>% dplyr::select(Ictus1, Ictus2, Ictus3, Ictus4)
  rform_pos3 <- data.frame(Ictus = integer(), part_of_speech = character(), n = integer(), props = double())
  
  for(i in 1:4){
    rform_pos3 <- rbind(rform_pos3, data.frame(Ictus = i, group_proportions(d3, d3[,i]) %>%
                                                 rename(part_of_speech = 1)))
  }
  
  remove(d3)
  
  rform_total3 <- rform_pos3[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    group_by(part_of_speech) %>%
    summarise(total = sum(n))
  
  rform_pos3 <- rform_pos3[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    full_join(rform_total3, by = c("part_of_speech")) %>%
    mutate(props = n / total)
  
  ## четвертая ритмическая форма
  
  d4 <- data %>% filter(RhythmForm == 4) %>% dplyr::select(Ictus1, Ictus2, Ictus3, Ictus4)
  rform_pos4 <- data.frame(Ictus = integer(), part_of_speech = character(), n = integer(), props = double())
  
  for(i in 1:4){
    rform_pos4 <- rbind(rform_pos4, data.frame(Ictus = i, group_proportions(d4, d4[,i]) %>%
                                                 rename(part_of_speech = 1)))
  }
  
  remove(d4)
  
  rform_total4 <- rform_pos4[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    group_by(part_of_speech) %>%
    summarise(total = sum(n))
  
  rform_pos4 <- rform_pos4[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    full_join(rform_total4, by = c("part_of_speech")) %>%
    mutate(props = n / total)
  
  ## пятая ритмическая форма
  
  d5 <- data %>% filter(RhythmForm == 5) %>% dplyr::select(Ictus1, Ictus2, Ictus3, Ictus4)
  rform_pos5 <- data.frame(Ictus = integer(), part_of_speech = character(), n = integer(), props = double())
  
  for(i in 1:4){
    rform_pos5 <- rbind(rform_pos5, data.frame(Ictus = i, group_proportions(d5, d5[,i]) %>%
                                                 rename(part_of_speech = 1)))
  }
  
  remove(d5)
  
  rform_total5 <- rform_pos5[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    group_by(part_of_speech) %>%
    summarise(total = sum(n))
  
  rform_pos5 <- rform_pos5[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    full_join(rform_total5, by = c("part_of_speech")) %>%
    mutate(props = n / total)
  
  ## шестая ритмическая форма
  
  d6 <- data %>% filter(RhythmForm == 6) %>% dplyr::select(Ictus1, Ictus2, Ictus3, Ictus4)
  rform_pos6 <- data.frame(Ictus = integer(), part_of_speech = character(), n = integer(), props = double())
  
  for(i in 1:4){
    rform_pos6 <- rbind(rform_pos6, data.frame(Ictus = i, group_proportions(d6, d6[,i]) %>%
                                                 rename(part_of_speech = 1)))
  }
  
  remove(d6)
  
  rform_total6 <- rform_pos6[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    group_by(part_of_speech) %>%
    summarise(total = sum(n))
  
  rform_pos6 <- rform_pos6[,c(2,1,3,4)] %>%
    arrange(part_of_speech) %>%
    full_join(rform_total6, by = c("part_of_speech")) %>%
    mutate(props = n / total)
  
  rform_pos1 <- data.frame(RhythmForm = 1, rform_pos1)
  rform_pos2 <- data.frame(RhythmForm = 2, rform_pos2)
  rform_pos3 <- data.frame(RhythmForm = 3, rform_pos3)
  rform_pos4 <- data.frame(RhythmForm = 4, rform_pos4)
  rform_pos5 <- data.frame(RhythmForm = 5, rform_pos5)
  rform_pos6 <- data.frame(RhythmForm = 6, rform_pos6)
  
  rform_pos_v2 <- rbind(rform_pos1, rform_pos2, rform_pos3, rform_pos4, rform_pos5, rform_pos6) %>%
    filter(part_of_speech != "0")
  
  rform_pos_v2 %>%
    write.csv(paste(args$outdir, "/", "rform_pos_within_rf.csv", sep = "", collapse = ""))
  
  # частотность частей речи
  
  pos_freq <- data.frame(data %>%
                           count(Ictus1) %>%
                           rename(part_of_speech = Ictus1) %>%
                           full_join(data %>%
                                       count(Ictus2) %>%
                                       rename(part_of_speech = Ictus2), by = "part_of_speech") %>%
                           full_join(data %>%
                                       count(Ictus3) %>%
                                       rename(part_of_speech = Ictus3), by = "part_of_speech") %>%
                           full_join(data %>%
                                       count(Ictus4) %>%
                                       rename(part_of_speech = Ictus4), by = "part_of_speech") %>%
                           replace(is.na(.), 0) %>%
                           filter(part_of_speech != "0") %>%
                           mutate(n = n.x + n.y + n.x.x + n.y.y, props = n / sum(n)) %>%
                           select(part_of_speech, n, props) %>%
                           arrange(-n))
  
  pos_freq %>%
    write.csv(paste(args$outdir, "/", "pos_freq.csv", sep = "", collapse = ""))
  
}

main(args)