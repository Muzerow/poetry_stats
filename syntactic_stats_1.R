
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
  base <- sub(".xlsx", "", basename(args$infile))
  
  # количество ударных иктов
  
  ictus_n <- data.frame(ictus_num = c("Ictus1","Ictus2","Ictus3","Ictus4"),
                        non_null = c(nrow(data %>% filter(Ictus1 != "0")),
                                     nrow(data %>% filter(Ictus2 != "0")),
                                     nrow(data %>% filter(Ictus3 != "0")),
                                     nrow(data %>% filter(Ictus4 != "0")))) %>%
    mutate(props = non_null / nrow(data))
  
  ictus_full <- data.frame(ictus_num = "Total", non_null = sum(ictus_n$non_null)) %>%
    mutate(props = non_null / (nrow(data) * 4))
  
  rbind(ictus_n, ictus_full) %>%
    write.csv(paste(args$outdir, "/", base, ".ictus_n.csv", sep = "", collapse = ""))
  
  # частотность форм четырехстопного ямба
  
  rhythm_form <- data.frame(group_proportions(data, RhythmForm))
  
  rhythm_form %>%
    write.csv(paste(args$outdir, "/", base, ".rhythm_form.csv", sep = "", collapse = ""))
  
  # частотность форм четырехстопного ямба по номерам строф
  
  nos <- data.frame(group_proportions(data, NoS, RhythmForm))
  
  nos %>%
    write.csv(paste(args$outdir, "/", base, ".nos.csv", sep = "", collapse = ""))

}

main(args)