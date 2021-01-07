
library(dplyr)
library(stringr)
library(readxl)
library(openxlsx)
library(Rutils)
library(tibble)
library(tidyr)
library(optparse)

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--infile"),
                     help = "Input file")
parser <- add_option(parser, c("-o", "--outdir"), 
                     help = "Output directory")
args <- parse_args(parser)

main <- function(args){
  data <- read_excel(args$infile)
  base <- sub(".xlsx", "", basename(args$infile))
  
  rhythmic_form_freq <- as.data.frame(t(data.frame(group_proportions(data, RhythmForm)) %>%
                                          mutate(props = props * 100)))[-1,]
  
  colnames(rhythmic_form_freq) <- str_remove(colnames(rhythmic_form_freq), "V")
  
  cols <- c("1" = NA_real_, "2" = NA_real_, "3" = NA_real_, "4" = NA_real_, "5" = NA_real_, "6" = NA_real_, "7" = NA_real_)
  
  rhythmic_form_freq <- rhythmic_form_freq %>%
    add_column(!!!cols[!names(cols) %in% names(.)]) %>%
    mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
    mutate(`Всего` = rowSums(.))
  
  row.names(rhythmic_form_freq) <- c("Кол-во", "%")
  
  rhythmic_form_freq[,c("1","2","3","4","5","6","7","Всего")]
  
  rhythmic_form_freq %>%
    write.csv(paste(args$outdir, "/", base, ".rhythmic_forms_freq.csv", sep = "", collapse = ""))
}

main(args)
