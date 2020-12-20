
library(dplyr)
library(readr)
library(tidyverse)
library(optparse)

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--infile"),
                     help = "Input file")
parser <- add_option(parser, c("-o", "--outdir"), 
                     help = "Output directory")
args <- parse_args(parser)

main <- function(args) {
  
  stress_dict <- read_delim(args$infile, ";", escape_double = FALSE, trim_ws = TRUE) %>%
    mutate(`а#а` = substr(`а#а`, start = str_locate(`а#а`, "#")[,1] + 1, stop = str_length(`а#а`))) %>%
    tidytext::unnest_tokens(word, `а#а`) %>%
    dplyr::rename(stress = word) %>%
    mutate(stress = ifelse(str_detect(stress, "'") == TRUE, stress,
                           str_c(stress, "'")),
           token = str_replace(stress, "'", "")) %>%
    distinct()
  
  stress_dict <- stress_dict[,c(2,1)] %>%
    write.csv(paste(args$outdir, "/", "stress_dict.csv", sep = "", collapse = ""))
}

main(args)