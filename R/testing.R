
lapply(c("dplyr", "stringr", "devtools"), library, character.only = T)
devtools::install_github("helmerbelbo/stanfordclassicr")

files = list.files(system.file("extdata", package = "StanFordClassicPackage"), full.names = T)
stmfiles = files[str_detect(files, ".stm")]
prifiles = files[str_detect(files, ".pri")]
ktrfiles = files[str_detect(files, ".ktr")]

stmlist <- read_stm_file(stmfiles)
