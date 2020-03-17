
# stanfordclassicr

To read StanForD classic forest machine reports

Currently the package provide functions to read .pri, .stm and .ktr files. The functions have been tested for only a few example files of each category. It is therefore likely the functions will fail and that they will need further modification to work. 

## Install from github:
```r
devtools::install_github("helmerbelbo/stanfordclassicr")
```
## Demo
```r
lapply(c("magrittr","stringr","dplyr","tibble","lubridate"), library, character.only =T) 

files = list.files(system.file("extdata", package = "stanfordclassicr"), full.names = T)

stmfiles = files[stringr::str_detect(files, ".stm")] 
prifiles = files[stringr::str_detect(files, ".pri")] 
ktrfiles = files[stringr::str_detect(files, ".ktr")]

stmfiles[1] 
ktrfiles[1] 
prifiles[1]

read_stm_file(stmfiles[1]) 
read_ktr_file(ktrfiles[1]) 
read_ktr_file(ktrfiles[1]) 
ktrtest = read_ktr_file(ktrfiles[1])
```


