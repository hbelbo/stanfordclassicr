

# sfvardefs = readRDS(
# file = list.files(system.file("extdata", package = "stanfordclassicr"),
# full.names = T, pattern = "sfvardefs.Rds"))


#' Convert stanford classic text string to dataframe
#'
#' @param strng
#'
#' @return a data.frame, where each pair of StanForD code and corresponding
#'   variable values will form one column. I.e  have one row
#' @export
#'
#' @examples
#' stanford_classic_string <- "~3 6 \n1470E~120 1 \nFURU \nGRAN \nLØV \nTØRRGRAN~"
#' sfclassic2df(stanford_classic_string)
sfclassic2df <- function(strng){
  varsvals = unlist(stringr::str_split(strng, "~"))
  varsvals <- varsvals[stringr::str_length(varsvals)>1] # to drop empty returns
  varnames = stringr::str_extract(varsvals, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}")
  varvals =  stringr::str_replace(string = varsvals, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}[ ]", replacement = "")
  varvals = varvals[which(!is.na(varnames))]
  varvals = stringr::str_remove(varvals, pattern = "\n")
  varnames = varnames[which(!is.na(varnames))]

  varnames = paste0("v", stringr::str_replace(string = varnames, pattern = "[ ]", replacement = "t") )
  varnames = make.names(varnames, unique = T)
  sfcdf = data.frame(matrix(data = varvals, nrow = 1), stringsAsFactors=F)
  names(sfcdf) = varnames
  return(sfcdf)
}


sfclassic2df_v2 <- function(strng){
  varsvals = unlist(stringr::str_split(strng, "~"))
  varsvals <- varsvals[stringr::str_length(varsvals)>1] # to drop empty returns
  varnames = stringr::str_extract(varsvals, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}")
  varvals =  stringr::str_replace(string = varsvals, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}[ ]", replacement = "")
  varvals = varvals[which(!is.na(varnames))]
  varnames = varnames[which(!is.na(varnames))]
  varnames = paste0("v", stringr::str_replace(string = varnames, pattern = "[ ]", replacement = "t") )

  varvals <- varvals[which(varnames %in% sfvardefs$sfv)]
  varnames <- varnames[which(varnames %in% sfvardefs$sfv)]

  sfcdf = data.frame(matrix(data = varvals, nrow = 1), stringsAsFactors=F)
  sfcdf = tibble::as_tibble(sfcdf)
  varnames = make.names(varnames, unique = T)
  names(sfcdf) = varnames

  #sfcdf2 = sfcdf %>% select_if(.,  !is.na(.))
  return(sfcdf)
}

sfclassic2list <- function(strng){
  VarStrings <- unlist(stringr::str_split( strng, pattern = "~")) # Split to individual variables and values at ~
  VarStrings <- VarStrings[stringr::str_length(VarStrings)>1] # to drop empty returns
  VarVals <- stringr::str_replace(string = VarStrings, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}[ ]", replacement = "")
  VarNames <- stringr::str_extract(string = VarStrings,  pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}" )
  Vars <- paste0("v", stringr::str_replace(string = VarNames, pattern = "[ ]", replacement = "t") )

  VarDataType <- dplyr::case_when(stringr::str_starts(string = VarVals, pattern = "\\n") ~ "txt", TRUE ~"Numeric")

  txtvars <- Vars[VarDataType=="txt"]
  txtvarvals <- VarVals[VarDataType=="txt"]

  txtvarvals <- as.list(txtvarvals)
  names(txtvarvals) <- txtvars


  numvars <- Vars[VarDataType == "Numeric"]
  numvarsvals <- VarVals[VarDataType=="Numeric"]
  numvarsvals <- as.list(numvarsvals)
  names(numvarsvals) <- numvars
  numvarsvals <-  lapply(X = numvarsvals,  FUN = function(X) {as.integer(unlist(stringr::str_split(X, pattern = " ")))})

  vls <- c(txtvarvals, numvarsvals) # A list of all variable tags and values
  return(vls)
  }

file2strng <- function(filename){
  enc <- readr::guess_encoding(filename)
  enc <- as.character(enc[1,1])

  strng <- readr::read_file(filename)
  if ( is.na(enc) & stringr::str_detect(string = strng, pattern = "~1 3 \nISO 8859-1")){ enc = "latin1"}
  Encoding(strng) <- enc
  if(stringr::str_detect(stringr::str_sub(strng, start = 1, end = 4), pattern = '\"')){
    strng <- stringr::str_replace(string = strng, pattern = '\"','') #Removing the funny tag at the very start of the string
  }

  return(strng)
}



populateselection <- function(valuelist, selector){
  selected = valuelist[selector]
  isnotnull = sapply(selected, function(x) {length(x)>0})
  subselected = selected[isnotnull]
  notselected = selector[!(selector %in% names(subselected))]
  dataselected = as_tibble(subselected)
  for (i in seq_along(notselected)){
    dataselected = dataselected %>% mutate(., !!notselected[i] := NA_character_)
  }
  return(dataselected)
}



expand_str <- function(tibbl){

  var1 = dplyr::pull(tibbl[,1])
  # Clue1: text variables starts with new line (\n or \cr \r\n)
  type1 = ifelse(stringr::str_starts(string = var1[1], pattern = "[\n\r]{1,2}"), "txt", "num")

  # Clue2: if text, each data entry is split by newline. If numeric, each data entry split by space.
  if(type1 == "txt") {
    lexp1 =
      unlist(stringr::str_split(stringr::str_remove(var1, "[\n\r]{1,2}"), "[\n\r]{1,2}" ))
  } else {
    lexp1 =
      as.integer(unlist(stringr::str_split(var1, " ")))
  }
  retdf = tibble::tibble(.rows = length(lexp1))


  for (i in seq_along(names(tibbl))){
    nami = names(tibbl)[i]
    vari = dplyr::pull(tibbl[,i])
    # Clue1: text variables starts with new line (\n or \cr \r\n)

    typei = ifelse(stringr::str_starts(string = vari[1], pattern = "[\n\r]{1,2}"), "txt", "num")
    n_obsi = length(vari)

    # Clue2: if text, each data entry is split by newline. If numeric, each data entry split by space.
    if(typei == "txt") {
      lexp =
        unlist(stringr::str_split(stringr::str_remove(vari, "[\n\r]{1,2}"), "[\n\r]{1,2}" ))
    } else {
      lexp =
        as.integer(unlist(stringr::str_split(vari, " ")))
    }
    retdf = mutate(retdf, !!nami := lexp)
  }
  return(retdf)
}



varvals2one <- function(stanford.tibbl, vars2use){
  selector <- vars2use # e.g. c(  "v21t2", "v21t3", "v21t4") #list of sfclassic vars we want
  selector <- selector[which(selector %in% names(stanford.tibbl))] # Ensure to not select vars not present
  selected <- df1 %>% dplyr::select(., tidyselect::all_of(selector)) %>%
    expand_str(.) %>%
    dplyr::select_if(., ~nchar(.)>0)

  if(ncol(selected)>0){
    collapsed2one <- selected %>%
      stringr::str_c(., sep = ", ", collapse = ", ")
  } else {
    collapsed2one <- character()
  }
  return(collapsed2one)
}

