

# sfvardefs = readRDS(
# file = list.files(system.file("extdata", package = "stanfordclassicr"),
# full.names = TRUE, pattern = "sfvardefs.Rds"))


#' Split stanford classic text string to dataframe
#'
#' @param strng is a "stanfor classic file text string", i.e. the content of any
#' StanForD classic machine report.
#' The content is a long string which carries all the data.
#' Stanford classic variables are separated by a "~",
#' followed by two numbers defining
#' the variable code and type, then followed by the variable values.
#'
#' @return a data.frame, where each pair of StanForD code and corresponding
#'   variable values will form one column. I.e  have one row
#' @export
#'
#' @examples
#' stanford_classic_string <-
#' "1 2 \nSTM~120 1 \nFURU \nGRAN~16 4 \n20160208064316~116 1 7 7 2 2"
#' sfclassic2df(stanford_classic_string)
sfclassic2df <- function(strng){
  varsvals <- unlist(stringr::str_split(strng, "~"))
  varsvals <- varsvals[stringr::str_length(varsvals)>1] # to drop empty returns
  varnames <- stringr::str_extract(varsvals, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}")
  varvals <-  stringr::str_replace(string = varsvals, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}[ ]", replacement = "")
  varvals <- varvals[which(!is.na(varnames))]
  varvals <- stringr::str_remove(varvals, pattern = "\n")
  varnames <- varnames[which(!is.na(varnames))]

  varnames <- paste0("v", stringr::str_replace(string = varnames, pattern = "[ ]", replacement = "t") )
  varnames <- make.names(varnames, unique = T)
  sfcdf <- data.frame(matrix(data = varvals, nrow = 1), stringsAsFactors=F)
  names(sfcdf) <- varnames
  return(sfcdf)
}


#' Split stanford classic text string to a tibble
#'
#' @param strng is a "stanfor classic file text string", i.e. the text string
#' being the content of any StanForD classic machine report.
#' The content is a long string which carries all the data.
#' Stanford classic variables are separated by a "~", followed by two numbers defining
#' the variable code and type, then followed by the variable values.
#' @param sfvardefs is a tibble providing all stanford classic variable
#' definitions found in the documentation.
#' Columns:
#' sfv is stanford variable number and type, e.g. "v1t1", and
#' sfvc is variable category (character, integer, code)
#'
#' @return a data.frame, where each pair of StanForD code and corresponding
#'   variable values will form one column. I.e  have one row
#' @export
#'
#' @examples
#' stanford_classic_string <-
#' "1 2 \nSTM~120 1 \nFURU \nGRAN~16 4 \n20160208064316~116 1 7 7 2 2"
#' sfclassic2df_v2(stanford_classic_string)
sfclassic2df_v2 <- function(strng, sfvardefs = stanfordclassicr::sfvardefs ){
  varsvals <- unlist(stringr::str_split(strng, "~"))
  varsvals <- varsvals[stringr::str_length(varsvals)>1] # to drop empty returns
  varnames <- stringr::str_extract(varsvals, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}")
  varvals <-  stringr::str_replace(string = varsvals, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}[ ]", replacement = "")
  varvals <- varvals[which(!is.na(varnames))]
  varnames <- varnames[which(!is.na(varnames))]
  varnames <- paste0("v", stringr::str_replace(string = varnames, pattern = "[ ]", replacement = "t") )

  varvals <- varvals[which(varnames %in% sfvardefs$sfv)]
  varnames <- varnames[which(varnames %in% sfvardefs$sfv)]

  sfcdf = data.frame(matrix(data = varvals, nrow = 1), stringsAsFactors=F)
  sfcdf = tibble::as_tibble(sfcdf)
  varnames <- make.names(varnames, unique = T)
  names(sfcdf) <- varnames
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
  numvarsvals <-  lapply(X = numvarsvals,
                         FUN = function(X) {
                           as.integer(unlist(stringr::str_split(
                             X, pattern = " ")))
                           })

  vls <- c(txtvarvals, numvarsvals) # A list of all variable tags and values
  return(vls)
  }

#' Read the content of a stanford classic text file and return the content as
#' a text string
#'
#' @param filename Is the name of the stanford classic text file to read
#'
#' @return a string
#' @export
#'
#' @examples
#' tmp <- tempfile()
#' x <- "stanford classic is fun"
#' readr::write_file(x, tmp)
#' file2strng(tmp)
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
  dataselected = tibble::as_tibble(subselected)
  for (i in seq_along(notselected)){
    dataselected = dataselected %>% dplyr::mutate(!!notselected[i] := NA_character_)
  }
  return(dataselected)
}



#' Expand stanford classic variable strings
#'
#' This function takes a tibble having stanford classic variable values,
#' and expands these to single observations in a tibble (ala data frame)
#' All stanford variables in the input tibble should preferably
#' have equal number of observations.
#' @param tibbl a tibble having one row and any number of columns.
#' Each single 'tibbl[1,j]' is a string containing zero, one, or multiple variable
#' values, and where text variables are preceded by a newline
#' and where numeric variables are separated by a space
#'
#' @return a tibble, equal number of variables as input tibble.
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' stanford_classic_variable_strings <-
#' tibble::tibble(speciescode = "1 2 3", speciesname = "\nspruce\npine\nbirch")
#' expand_stcvs (stanford_classic_variable_strings)
expand_stcvs <- function(tibbl){

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

    typei <- ifelse(stringr::str_starts(string = vari[1], pattern = "[\n\r]{1,2}"), "txt", "num")
    n_obsi = length(vari)


    # Clue2: if text, each data entry is split by newline. If numeric, each data entry split by space.
    if(typei == "txt") {
      lexp =
        unlist(stringr::str_split(stringr::str_remove(vari, "[\n\r]{1,2}"), "[\n\r]{1,2}" ))
    } else {
      vari = stringr::str_remove(vari, "^[ ]+")
      lexp =
        as.double(unlist(stringr::str_split(vari, "[ ]+")))

    }
    n <- length(lexp)
    if( length(lexp)== nrow(retdf)){
      retdf = dplyr::mutate(retdf, !!nami := lexp)
    }

  }
  return(retdf)
}



varvals2one <- function(stanford.tibbl, vars2use){
  selector <- vars2use # e.g. c(  "v21t2", "v21t3", "v21t4") #list of sfclassic vars we want
  selector <- selector[which(selector %in% names(stanford.tibbl))] # Ensure to not select vars not present
  selected <- stanford.tibbl %>% dplyr::select( tidyselect::all_of(selector))
  selected <- expand_stcvs(selected) # To remove "\n" from all string variebles
  # selected <- dplyr::select_if(selected, ~nchar(.data)>0) this doesnt work

  if(ncol(selected)>0){
    collapsed2one <-
      stringr::str_c(as.character(selected[1,]), sep = ", ", collapse = ", ")
  } else {
    collapsed2one <- character()
  }
  return(collapsed2one)
}

