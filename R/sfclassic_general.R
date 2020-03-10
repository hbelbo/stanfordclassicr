#' Convert stanford classic text string to dataframe
#'
#' @param strng
#'
#' @return a data.frame, where each pair of StanForD code and corresponding
#'   variable values will form one column. I.e  have one row
#' @export
#'
#' @examples
#' stanford_classic_string = "~3 6 \n1470E~120 1 \nFURU \nGRAN \nLØV \nTØRRGRAN~"
#' sfclassic2df(stanford_classic_string)
sfclassic2df = function(strng){
  varsvals = unlist(stringr::str_split(strng, "~"))
  varnames = stringr::str_extract(varsvals, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}")
  varvals =  stringr::str_replace(string = varsvals, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}[ ]", replacement = "")
  varvals = varvals[which(!is.na(varnames))]
  varvals = stringr::str_remove(varvals, pattern = "\n")
  varnames = varnames[which(!is.na(varnames))]
  varnames = paste0("v", stringr::str_replace(string = varnames, pattern = "[ ]", replacement = "t") )
  sfcdf = data.frame(matrix(data = varvals, nrow = 1), stringsAsFactors=F)
  names(sfcdf) = varnames
  return(sfcdf)
}


sfclassic2df_v2 = function(strng){
  varsvals = unlist(stringr::str_split(strng, "~"))
  varnames = stringr::str_extract(varsvals, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}")
  varvals =  stringr::str_replace(string = varsvals, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}[ ]", replacement = "")
  varvals = varvals[which(!is.na(varnames))]
  varnames = varnames[which(!is.na(varnames))]
  varnames = paste0("v", stringr::str_replace(string = varnames, pattern = "[ ]", replacement = "t") )
  sfcdf = data.frame(matrix(data = varvals, nrow = 1), stringsAsFactors=F)
  names(sfcdf) = varnames
  return(sfcdf)
}

sfclassic2list = function(strng){
  VarStrings <- unlist(stringr::str_split( strng, pattern = "~")) # Split to individual variables and values at ~
  #VarStrings <- VarStrings[1:(length(VarStrings)-1)] #Removing the funny last tag after the last variable value
  VarVals <- stringr::str_replace(string = VarStrings, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}[ ]", replacement = "")
  VarNames <- stringr::str_extract(string = VarStrings,  pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}" )
  Vars <- paste0("v", stringr::str_replace(string = VarNames, pattern = "[ ]", replacement = "t") )

  VarDataType = dplyr::case_when(stringr::str_starts(string = VarVals, pattern = "\\n") ~ "txt", TRUE ~"Numeric")
  VarVals = stringr::str_replace(string = VarVals, pattern = "\\n", replacement = '') #remove first \n in txt variables

  txtvars = Vars[VarDataType=="txt"]
  txtvarvals = VarVals[VarDataType=="txt"]
  txtVarLength = sapply(X = stringr::str_split(string = txtvarvals, pattern = "[\\n]"), length)

  txtvarvals <- as.list(txtvarvals)
  names(txtvarvals) = txtvars
  txtvarvals <- lapply(X = txtvarvals,  FUN = function(X) {
    unlist(stringr::str_split(X, pattern = "\n"))})

  numvars <- Vars[VarDataType == "Numeric"]
  numvarsvals <- VarVals[VarDataType=="Numeric"]
  numvarsvals <- as.list(numvarsvals)
  names(numvarsvals) <- numvars
  numvarsvals <-  lapply(X = numvarsvals,  FUN = function(X) {as.integer(unlist(stringr::str_split(X, pattern = " ")))})

  vls = c(txtvarvals, numvarsvals) # A list of all variable tags and values
  return(vls)
  }

file2strng = function(filename){
  enc <- readr::guess_encoding(filename)
  enc = as.character(enc[1,1])

  strng <- readr::read_file(filename)
  if ( is.na(enc) & stringr::str_detect(string = strng, pattern = "~1 3 \nISO 8859-1")){ enc = "latin1"}
  Encoding(strng) <- enc
  strng <- stringr::str_replace(string = strng, pattern = '\"','') #Removing the funny tag at the very start of the string
  return(strng)
}

