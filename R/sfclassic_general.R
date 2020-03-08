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



file2strng = function(filename){
  enc <- readr::guess_encoding(filename)
  enc = as.character(enc[1,1])

  strng <- readr::read_file(filename)
  if ( is.na(enc) & stringr::str_detect(string = strng, pattern = "~1 3 \nISO 8859-1")){ enc = "latin1"}
  Encoding(strng) <- enc
  strng <- stringr::str_replace(string = strng, pattern = '\"','') #Removing the funny tag at the very start of the string
  return(strng)
}

