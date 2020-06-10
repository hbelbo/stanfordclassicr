#' Read StanForD Classic PRI-files (mahcine reports from forest machines)
#'
#' @param filename
#'
#' @return should return a list of tables populated with data from the stm report: report_header, object_definition, operator_definition, product_definitions, stems, logs
#' @export
#'
#' @examples
#'  files = list.files(system.file("extdata", package = "stanfordclassicr"), full.names = T)
#'  prifiles = files[stringr::str_detect(files, ".pri")]
#'  read_pri_file(prifiles[1])
read_pri_file = function(filename){

  enc <- readr::guess_encoding(filename)
  enc = as.character(enc[1,1])

  strng <- readr::read_file(filename)
  Encoding(strng) <- enc
  strng <- str_replace(string = strng, pattern = '\"','') #Removing the funny tag at the very start of the string
  VarStrings <- unlist(str_split(strng, pattern = "~")) # Split to individual variables and values at ~
  VarStrings <- VarStrings[1:(length(VarStrings)-1)] #Removing the funny last tag after the last variable value
  VarVals <- str_replace(string = VarStrings, pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}[ ]", replacement = "")
  VarNames <- str_extract(string = VarStrings,  pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}" )
  Vars <- paste0("v",str_replace(string = VarNames, pattern = "[ ]", replacement = "t") )
  VarNr <- as.integer(str_extract(string = VarNames, pattern = "[[:digit:]]{1,4}"))
  VarType <- as.integer(str_replace(string = VarNames, pattern = "[[:digit:]]{1,4}[ ]", replacement = ""))
  VarDataType = case_when(str_starts(string = VarVals, pattern = "\\n") ~ "txt", TRUE ~"Numeric")
  VarVals = str_replace(string = VarVals, pattern = "\\n", replacement = '') #remove first \n in txt variables
  VarValsNchar= nchar(VarVals)

  VarTeaser = str_sub(VarVals, start = 1, end = 25)
  #sfc = tibble(V=Vars, v=VarNames, VarNr, VarType, VarDataType, strn = VarTeaser)

  #https://stats.stackexchange.com/questions/10838/produce-a-list-of-variable-name-in-a-for-loop-then-assign-values-to-them

  txtvars = Vars[VarDataType=="txt"]
  txtvarvals = VarVals[VarDataType=="txt"]
  txtVarLength = sapply(X = str_split(string = txtvarvals, pattern = "[\\n]"), length)

  txtvarvals <- as.list(txtvarvals)
  names(txtvarvals) = txtvars
  txtvarvals <- lapply(X = txtvarvals,  FUN = function(X) {str_split(X, pattern = "\n")})
  tmp <- str_sub(VarVals[VarDataType=="txt"], start = 1, end = 25)
  for(i in 1:length(txtvarvals)){
    assign(txtvars[i], as.vector(txtvarvals[[i]]))
    }
  sftxt = tibble(V=Vars[VarDataType=="txt"], v=VarNames[VarDataType=="txt"], VarNr = VarNr[VarDataType=="txt"], VarType = VarType[VarDataType=="txt"],
                 VarDType = VarDataType[VarDataType=="txt"], VarLength = txtVarLength, strn = tmp)

  numvars <- Vars[VarDataType == "Numeric"]
  numvarsvals <- VarVals[VarDataType=="Numeric"]
  numvarsvals <- as.list(numvarsvals)
  names(numvarsvals) <- numvars
  numvarsvals <-  lapply(X = numvarsvals,  FUN = function(X) {as.numeric(unlist(str_split(X, pattern = " ")))})
  #numVarLength = sapply(X = str_split(string = numvarvals, pattern = " "), length)
  numVarLength = sapply(X = numvarsvals, length)
  tmp <- str_sub(VarVals[VarDataType=="Numeric"], start = 1, end = 25)
  for(i in 1:length(numvarsvals)){
    assign(numvars[i], as.numeric(as.vector(numvarsvals[[i]])))

  }
  sfnum = tibble(V=Vars[VarDataType=="Numeric"], v=VarNames[VarDataType=="Numeric"], VarNr = VarNr[VarDataType=="Numeric"], VarType = VarType[VarDataType=="Numeric"],
                 VarDType = VarDataType[VarDataType=="Numeric"], VarLength = numVarLength, strn = tmp)

  sfdt = bind_rows(sftxt, sfnum) %>% arrange(., VarNr, VarType)


  ReportHeader = tibble(CreationDate = v12t4, CountryCode = v6t1, BaseMachineNumber = v3t1,
                        BaseMachineId = v3t2,  BaseMachineManufacturer = v3t5, BaseMachineModel = v3t6, HarvesterHeadModel = v3t8)

  ObjectDefinition = tibble(ObjectName = v21t1 ,
                            ObjectUserId = v21t1,
                            ObjectStartDate = ymd_hms(v16t4),
                            ObjectKey = as.integer(ObjectStartDate),
                            SubObjectName = v21t2,
                            SubObjectUserId = paste0(v21t2, v21t3, v21t4),
                            SubObjectKey = 1)

  # Species and Product definitions
  SpeciesGroupDefinition =
    tibble(SpeciesGroupName = v120t1,
               SpeciesGroupUserId = paste0(v120t1, "#", v120t3, "#", v2t1),
               tmp_species_nr = 1:v111t1,
               SpeciesGroupkey = (as.numeric(v13t4)+ tmp_species_nr))


  # Help-table of product groups
  product_grp_species_nr = rep(1:length(v125t1), v125t1)
  product_grp_code = integer()
  for (i in 1:v111t1) {
    product_grp_code = c(product_grp_code, 1:v125t1[i])
  }


  product_grp_table =
    tibble(product_grp_code,
               product_grp_species_nr,
               ProductGroupName = v127t1)


  v121t6 = ifelse(exists("v121t6"), v121t6, NA)

  ProductDefinition =
    tibble(
      tmp_species_nr = rep(1:v111t1, v116t1),
      tmp_product_number = as.integer(1:length(v121t1)),
      ProductKey = case_when(
        !is.na(v121t6) ~ as.integer(v121t6),
        TRUE ~ as.integer(ymd_hms(v13t4)) + tmp_product_number),
      ProductName = v121t1,
      ProductInfo = v121t2,
      v126t1 = v126t1,
      SpeciesGroupkey =  as.integer(ymd_hms(v13t4)) + tmp_species_nr,
      tmp_species_name = rep(v120t1, v116t1))


   ProductDefinition =
    left_join(ProductDefinition,
              product_grp_table,
              by = c("tmp_species_nr" = "product_grp_species_nr",
                     "v126t1" = "product_grp_code"))






  # .. harvested trees
  CodeCodes <- c( 2,               500,            505,                 723,        724,        740,        741,        750,              760,         761,           762,           763,              764,           2001)
  CodeNames <- c("species", "stemnumber", "AdaptedForBioExt", "RefDiaForDbh", "RefDiaHeigth", "dbhmm", "StemType", "OperatorNumber", "Latitude", "Lat1North2South", "Longitude", "Lon2West1East", "Altitude","Userdefined1")
  Code_Name_pairs <- data.frame(Code=CodeCodes, CodeN= CodeNames)
  Matches <- match(v266t1, Code_Name_pairs$Code)
  Code_Name_pairs_present <- Code_Name_pairs[Matches,]

  TreeData <- matrix(data = v267t1, ncol = v265t1, byrow=T)
  colnames(TreeData) = Code_Name_pairs_present$CodeN
  TreeData <- as_tibble(TreeData)
  #TreeData <- as.data.frame(TreeData)
  #names(TreeData) <- Code_Name_pairs_present$CodeN
  TreeData$StemKey = as.integer(ymd_hms(v13t4))+ TreeData$stemnumber

  # .. Logs
  LogData <- matrix(data = v257t1, ncol = v255t1, byrow=T)

  # Regarding Log_CodeCodes and names: IN SF2010 code 20 would be the same as Product Key, while code 1 would not have any direct equivalent as it is not a true unique key.

  Log_CodeCodes <- c( 1,         2,       20,    201,       202,       203,        204,      205,       206,       207,       208,       300,        301,    302,       400,   1400,    401,     1401,      402,      1402,       403,     1403,      404,      1404,       405,     1405,      406,        1406,        420,    421,        422,      423,          424,         425,         426,        500,       501,     600,        2000)
  Log_CodeNames <- c("ord_asort","specie","asor","Diatopob","Diatopub","Diamidob","Diamidub","Diabotob","Diabotub","HKSmidob","HKSmidub","forcecut","lenght","Lenclass","vol","voldec","volsob","volsobdec","volsub","volsubdec","volpob","volpobdec","volpub","volpubdec","volsmi","volsmidec","volsmiub","volsmiubdec","voldl","voldlsob","voldlsub","voldltopob","voldltopub","voldlsmiob","voldlsmiub","stem_no","log_no","numoflogs","userdefined")
  Log_Code_Name_pairs <- tibble(Code=Log_CodeCodes, CodeN= Log_CodeNames)
  Matches <- match(v256t1, Log_Code_Name_pairs$Code)
  Log_Code_Name_pairs_present <-Log_Code_Name_pairs[Matches,]
  Log_Code_Name_pairs_present$Orginal <- v256t1

  LogData <- as.data.frame(LogData)
  names(LogData) <- Log_Code_Name_pairs_present$CodeN


  Ret <- list(ReportHeader = ReportHeader,
              ObjectDefinition = ObjectDefinition,
              SpeciesGroupDefinition = SpeciesGroupDefinition,
              ProductDefinition = ProductDefinition,
              TreeData=TreeData,
              LogData=LogData,
              product_grp_table = product_grp_table,
              StanFordClassicVars = Vars,
              file_content = sfdt
              )
  return(Ret)

}







