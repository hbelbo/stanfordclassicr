#' Read StanForD Classic ktr-files (mahcine reports from forest machines)
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
#'  read_ktr_file(list.files(pattern = ".ktr")[1]
read_ktr_file = function(filename){

  # warning:
print("NBNB: this read_ktr_file functon ist really not working yet, just copied in some structure from pri2tables to get started on the development")

  strng <- file2strng(filename)

  strng_to_v110_1 <- stringr::str_sub(string =  strng,
                                      start = 1, end = stringr::str_locate(string = strng, pattern = "~110 1")[1,1] )

  df1 <- sfclassic2df(strng_to_v110_1)

  report_headervs <- tibble::tibble(
    v1t2 = "", v3t1 = "", v3t2 = "", v3t5 = "",
    v3t6 = "", v3t8="", v6t1="",
    v12t4="" )[NULL, ]
  report_header <- bind_rows(report_headervs, df1) %>% select(., names(report_headervs))
  report_header <-
    rename(report_header,report_type = v1t2,
           creation_date = v12t4,
           country_code = v6t1,
           base_machine_number = v3t1,
           base_machine_id = v3t2,
           base_machine_manufacturer = v3t5,
           base_machine_model = v3t6,
           harvester_head_model = v3t8
           )


  # .. harvested stems
  strng_from_v110_1 <- # Fetch the string keeping all stem level data for all stems
    stringr::str_sub(string =  strng,
                     start = stringr::str_locate(string = strng, pattern = "~110 1")[1,1],
                     end = stringr::str_length(string = strng))

  loopstring <- stringr::str_sub(string = strng_from_v110_1,  # split string to one piece per stem
                                 start = stringr::str_locate_all(string = strng_from_v110_1, pattern = "~110")[[1]][,1],
                                 end = c(stringr::str_locate_all(string = strng_from_v110_1, pattern = "~110")[[1]][-1,1], -1))

  present_vars <- # Find all variables and var.types present for each stem. Assuming two first trees will have all of tehm
    unlist(stringr::str_split(loopstring[1:2], pattern = "~"))[-1] %>%
    .[nchar(.)>0] %>%
    stringr::str_extract(string = .,  pattern = "[[:digit:]]{1,4}[ ]{1}[[:digit:]]{1,2}" ) %>%
    unique(.)


  # stemdat - one obs per stem
  stemdat = sfclassic2df(loopstring[1])[NULL, ]
  for (i in seq_along(loopstring)){
    stemdat <- dplyr::bind_rows(stemdat, sfclassic2df(loopstring[i]))
  }
  stemdat$v110t = dplyr::coalesce(stemdat$v110t1, stemdat$v110t2)
  stemvars = names(stemdat)



  ObjectDefinition = tibble(ObjectName = v21t1 ,
                            ObjectUserId = v21t1,
                            ObjectStartDate = ymd_hms(v16t4),
                            ObjectKey = as.integer(ObjectStartDate),
                            SubObjectName = v21t2,
                            SubObjectUserId = paste0(v21t2,  ifelse(exists("v21t3"), paste0(" ", v21t3), ""),
                                                                           ifelse(exists("v21t4"),paste0(" ", v21t4), "" )),
                            SubObjectKey = 1)

  # Species and Product definitions
  SpeciesGroupDefinition =
    tibble(SpeciesGroupName = v120t1,
               SpeciesGroupUserId = paste0(v120t1, "#",
                    ifelse(exists("v120t3"), paste0(v120t3,"#"), ""),
                    ifelse(exists("v2t1"), v2t1, "")),
               tmp_species_nr = 1:v111t1,
               SpeciesGroupkey = paste0(ifelse(exists("v13t4"), (as.numeric(v13t4)+ tmp_species_nr), ""), tmp_species_nr))


  # Help-table of product groups
  if(exists("v125t1")) {
  product_grp_species_nr =  rep(1:length(v125t1), v125t1)
  product_grp_code = integer()
  for (i in 1:v111t1) {
    product_grp_code = c(product_grp_code, 1:v125t1[i])
  }


  product_grp_table =
    tibble(product_grp_code,
               product_grp_species_nr,
               ProductGroupName = v127t1)
  } else product_grp_table = NULL

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







