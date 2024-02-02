#' Read StanForD Classic PRI-files (mahcine reports from forest machines)
#'
#' @param filename is the file name of the pri file to be digested
#' @param verbose Setting this parameter to TRUE will make the function also
#' return the variable content of all variables available in the file.
#'
#' @return should return a list of tables populated with data from the stm report: report_header, object_definition, operator_definition, product_definitions, stems, logs

#' @export
#'
#' @examples
#'  files <- list.files(system.file("extdata", package = "stanfordclassicr"), full.names = TRUE)
#'  prifiles <- files[stringr::str_detect(files, ".pri")]
#'  pridat1 <- read_pri_file(prifiles[1], verbose = TRUE)
#'  pridat2 <- read_pri_file(prifiles[2])
read_pri_file <- function(filename, verbose = FALSE){
  #  filename <- prifiles[1]
  strng <- file2strng(filename)
  df1 <- sfclassic2df_v2(strng)
  start_epoch <- as.integer(
    lubridate::ymd_hms(stringr::str_replace(df1$v16t4, "\n", "")))


  ## Report header
  selector <- c("v1t2", "v3t1", "v3t2", "v3t5", "v3t6",
                "v3t8", "v5t1" , "v6t1", "v12t4") #list of sfclassic vars we want

  selector <- selector[which(selector %in% names(df1))] # Ensure to not select vars not present
  selected <- df1 %>% dplyr::select( tidyselect::all_of(selector))
  report_header <- expand_stcvs(tibbl = selected) %>%
    dplyr::mutate( report_type = get0("v1t2", ifnotfound = NA_character_),
                  creation_date = get0("v12t4", ifnotfound = NA_character_),
                  country_code = get0("v6t1", ifnotfound = NA_integer_),
                  base_machine_number = get0("v3t1", ifnotfound = NA_character_),
                  base_machine_id = get0("v3t2", ifnotfound = NA_character_),
                  base_machine_manufacturer = get0("v3t5", ifnotfound = NA_character_),
                  base_machine_model = get0("v3t6", ifnotfound = NA_character_),
                  harvester_head_model = get0("v3t8", ifnotfound = NA_character_),
                  machine_application_verision = get0("v5t1", ifnotfound = NA_character_),
                  filename = stringr::str_extract(filename, pattern = "\\w*.pri")) %>%
    dplyr::select( -tidyselect::all_of(selector))



  ## Object definition
  selector <- c( "v16t4", "v21t1", "v21t2", "v21t3", "v21t4",
                 "v31t1", "v31t2", "v31t3", "v31t1",
                 "v34t2", "v34t3", "v34t5", "v34t5", "v34t6",
                 "v35t1", "v35t2") #list of sfclassic vars we want
  selector <- selector[which(selector %in% names(df1))] # Ensure to not select vars not present
  selected <- df1 %>% dplyr::select( tidyselect::all_of(selector))
  object_definition  <-
    expand_stcvs(tibbl = selected) %>%
    dplyr::mutate( object_name = get0("v21t1", ifnotfound = NA_character_),
                  object_user_id = get0("v21t1", ifnotfound = NA_character_),
                  object_start_date = lubridate::ymd_hms(get0("v16t4", ifnotfound = NA_character_)),
                  object_key = get0("start_epoch", ifnotfound = NA_integer_),
                  sub_object_name = get0("v21t2", ifnotfound = NA_character_),
                  sub_object_user_id = paste0(get0("v21t2", ifnotfound = ""),
                                              get0("v21t3", ifnotfound = ""),
                                              get0("v21t4", ifnotfound = "")),
                  sub_object_key = 0,
                  logging_org = paste0(get0("v31t1", ifnotfound = ""),
                                       get0("v31t2", ifnotfound = ""),
                                       get0("v31t3", ifnotfound = ""),
                                       get0("v31t4", ifnotfound = ""),
                                       get0("v31t5", ifnotfound = "")),
                  contractor = paste0(get0("v34t2", ifnotfound = ""),
                                      get0("v34t3", ifnotfound = ""),
                                      get0("v34t4", ifnotfound = ""),
                                      get0("v34t5", ifnotfound = "")),
                  contract_nr = dplyr::coalesce(get0("v35t2"), get0("v35t1"))) %>%
    dplyr::select( -tidyselect::matches("v\\d", perl =T))


  ## Species and Product definitions
  # Species

  selector <- c( "v120t1", "v120t3")
  selected <- df1 %>% dplyr::select( tidyselect::all_of(selector) )
  dfx <- expand_stcvs(selected)
  dfx$tmp_species_nr = 1:nrow(dfx)

  species_group_definition <-
    dfx %>%
    dplyr::mutate( species_group_name = dfx$v120t1,
                    species_group_user_id =
                     paste0(dfx$v120t1, "#", dfx$v120t3, "#",
                            stringr::str_replace( df1$v2t1, "\n", "")),
                    species_code = dfx$v120t3,
                    species_group_key =
                     as.numeric(paste0(start_epoch, dfx$tmp_species_nr)))  %>%
    dplyr::select( -tidyselect::matches("v\\d", perl =T))


  # Help-table of product groups
  if("v125t1" %in% names(df1)){
  replicator <- as.integer(unlist(stringr::str_split(df1$v125t1, " ")))
    product_grp_species_nr <- rep(1:length(replicator), replicator)
  product_grp_code <- integer()
  for (i in 1:as.integer(df1$v111t1)) {
    if(replicator[i]!=0) {
      product_grp_code <- c(product_grp_code, 1:replicator[i])
    }
  }
  product_grp_table <-
    tibble::tibble(
      product_grp_code,
      product_grp_species_nr,
      product_group_name =
        (stanfordclassicr::expand_stcvs(df1 %>%
                                          dplyr::select( .data$v127t1)) %>%
           dplyr::pull())

    )
  } else {product_grp_table <- tibble::tibble()}




  # Product definitions
  selector <- c( "v121t1", "v121t2", "v126t1", "v121t6, v126t1")
  selector <- selector[which(selector %in% names(df1))] # Ensure to not select vars not present

  selected <- df1 %>% dplyr::select( tidyselect::all_of(selector))
  prods_per_species <- as.integer(unlist(stringr::str_split(df1$v116t1, " ")))
  dfx <- expand_stcvs(selected)
  lookup <- c(product_name = "v121t1", product_info = "v121t2")
  product_definition <-
    dfx %>%
    dplyr::mutate(
           tmp_species_nr = rep(1:as.integer(df1$v111t1), prods_per_species),
           tmp_product_nr = as.integer(1:length(.data$v121t1)),
           species_group_name =
             rep(
               dplyr::pull(
                 stanfordclassicr::expand_stcvs(
                   df1 %>% dplyr::select(.data$v120t1))),
               prods_per_species),
           product_key = as.numeric(paste0(start_epoch, .data$tmp_product_nr)),
           species_group_key = as.numeric(paste0(start_epoch, .data$tmp_species_nr)))
  if(nrow(product_grp_table)>0){
    product_definition <- product_definition %>%
    dplyr::left_join( product_grp_table,
                     by = c("tmp_species_nr" = "product_grp_species_nr",
                            "v126t1" = "product_grp_code"))
  }
  product_definition <- product_definition %>%
    dplyr::select( -tidyselect::matches("tmp|v\\d", perl =T),
                   tidyselect::matches("tmp|v\\d", perl =T)) %>%
    dplyr::rename(dplyr::any_of(lookup))



  # .. harvested trees
  CodeCodes <- c( 2,               500,            505,                 723,        724,        740,        741,        750,              760,         761,           762,           763,              764,           2001)
  CodeNames <- c("species", "stemnumber", "AdaptedForBioExt", "RefDiaForDbh", "RefDiaHeigth", "dbhmm", "StemType", "OperatorNumber", "latitude", "Lat1North2South", "longitude", "Lon2West1East", "altitude","Userdefined1")
  Code_Name_pairs <- data.frame(Code=CodeCodes, CodeN= CodeNames, stringsAsFactors = F)
  Matches <- match(as.integer(unlist(stringr::str_split(df1$v266t1, " "))), Code_Name_pairs$Code)
  Code_Name_pairs_present <- Code_Name_pairs[Matches,]

  stems <- matrix(data = as.integer(unlist(stringr::str_split(df1$v267t1, " "))), ncol = as.integer(df1$v265t1), byrow=T)
  colnames(stems) = Code_Name_pairs_present$CodeN
  stems <- tibble::as_tibble(stems)
  #stems <- as.data.frame(stems)
  #names(stems) <- Code_Name_pairs_present$CodeN
  stems <-
    stems %>%
    dplyr::mutate( stem_key = paste0(start_epoch, .data$stemnumber),
           species_group_key = paste0(start_epoch, .data$species),
           object_key = start_epoch
           ) %>%
    dplyr::mutate(
                  latitude_category =
                    dplyr::case_when(
                      Lat1North2South == 1 ~ "North",
                      Lat1North2South == 2 ~ "South",
                      TRUE ~ NA_character_),
                  longitude_category =
                    dplyr::case_when( Lon2West1East == 1 ~"East",
                                      Lon2West1East == 1 ~"West",
                                      TRUE ~ NA_character_)
    )


  # .. Logs
  # LogData <- matrix(data =
  #                     as.integer(unlist(stringr::str_split(df1$v257t1, " "))),
  #                   ncol = as.integer(df1$v255t1), byrow=T)
  LogData <- matrix(data =
                     (unlist(stringr::str_split(df1$v257t1, " "))),
                    ncol = as.integer(df1$v255t1), byrow=T)

  # Regarding Log_CodeCodes and names: IN SF2010 code 20 would be the same as Product Key, while code 1 would not have any direct equivalent as it is not a true unique key.

  Log_CodeCodes <- c(   1,               2,           20,                  201,       202,        203,          204,         205,         206,           207,              208,            300,        301,    302)
  Log_CodeNames <- c("price_matrix_nr","species_nr","price_matrix_uid", "diatopob","diatopub","diamidob","diamidub","diabuttob","diabuttub","diahksmidob","diahksmidub","forcecut","length","Lenclass")

  Log_CodeCodes = c(Log_CodeCodes,  400,     1400,     401,     1401,           402,      1402,          403,        1403,          404,           1404,            405,           1405,          406,         1406,          420,         421,        422,         423,           424,              425,         426,             500,       501,         600,        2000)
  Log_CodeNames = c(Log_CodeNames, "vol", "voldec", "vol_sob","vol_sob_dec","vol_sub","vol_sub_dec","vol_top_ob","vol_top_ob_dec","vol_top_ub","vol_top_ub_dec","vol_smi_ob","vol_smi_ob_dec","vol_smi_ub","vol_smi_ub_dec","vol_dl", "vol_dl_sob","vol_dl_sub","vol_dl_top_ob","vol_dl_top_ub","vol_dl_smi_ob","vol_dl_smi_ub","stem_nr","log_nr","numoflogs","userdefined")

  Log_Code_Name_pairs <- tibble::tibble(Code=Log_CodeCodes, CodeN= Log_CodeNames)
  Matches <- match(as.integer(unlist(stringr::str_split(df1$v256t1, " "))),
                   Log_Code_Name_pairs$Code)
  Log_Code_Name_pairs_present <-Log_Code_Name_pairs[Matches,]
  Log_Code_Name_pairs_present$Orginal <-
    as.integer(
      unlist(
        stringr::str_split(df1$v256t1, " ")))

  LogData <- as.data.frame(LogData, stringsAsFactors = F)
  names(LogData) <- Log_Code_Name_pairs_present$CodeN

  LogData <- LogData %>%
    dplyr::mutate( dplyr::across(.cols = dplyr::ends_with("dec"), ~ stringr::str_pad(string = .x, width=3, side = "left", pad = "0")))


  if ("price_matrix_uid" %in% names(LogData)){
    LogData$tmp_pk = LogData$price_matrix_uid
  } else {LogData$tmp_pk = LogData$price_matrix_nr}

  if("vol_dl" %in%  names(LogData)){
    LogData <- LogData %>%
      dplyr::mutate( m3price = as.numeric(.data$vol_dl) / 10000,
                m3sob = as.numeric(.data$vol_dl_sob) / 10000,
                m3sub = as.numeric(.data$vol_dl_sub) / 10000
             )
  } else if ("vol_sob" %in% names(LogData)) {
    LogData <- LogData %>%
      dplyr::rowwise() %>%
      dplyr::mutate( m3price = as.numeric(paste(.data$vol, .data$voldec, sep = ".")),
             m3sob = as.numeric(paste(.data$vol_sob, .data$vol_sob_dec, sep = ".")),
             m3sub = as.numeric(paste(.data$vol_sub, .data$vol_sub_dec, sep = "."))) %>%
      dplyr::ungroup()

  }

  LogData <- LogData %>% dplyr::mutate_if(is.character, as.numeric)

  logs = LogData %>%
    dplyr::mutate(
      product_key =
        dplyr::case_when(
          "price_matrix_uid" %in% names(LogData) ~  as.numeric(.data$tmp_pk),
          TRUE ~ as.numeric(paste0(start_epoch, .data$tmp_pk))),
      stem_key = as.numeric(paste0(start_epoch, .data$stem_nr)),
           log_key = .data$log_nr ) %>%
    dplyr::select( .data$stem_nr, .data$log_nr, .data$diatopub,
                  .data$length, .data$m3price, .data$m3sob, .data$m3sub,
                  .data$product_key, .data$price_matrix_nr, .data$stem_key,
                  .data$log_key   )



  if(verbose == FALSE) {

 product_definition <-product_definition %>%
      dplyr::select( -tidyselect::starts_with(c("v1", "tmp")))

  Ret <- list(report_header = report_header,
              object_definition = object_definition,
              species_group_definition = species_group_definition,
              product_definition = product_definition,
              stems = stems,
              logs = logs,
              product_grp_table = product_grp_table

              )

  } else if(verbose == TRUE) {


    Ret <- list(report_header = report_header,
                object_definition = object_definition,
                species_group_definition = species_group_definition,
                product_definition = product_definition,
                stems = stems,
                logs = logs,
                product_grp_table = product_grp_table,
                present_vars = names(df1),
                present_dat = df1)
    }
  return(Ret)

}







