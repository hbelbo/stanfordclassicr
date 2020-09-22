#' Read StanForD Classic PRI-files (mahcine reports from forest machines)
#'
#' @param filename
#'
#' @return should return a list of tables populated with data from the stm report: report_header, object_definition, operator_definition, product_definitions, stems, logs
#' @export
#'
#' @examples
#'  files <- list.files(system.file("extdata", package = "stanfordclassicr"), full.names = T)
#'  prifiles <- files[stringr::str_detect(files, ".pri")]
#'  read_pri_file(prifiles[1])
read_pri_file2 <- function(filename, verbose = FALSE){
  #  filename <- prifiles[1]
  strng <- file2strng(filename)
  df1 <- sfclassic2df_v2(strng)
  start_epoch <- as.integer(lubridate::ymd_hms(stringr::str_replace(df1$v16t4, "\n", "")))


  ## Report header
  selector <- c("v1t2", "v3t1", "v3t2", "v3t5", "v3t6",
                "v3t8", "v5t1" , "v6t1", "v12t4") #list of sfclassic vars we want

  selector <- selector[which(selector %in% names(df1))] # Ensure to not select vars not present
  selected <- df1 %>% dplyr::select(., tidyselect::all_of(selector))
  report_header <- expand_str(tibbl = selected) %>%
    dplyr::mutate(., report_type = get0("v1t2", ifnotfound = NA_character_),
                  creation_date = get0("v12t4", ifnotfound = NA_character_),
                  country_code = get0("v6t1", ifnotfound = NA_integer_),
                  base_machine_number = get0("v3t1", ifnotfound = NA_character_),
                  base_machine_id = get0("v3t2", ifnotfound = NA_character_),
                  base_machine_manufacturer = get0("v3t5", ifnotfound = NA_character_),
                  base_machine_model = get0("v3t6", ifnotfound = NA_character_),
                  harvester_head_model = get0("v3t8", ifnotfound = NA_character_),
                  machine_application_verision = get0("v5t1", ifnotfound = NA_character_),
                  filename = str_extract(filename, pattern = "\\w*.pri")) %>%
    dplyr::select(., -tidyselect::all_of(selector))



  ## Object definition
  selector <- c( "v16t4", "v21t1", "v21t2", "v21t3", "v21t4",
                 "v31t1", "v31t2", "v31t3", "v31t1",
                 "v34t2", "v34t3", "v34t5", "v34t5", "v34t6",
                 "v35t1", "v35t2") #list of sfclassic vars we want
  selector <- selector[which(selector %in% names(df1))] # Ensure to not select vars not present
  selected <- df1 %>% dplyr::select(., tidyselect::all_of(selector))
  object_definition  <-
    expand_str(tibbl = selected) %>%
    dplyr::mutate(., object_name = get0("v21t1", ifnotfound = NA_character_),
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
    dplyr::select(., -tidyselect::matches("v\\d", perl =T))


  ## Species and Product definitions
  # Species

  selector <- c( "v120t1", "v120t3")
  selected <- df1 %>% dplyr::select(., tidyselect::all_of(selector))
  dfx <- expand_str(selected)
  dfx$tmp_species_nr = 1:nrow(dfx)

  species_group_definition <-
    dfx %>%  mutate(., species_group_name = v120t1,
                    species_group_user_id = paste0(v120t1, "#", v120t3, "#", stringr::str_replace( df1$v2t1, "\n", "")),
                    species_code = v120t3,
                    species_group_key = as.numeric(paste0(start_epoch, tmp_species_nr)))  %>%
    dplyr::select(., -tidyselect::matches("v\\d", perl =T))


  # Help-table of product groups
  replicator <- as.integer(unlist(stringr::str_split(df1$v125t1, " ")))
    product_grp_species_nr <- rep(1:length(replicator), replicator)
  product_grp_code <- integer()
  for (i in 1:as.integer(df1$v111t1)) {
    product_grp_code <- c(product_grp_code, 1:replicator[i])
  }
  product_grp_table <-
    tibble::tibble(product_grp_code,
                   product_grp_species_nr,
                   product_group_name = (expand_str(df1 %>% dplyr::select(., v127t1)) %>% dplyr::pull(.))
    )



  # Product definitions
  selector <- c( "v121t1", "v121t2", "v126t1", "v121t6, v126t1")
  selector <- selector[which(selector %in% names(df1))] # Ensure to not select vars not present
  selected <- df1 %>% dplyr::select(., tidyselect::all_of(selector))
  prods_per_species <- as.integer(unlist(stringr::str_split(df1$v116t1, " ")))
  dfx <- expand_str(selected)

  product_definition <-
    dfx %>%
    mutate(., product_name = v121t1,
           product_info = v121t2,
           v126t1 = v126t1,
           tmp_species_nr = rep(1:as.integer(df1$v111t1), prods_per_species),
           tmp_product_nr = as.integer(1:length(v121t1)),
           species_group_name = rep(dplyr::pull(expand_str(df1 %>% dplyr::select(., v120t1))), prods_per_species),
           product_key = as.numeric(paste0(start_epoch, tmp_product_nr)),
           species_group_key = as.numeric(paste0(start_epoch, tmp_species_nr))) %>%
    dplyr::left_join(.,
                     product_grp_table,
                     by = c("tmp_species_nr" = "product_grp_species_nr",
                            "v126t1" = "product_grp_code")) %>%
    dplyr::select(., -matches("tmp|v\\d", perl =T), matches("tmp|v\\d", perl =T))



  # .. harvested trees
  CodeCodes <- c( 2,               500,            505,                 723,        724,        740,        741,        750,              760,         761,           762,           763,              764,           2001)
  CodeNames <- c("species", "stemnumber", "AdaptedForBioExt", "RefDiaForDbh", "RefDiaHeigth", "dbhmm", "StemType", "OperatorNumber", "latitude", "Lat1North2South", "longitude", "Lon2West1East", "altitude","Userdefined1")
  Code_Name_pairs <- data.frame(Code=CodeCodes, CodeN= CodeNames, stringsAsFactors = F)
  Matches <- match(as.integer(unlist(stringr::str_split(df1$v266t1, " "))), Code_Name_pairs$Code)
  Code_Name_pairs_present <- Code_Name_pairs[Matches,]

  stems <- matrix(data = as.integer(unlist(stringr::str_split(df1$v267t1, " "))), ncol = as.integer(df1$v265t1), byrow=T)
  colnames(stems) = Code_Name_pairs_present$CodeN
  stems <- as_tibble(stems)
  #stems <- as.data.frame(stems)
  #names(stems) <- Code_Name_pairs_present$CodeN
  stems <-
    stems %>%
    dplyr::mutate(., stem_key = paste0(start_epoch, stemnumber),
           species_group_key = paste0(start_epoch, species),
           object_key = start_epoch
           ) %>%
    dplyr::mutate(.,
                  latitude_category =
                    case_when(
                      Lat1North2South == 1 ~ "North",
                      Lat1North2South == 2 ~ "South",
                      TRUE ~ NA_character_),
                  longitude_category = case_when( Lon2West1East == 1 ~"East", Lon2West1East == 1 ~"West", TRUE ~ NA_character_)
    )


  # .. Logs
  LogData <- matrix(data =
                      as.integer(unlist(stringr::str_split(df1$v257t1, " "))),
                    ncol = as.integer(df1$v255t1), byrow=T)

  # Regarding Log_CodeCodes and names: IN SF2010 code 20 would be the same as Product Key, while code 1 would not have any direct equivalent as it is not a true unique key.

  Log_CodeCodes <- c(   1,               2,           20,                  201,       202,        203,          204,         205,         206,           207,              208,            300,        301,    302)
  Log_CodeNames <- c("price_matrix_nr","species_nr","price_matrix_uid", "diatopob","diatopub","diamidob","diamidub","diabuttob","diabuttub","diahksmidob","diahksmidub","forcecut","length","Lenclass")

  Log_CodeCodes = c(Log_CodeCodes,  400,     1400,     401,     1401,           402,      1402,          403,        1403,          404,           1404,            405,           1405,          406,         1406,          420,         421,        422,         423,           424,              425,         426,             500,       501,         600,        2000)
  Log_CodeNames = c(Log_CodeNames, "vol", "voldec", "vol_sob","vol_sob_dec","vol_sub","vol_sub_dec","vol_top_ob","vol_top_ob_dec","vol_top_ub","vol_top_ub_dec","vol_smi_ob","vol_smi_ob_dec","vol_smi_ub","vol_smi_ub_dec","vol_dl", "vol_dl_sob","vol_dl_sub","vol_dl_top_ob","vol_dl_top_ub","vol_dl_smi_ob","vol_dl_smi_ub","stem_nr","log_nr","numoflogs","userdefined")

  Log_Code_Name_pairs <- tibble(Code=Log_CodeCodes, CodeN= Log_CodeNames)
  Matches <- match(as.integer(unlist(stringr::str_split(df1$v256t1, " "))), Log_Code_Name_pairs$Code)
  Log_Code_Name_pairs_present <-Log_Code_Name_pairs[Matches,]
  Log_Code_Name_pairs_present$Orginal <- as.integer(unlist(stringr::str_split(df1$v256t1, " ")))

  LogData <- as.data.frame(LogData, stringsAsFactors = F)
  names(LogData) <- Log_Code_Name_pairs_present$CodeN

  if ("price_matrix_uid" %in% names(LogData)){
    LogData$tmp_pk = LogData$price_matrix_uid
  } else {LogData$tmp_pk = LogData$price_matrix_nr}

  if("vol_dl" %in%  names(LogData)){
    LogData <- LogData %>%
      mutate(., m3price = as.numeric(vol_dl) / 10000,
                m3sob = as.numeric(vol_dl_sob) / 10000,
                m3sub = as.numeric(vol_dl_sub) / 10000
             )
  } else if ("vol_sob" %in% names(LogData)) {
    LogData <- LogData %>% rowwise() %>%
      mutate(., m3price = as.numeric(paste(vol, voldec, sep = ".")),
             m3sob = as.numeric(paste(vol_sob, vol_sob_dec, sep = ".")),
             m3sub = as.numeric(paste(vol_sub, vol_sub_dec, sep = ".")))

  }

  logs = LogData %>%
    mutate(.,
           product_key = case_when("price_matrix_uid" %in% names(LogData) ~  as.numeric(tmp_pk),
                                   TRUE ~ as.numeric(paste0(start_epoch, tmp_pk))),
           stem_key = as.numeric(paste0(start_epoch, stem_nr)),
           log_key = log_nr ) %>%
    select(., stem_nr, log_nr, diatopub, length, m3price, m3sob, m3sub,
           product_key, price_matrix_nr, stem_key, log_key,   )




  Ret <- list(report_header = report_header,
              object_definition = object_definition,
              species_group_definition = species_group_definition,
              product_definition = product_definition,
              stems = stems,
              logs = logs,
              product_grp_table = product_grp_table,
              present_vars = names(df1)

              )

  if(verbose == TRUE) {
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







