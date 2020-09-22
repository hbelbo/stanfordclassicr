#' Read StanForD Classic ktr-files (mahcine reports from forest machines)
#'
#' @param filename A filename (including path) the function should read
#'
#' @return a list of tables populated with data from the stm report: report_header, object_definition, calibration dates, control measurements
#' @export
#'
#' @examples
#'  files <- list.files(system.file("extdata", package = "stanfordclassicr"), full.names = T)
#'  ktrfiles <- files[stringr::str_detect(files, ".ktr")]
#'  filename = ktrfiles[2]
#'  ktrdata <- read_ktr_file(filename)
read_ktr_file <- function(filename){

  strng <- file2strng(filename)
  strng_to_v110_1 <- stringr::str_sub(string =  strng,
                                      start = 1,
                                      end = stringr::str_locate(string = strng, pattern = "~110 1")[1,1] )
  df1 <- sfclassic2df_v2(strng_to_v110_1)

  ## Report header
  selector <- c("v1t2", "v3t1", "v3t2", "v3t5", "v3t6",
                "v3t8", "v5t1" , "v6t1", "v12t4") #list of sfclassic vars we want

  selector <- selector[which(selector %in% names(df1))] # Ensure to not select vars not present
  report_header <- df1 %>% dplyr::select(., tidyselect::all_of(selector))
  report_header <- expand_str(tibbl = report_header) %>%
    mutate(., report_type = get0("v1t2", ifnotfound = NA_character_),
           creation_date = get0("v12t4", ifnotfound = NA_character_),
           country_code = get0("v6t1", ifnotfound = NA_integer_),
           base_machine_number = get0("v3t1", ifnotfound = NA_character_),
           base_machine_id = get0("v3t2", ifnotfound = NA_character_),
           base_machine_manufacturer = get0("v3t5", ifnotfound = NA_character_),
           base_machine_model = get0("v3t6", ifnotfound = NA_character_),
           harvester_head_model = get0("v3t8", ifnotfound = NA_character_),
           machine_application_verision = get0("v5t1", ifnotfound = NA_character_),
           filename = str_extract(filename, pattern = "\\w*.ktr")) %>%
    select(., -selector)

  ## Species and Product definitions
  # Species

  selector <- c( "v120t1", "v120t3")
  selector <- selector[which(selector %in% names(df1))] # Ensure to not select vars not present
  selected <- df1 %>% dplyr::select(., tidyselect::all_of(selector))
  species <- expand_str(selected)
  species$tmp_species_nr = 1:nrow(species)





  ## Calibrations
  # Length
  nmlngth_s <- df1 %>% dplyr::select(., v40t2)
  nmlngth_n <- df1 %>% dplyr::select(., v46t1)

  selector <- c("v41t4", "v42t1", "v42t2") #list of sfclassic vars we want
  selector <- selector[which(selector %in% names(df1))]
  calibsl <- df1 %>% dplyr::select(., tidyselect::all_of(selector))

  calibsl <- expand_str(tibbl = calibsl) %>%
    dplyr::mutate(., calibration_date = get0("v41t4"),
                  calibration_reason = get0("v42t1"),
                  calibration_reason_code =get0("v42t2")) %>%
    mutate(., calibrationtype = "LengthCalibration") %>%
    select(., -tidyselect::all_of(selector))

  calibsl$species_group_user_id =
    rep(dplyr::pull(expand_str(df1 %>% select(v120t1)), 1),
        times = dplyr::pull(expand_str(df1 %>% select(v40t2))))

  #Dia
  selector <- c("v44t4", "v45t1", "v45t2") #list of sfclassic vars we want
  selector <- selector[which(selector %in% names(df1))]
  calibsd <- df1 %>% dplyr::select(., tidyselect::all_of(selector))
  calibsd <- expand_str(tibbl = calibsd)  %>%
    dplyr::mutate(., calibration_date = get0("v44t4"),
                  calibration_reason = get0("v45t1"),
                  calibration_reason_code = get0("v45t2")) %>%
    dplyr::mutate(., calibrationtype = "DiameterCalibration") %>%
    dplyr::select(., -tidyselect::all_of(selector))
  calibsd$species_group_user_id =
    rep(dplyr::pull(expand_str(df1 %>% select(v120t1)), 1),
        times = dplyr::pull(expand_str(df1 %>% select(v43t2))))

  # Both calibration types in one table
  calibrations <- dplyr::bind_rows(
    calibsl, calibsd)



  ## Stems, Logs, Controls
  strng_from_v110_1 <- # Fetch the string keeping all stem level data for all stems
    stringr::str_sub(string =  strng,
                     start = stringr::str_locate(string = strng, pattern = "~110 1")[1,1],
                     end = stringr::str_length(string = strng))

  loopstring <- stringr::str_sub(string = strng_from_v110_1,  # split string to one piece per stem
                                 start = stringr::str_locate_all(string = strng_from_v110_1, pattern = "~110")[[1]][,1],
                                 end = c(stringr::str_locate_all(string = strng_from_v110_1, pattern = "~110")[[1]][-1,1], -1))


  # stemdat - one obs per stem
  stemdat <- sfclassic2df_v2(loopstring[1])[NULL, ]
  for (i in seq_along(loopstring)){

    stemdat <- dplyr::bind_rows(stemdat, sfclassic2df_v2(loopstring[i]))
  }

  stemdat$v110t1 <- dplyr::coalesce(stemdat$v110t1, stemdat$v110t2)


  nanyna = function(x){ !(any(is.na(x)))}
  stemdat <- stemdat %>% select_if(., nanyna)

  is_one_text <- function(x){ all (stringr::str_count(x, "\n") == 1)}
  removeslash <- function(x){ stringr::str_remove(x, "\n")}
  onetextdat = dplyr::select_if(.tbl = stemdat, .predicate = is_one_text) %>%
    mutate_if(is.character, removeslash)

  is_one_number <- function(x){ all ((!stringr::str_detect(x,"\n")) & (!stringr::str_detect(x, " ")))}
  onenumdat = dplyr::select_if(.tbl = stemdat, .predicate = is_one_number)  %>%
    mutate_if(is.character, as.integer)

  stemvars = sfvardefs$sfv[which(sfvardefs$sfv %in% names(stemdat))] # for reordering to varname vartype

  stemdat <- stemdat %>% dplyr::select(., -tidyselect::all_of(c(names(onetextdat), names(onenumdat)))) %>%
    dplyr::bind_cols(., onenumdat) %>%
    dplyr::bind_cols(.,onetextdat) %>%
    ## Reorder variables in ascending order
    dplyr::select(., tidyselect::all_of(stemvars)) %>%
    mutate(.,
           object_key = get0("v16t4"), #as.integer(lubridate::ymd_hms(get0("v16t4"))),
           stem_number = dplyr::coalesce(get0("v270t3"), get0("v270t1")),
           stem_key = paste0((get0("v16t4")), stem_number), # as.numeric((paste0(as.numeric(lubridate::ymd_hms(get0("v16t4"))), stem_number))),
           species_nr = get0("v110t1"),
           measurement_date_machine = get0("v18t4"),
           measurement_date_operator = get0("v18t5")) %>%
    select(., -starts_with("v"), starts_with("v"))

  # preparing logs dataset ----

  logselector <- c( "v291t3",
                    "v291t5", "v291t7", "v291t8",
                    "v292t5",
                    "v293t3","v293t5", "v294t2",
                    "v295t2",
                    "v296t1",
                    "v296t2", "v296t3",  "v298t1",
                    "v299t1", "v299t2","v299t3", "v299t4","v306t1",
                    "v306t2",  "v372t3", "v372t5"
  )

  stmlogdat <- stemdat %>% dplyr::select(., which(names(stemdat) %in% logselector))
  logselector = logselector[which(logselector %in% names(stmlogdat))]

  logs  <- expand_str(tibbl = stmlogdat)
  logs$stem_key <- rep(stemdat$stem_key, stemdat$v290t1)
  logs$object_key <- rep(stemdat$object_key, stemdat$v290t1)
  logs$log_key <- sequence(stemdat$v290t1)

  logselector_n <- c( "v291t3", "v291t5", "v292t5",
                      "v293t3","v293t5", "v294t2",
                      "v295t2",
                      "v296t1",
                      "v296t2", "v296t3",
                      "v299t1", "v299t2","v299t3")

  logs <- logs %>%
    mutate(.,

           product_key = as.numeric(paste0(object_key, v296t1)),
           product_name = get0("v296t2"),
           assortment_code = get0("v296t3"),
           price_category_code = get0("v296t4"),
           m3price = get0("v299t1"),
           m3sub = get0("v299t2"),
           m3sob = get0("v299t3"),
           topdiaob_manual = get0("v291t3"),
           topdiaob_machine = get0("v291t5"),
           topdiaub_manual = get0("v292t3"),
           topdiaub_machine = get0("v292t5"),
           length_manual = get0("v293t3"),
           length_machine = get0("v293t5"),
           length_class = get0("v295t2"),
           dia_class = get0("v294t2")
    ) %>%
    select(., -tidyselect::all_of(logselector_n),
           -tidyselect::matches("v\\d*"), tidyselect::matches("v\\d*"))



  # ktr data

  ktr_selector <- c( "v373t3", "v373t4",  "v373t5", "v373t6",
                     "v374t3", "v374t5")

  ktr_selector <- ktr_selector[which(ktr_selector %in% stemvars)]
  ktrs = stemdat %>% dplyr::select(., tidyselect::all_of(ktr_selector))


  ktrsd  <- expand_str(ktrs)
  if(sum(logs$v372t3) == dim(ktrsd)[1]){
    ktrsd$stem_key <- rep(logs$stem_key, logs$v372t3)
    ktrsd$logkey <- rep(logs$log_key, logs$v372t3)
    selectable <- c("v373t3", "v373t4",  "v373t5", "v373t6", "v374t3", "v374t5")
    selectable <- selectable[which(selectable %in% ktr_selector)]
    ktrsd <- ktrsd %>%
      dplyr::mutate(.,
                    ctr_pos_manual = get0("v374t3"),
                    ctr_dia_manual = get0("v373t3"),
                    ctr_pos_machine = get0("v374t5"),
                    ctr_dia_machine = get0("v373t5"),
                    #ctr_dia_manual_uf = get0("v373t4"),
                    #ctr_dia_machine_uf = get0("v373t6"),
                    ) %>%
      dplyr::select(., -tidyselect::all_of(selectable)) %>%
      left_join(., (stemdat %>%
                      select(., stem_key,
                             measurement_date_machine = v18t4,
                             measurement_date_operator = v18t5)))

  } else {
    ktrsd$stem_key <- NA
    ktrsd$logkey <- NA
    selectable <- c("v373t3", "v373t4",  "v373t5", "v373t6", "v374t3", "v374t5")
    selectable = selectable[which(selectable %in% ktr_selector)]
    ktrsd <- ktrsd %>%
      dplyr::mutate(., ctr_dia_manual = get0("v373t3"),
                    #ctr_dia_manual_uf = get0("v373t4"),
                    ctr_dia_machine = get0("v373t5"),
                    #ctr_dia_machine_uf = get0("v373t6"),
                    ctr_pos_manual = get0("v374t3"),
                    ctr_pos_machine = get0("v374t5")) %>%
      dplyr::select(., -tidyselect::all_of(selectable)) %>%
      select(., -tidyselect::starts_with("v"), tidyselect::starts_with("v"))
  }


  Ret <- list(report_header = report_header,
              calibrations = calibrations,
              species = species,
              stemdat = stemdat,
              logs = logs,
              ctrs = ktrsd
  )
  return(Ret)

}







