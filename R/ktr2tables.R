#' Read StanForD Classic ktr-files (mahcine reports from forest machines)
#'
#' @param filename A filename (including path) the function should read
#'
#' @return a list of tables populated with data from the stm report: report_header, object_definition, calibration dates, control measurements
#' @export
#'
#' @examples
#'  files <- list.files(system.file("extdata", package = "stanfordclassicr"), full.names = TRUE)
#'  ktrfiles <- files[stringr::str_detect(files, ".ktr")]
#'  filename = ktrfiles[2]
#'  ktrdata <- read_ktr_file(filename)
#'  ktrdata <- read_ktr_file(ktrfiles[1])
#'  ktrdata <- read_ktr_file(ktrfiles[2])
#'  ktrdata <- read_ktr_file(ktrfiles[3])
#'  ktrdata <- read_ktr_file(ktrfiles[4])
#'
read_ktr_file <- function(filename){
# print(filename)
  strng <- file2strng(filename)
  # strng <- file2strng("C:/Users/Hbel/Documents/hbdev/R_pkgs/StanFordClassicPackage/inst/extdata/ktr_JD_Timbermatic_1_16_11_20181024.ktr")
  strng_to_v110_1 <- stringr::str_sub(string =  strng,
                                      start = 1,
                                      end = stringr::str_locate(string = strng, pattern = "~110 1")[1,1] )
  df1 <- sfclassic2df_v2(strng_to_v110_1)

  ## Report header
  selector <- c("v1t2", "v3t1", "v3t2", "v3t5", "v3t6",
                "v3t8", "v5t1" , "v6t1", "v12t4") #list of sfclassic vars we want

  selector <- selector[which(selector %in% names(df1))] # Ensure to not select vars not present
  report_header <- df1 %>% dplyr::select(tidyselect::all_of(selector))
  report_header <- stanfordclassicr::expand_stcvs(tibbl = report_header) %>%
    dplyr::mutate( report_type = get0("v1t2", ifnotfound = NA_character_),
           creation_date = get0("v12t4", ifnotfound = NA_character_),
           country_code = get0("v6t1", ifnotfound = NA_integer_),
           base_machine_number = get0("v3t1", ifnotfound = NA_character_),
           base_machine_id = get0("v3t2", ifnotfound = NA_character_),
           base_machine_manufacturer = get0("v3t5", ifnotfound = NA_character_),
           base_machine_model = get0("v3t6", ifnotfound = NA_character_),
           harvester_head_model = get0("v3t8", ifnotfound = NA_character_),
           machine_application_verision = get0("v5t1", ifnotfound = NA_character_),
           filename = stringr::str_extract(filename, pattern = "\\w*.ktr")) %>%
    dplyr::select( -selector)

  ## Species and Product definitions
  # Species

  selector <- c( "v120t1", "v120t3")
  selector <- selector[which(selector %in% names(df1))] # Ensure to not select vars not present
  if(length(selector)){
     selected <- df1 %>% dplyr::select( tidyselect::all_of(selector))
  species <- expand_stcvs(selected)
  species$tmp_species_nr = 1:nrow(species)
  } else {
    species <- NULL
  }


  ## Calibrations
  # Length
  # nmlngth_s <- df1 %>% dplyr::select( .data$v40t2) # Number of length calibs per tree species
  # nmlngth_n <- df1 %>% dplyr::select(.data$v46t1)  # Length positions

  selector <- c("v41t4", "v42t1", "v42t2") #list of sfclassic vars we want
  selector <- selector[which(selector %in% names(df1))]

  if(length(selector)){
    calibsl <- df1 %>% dplyr::select(tidyselect::all_of(selector))

  calibsl <- expand_stcvs(tibbl = calibsl) %>%
    dplyr::mutate( calibration_date = get0("v41t4"),
                  calibration_reason = get0("v42t1"),
                  calibration_reason_code =get0("v42t2")) %>%
    dplyr::mutate( calibrationtype = "LengthCalibration") %>%
    dplyr::select( -tidyselect::all_of(selector))
print(names(df1))

  if(!is.null(species) & "v40t2" %in% names(df1)){
    calibsl$species_group_user_id =
    rep(dplyr::pull(expand_stcvs(df1 %>% dplyr::select(.data$v120t1)), 1),
        times = dplyr::pull(expand_stcvs(df1 %>% dplyr::select(.data$v40t2))))
    }
  } else { calibsl <- NULL}


  #Dia
  selector <- c("v44t4", "v45t1", "v45t2") #list of sfclassic vars we want
  selector <- selector[which(selector %in% names(df1))]
  if(length(selector)){
       calibsd <- df1 %>% dplyr::select( tidyselect::all_of(selector))
    calibsd <- stanfordclassicr::expand_stcvs(tibbl = calibsd)  %>%
      dplyr::mutate(calibration_date = get0("v44t4"),
                    calibration_reason = get0("v45t1"),
                    calibration_reason_code = get0("v45t2")) %>%
      dplyr::mutate( calibrationtype = "DiameterCalibration") %>%
      dplyr::select( -tidyselect::all_of(selector))
    if(!is.null(species) & "v43t2" %in% names(df1)){
      calibsd$species_group_user_id =
        rep(dplyr::pull(expand_stcvs(df1 %>% dplyr::select(.data$v120t1)), 1),
            times = dplyr::pull(expand_stcvs(df1 %>% dplyr::select(.data$v43t2))))
    }
  } else {
   calibsd <- NULL
 }

  # Both calibration types in one table
  calibrations <- dplyr::bind_rows(calibsl, calibsd)



  ## Stems, Logs, Controls
  strng_from_v110_1 <- # Fetch the string keeping all stem level data for all stems
    stringr::str_sub(string =  strng,
                     start = stringr::str_locate(string = strng, pattern = "~110 1")[1,1],
                     end = stringr::str_length(string = strng))

  loopstring <-
    stringr::str_sub(string = strng_from_v110_1,  # split string to one piece per stem
                        start =
                       stringr::str_locate_all(string = strng_from_v110_1, pattern = "~110")[[1]][,1],
                        end =
                       c(stringr::str_locate_all(string = strng_from_v110_1, pattern = "~110")[[1]][-1,1], -1))

  if(length(loopstring)){ #Then we have stems recorded and can do stems,
      # logs and control measurements
    # stemdat - one obs per stem
     stemdat <- sfclassic2df_v2(loopstring[1])
     if (length(loopstring)>1) {
     stemdat <- stemdat[NULL, ]
      for (i in seq_along(loopstring)){
       print(paste0( "i = ", i))
       stemdat <- dplyr::bind_rows(stemdat, sfclassic2df_v2(loopstring[i]))
      }
      stemdat$v110 <-  dplyr::coalesce(stemdat$v110t1, stemdat$v110t2) # c(tmp1, tmp2)
     } else {
       stemdat$v110 <- stemdat$v110t1
     }

    stemdat <- stemdat[, !names(stemdat) %in% c("v110t1", "v110t2")]
    print(paste0(c("Names stemdat: ", names(stemdat)), collapse = ", "))
    print(paste0(c("Dim stemdat", dim(stemdat)), collapse = ", "))


    nanyna = function(x){ !(any(is.na(x)))}
    stemdat <- stemdat %>% dplyr::select_if( nanyna)

    is_one_text <- function(x){ all (stringr::str_count(x, "\n") == 1)}
    removeslash <- function(x){ stringr::str_remove(x, "\n")}
    onetextdat = dplyr::select_if(.tbl = stemdat, .predicate = is_one_text) %>%
      dplyr::mutate_if(is.character, removeslash)

    is_one_number <- function(x){ all ((!stringr::str_detect(x,"\n")) & (!stringr::str_detect(x, " ")))}
    onenumdat = dplyr::select_if(.tbl = stemdat, .predicate = is_one_number)  %>%
      dplyr::mutate_if(is.character, as.integer)

    stemvars = stanfordclassicr::sfvardefs$sfv[which(stanfordclassicr::sfvardefs$sfv %in% names(stemdat))] # for reordering to varname vartype

  stemdat <- stemdat %>%
    dplyr::select( -tidyselect::all_of(c(names(onetextdat), names(onenumdat)))) %>%
    dplyr::bind_cols( onenumdat) %>%
    dplyr::bind_cols( onetextdat) %>%
    ## Reorder variables in ascending order
    dplyr::select( tidyselect::all_of(stemvars)) %>%
    dplyr::mutate(
           object_key = get0("v16t4"))

  if("v270t3" %in% stemvars)  {
    stemdat <- stemdat %>% dplyr::mutate(stem_number = get0("v270t3"))
  } else if ("v270t1" %in% stemvars) {
    stemdat <- stemdat %>% dplyr::mutate(stem_number = get0("v270t1"))
  } else  stemdat <- stemdat %>% dplyr::mutate(stem_number = NA_real_)

  stemdat <- stemdat %>%
    dplyr::mutate(
           # PROBLEM is we  do not have v16t1 for each stem, therefore no stemkey.
           #stem_key = paste0( .data$object_key, .data$stem_number), # as.numeric((paste0(as.numeric(lubridate::ymd_hms(get0("v16t4"))), stem_number))),
           species_n = get0("v120t2"),
           measurement_date_machine = get0("v18t4"),
           measurement_date_operator = get0("v18t5")) %>%
    dplyr::select( -tidyselect::starts_with("v"), tidyselect::starts_with("v"))

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

  stmlogdat <- stemdat %>% dplyr::select( which(names(stemdat) %in% logselector))
  logselector <- logselector[which(logselector %in% names(stmlogdat))]
if(length(logselector)){
  logs  <- expand_stcvs(tibbl = stmlogdat)
  logs$stem_number <- rep(stemdat$stem_number, stemdat$v290t1)
  #logs$stem_key <- rep(stemdat$stem_key, stemdat$v290t1)
  #logs$object_key <- rep(stemdat$object_key, stemdat$v290t1)
  logs$log_key <- sequence(stemdat$v290t1)

  logselector_n <- c( "v291t3", "v291t5", "v292t5",
                      "v293t3","v293t5", "v294t2",
                      "v295t2",
                      "v296t1",
                      "v296t2", "v296t3",
                      "v299t1", "v299t2","v299t3")

  logselector_n <- logselector_n[which(logselector_n %in% logselector)]

  logs <-
    logs %>%
    dplyr::mutate(
           #product_number = as.numeric(.data$v296t1),
           #product_key = as.numeric(paste0(.data$object_key, .data$v296t1)),
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
    dplyr::select( -tidyselect::all_of(logselector_n),
           -tidyselect::matches("v\\d*"), tidyselect::matches("v\\d*"))
} else {
  logs <- NULL
}




ktr_selector <- c( "v373t3", "v373t4",  "v373t5", "v373t6","v374t3", "v374t5")
ktrs <- stemdat %>% dplyr::select( tidyselect::any_of(ktr_selector))
if(ncol(ktrs)>1){
    ktrsd  <- stanfordclassicr::expand_stcvs(ktrs)
  if(sum(logs$v372t3) == dim(ktrsd)[1]){
    ktrsd$stem_number <- rep(logs$stem_number, logs$v372t3)
    #ktrsd$stem_key <- rep(logs$stem_key, logs$v372t3)
    ktrsd$logkey <- rep(logs$log_key, logs$v372t3)
    selectable <- c("v373t3", "v373t4",  "v373t5", "v373t6", "v374t3", "v374t5")
    selectable <- selectable[which(selectable %in% ktr_selector)]
    ktrsd <- ktrsd %>%
      dplyr::mutate(
                    ctr_pos_manual = get0("v374t3"),
                    ctr_dia_manual = get0("v373t3"),
                    ctr_pos_machine = get0("v374t5"),
                    ctr_dia_machine = get0("v373t5"),
                    #ctr_dia_manual_uf = get0("v373t4"),
                    #ctr_dia_machine_uf = get0("v373t6"),
                    ) %>%
      dplyr::select( -tidyselect::any_of(selectable))

    if("v18t4" %in% names(stemdat)) {
        ktrsd <- ktrsd %>%
          dplyr::left_join( (stemdat %>%
                             dplyr::select( .data$stem_number,
                               measurement_date_machine = .data$v18t4,
                               measurement_date_operator = .data$v18t5)))
        }
    ktrsd <- ktrsd %>% dplyr::select( -tidyselect::starts_with("v"), tidyselect::starts_with("v"))
      } else {
        ktrsd <- NULL
        print("Number of manual measurements v372t3 do not correspond the number of rows in the control diameter table (v373t3)")
        print(paste0(c("ktrfile filename: ", filename)))
      }
     } else {
       ktrsd <- NULL
       print("No control variables found in the stemdata in the ktr file")
       print(paste0(c("ktrfile filename: ", filename)))
      }
    } else {
    stemdat <- NULL
    print("No stemstrings v110t loopstrings found in ktr file")
    print(paste0(c("ktrfile filename: ", filename)))
  } # end if loopstrings


  Ret <- list(report_header = report_header,
              calibrations = calibrations,
              species = species,
              stemdat = stemdat,
              logs = logs,
              ctrs = ktrsd
  )
  return(Ret)

}







