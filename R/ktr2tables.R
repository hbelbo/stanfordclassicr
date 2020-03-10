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
#'  read_ktr_file(ktrfiles[1])
read_ktr_file <- function(filename){

  strng <- file2strng(filename)

  strng_to_v110_1 <- stringr::str_sub(string =  strng,
                                      start = 1, end = stringr::str_locate(string = strng, pattern = "~110 1")[1,1] )

  df1 <- sfclassic2df_v2(strng_to_v110_1)
  dl1 <- sfclassic2list(strng_to_v110_1)


 ## Report header
  selector <- c("v1t2", "v3t1", "v3t2", "v3t5", "v3t6",
                "v3t8", "v6t1", "v12t4") #list of sfclassic vars we want
  report_header = populateselection(valuelist = dl1, selector = selector)

  report_header <-
    rename(report_header,report_type = v1t2,
           creation_date = v12t4,
           country_code = v6t1,
           base_machine_number = v3t1,
           base_machine_id = v3t2,
           base_machine_manufacturer = v3t5,
           base_machine_model = v3t6,
           harvester_head_model = v3t8
           )%>%
    mutate(., filename = str_extract(filename, pattern = "\\w*.ktr"))



  ## Calibrations
  # Length
  selector <- c("v41t4", "v42t1", "v42t2") #list of sfclassic vars we want
  calibsl = populateselection(valuelist = dl1, selector = selector)
  calibsl = calibsl %>% mutate(., calibrationtype = "LengthCalibration")

  #Dia
  selector <- c("v44t4", "v45t1", "v45t2") #list of sfclassic vars we want
  calibsd <- populateselection(valuelist = dl1, selector = selector)
  calibsd <- calibsd %>% mutate(.,  calibrationtype = "DiameterCalibration")

  calibrations <- bind_rows(
    calibsl %>% rename(., calibration_date = v41t4, calibration_reason = v42t1, calibration_reason_code = v42t2),
    calibsd %>% rename(., calibration_date = v44t4, calibration_reason = v45t1, calibration_reason_code = v45t2))


  # ..controls
  strng_from_v110_1 <- # Fetch the string keeping all stem level data for all stems
    stringr::str_sub(string =  strng,
                     start = stringr::str_locate(string = strng, pattern = "~110 1")[1,1],
                     end = stringr::str_length(string = strng))

  loopstring <- stringr::str_sub(string = strng_from_v110_1,  # split string to one piece per stem
                                 start = stringr::str_locate_all(string = strng_from_v110_1, pattern = "~110")[[1]][,1],
                                 end = c(stringr::str_locate_all(string = strng_from_v110_1, pattern = "~110")[[1]][-1,1], -1))


  # stemdat - one obs per stem
  stemdat <- sfclassic2df(loopstring[1])[NULL, ]
  for (i in seq_along(loopstring)){
    stemdat <- dplyr::bind_rows(stemdat, sfclassic2df(loopstring[i]))
  }
  stemdat$v110t <- dplyr::coalesce(stemdat$v110t1, stemdat$v110t2)
  stemdat <- stemdat %>% select(., -v110t2, v110t1)
  stemdat <- as_tibble(stemdat)


  ktr_vs <- tibble::tibble( #list of sfclassic vars we want to use
    v110t = "", v18t4 = "",    v18t5 = "",    v16t4 = "",    v21t1 = "",    v21t2 = "",    v21t3 = "",  v21t4 = "",  v21t5 = "",
     v31t1 = "",    v31t3 = "",    v35t1 = "",    v35t2 = "",    v211t1 = "",   v270t1 = "",   v270t2 = "",   v270t3 = "",
     v290t1 = "",   v291t5 = "",   v292t5 = "",   v293t5 = "",   v294t2 = "",   v295t2 = "",   v298t1 = "",   v306t1 = "",
     v306t2 = "",   v299t1 = "",   v299t2 = "",   v299t3 = "",   v291t3 = "",   v291t7 = "",   v291t8 = "",   v293t3 = "",
     v299t4 = "",   v372t3 = "",   v372t5 = "",   v373t3 = "",   v373t4 = "",   v373t5 = "",   v373t8 = "",   v373t9 = "",
     v374t3 = "",   v374t5 = "",   v296t1 = "",   v296t2 = "",   v296t3 = "",   v296t4 = "",   v38t1 = "",    v38t3 = "",
     v38t4 = "",    v38t5 = "",    v38t11 = "",   v523t1 = "",   v523t2 = "",   v523t3 = "",   v523t4 = "")[NULL, ]


  ktrs <-
    bind_rows(ktr_vs, stemdat) %>%
    select(., names(ktr_vs)) %>% # To ensure we have the necessary variables
    mutate(.,
           object_key = as.integer(lubridate::ymd_hms(v16t4)),
           stem_number = dplyr::coalesce(v270t3, v270t1),
           stem_key = paste0(as.integer(lubridate::ymd_hms(v16t4)), stem_number),

           measurement_date_machine = v18t4,
           measurement_date_operator = v18t5
           # ... to be continued..

           ) %>%
    select(., -starts_with("v"), starts_with("v"))


  # Extract object definitions. they are tied to each control stem and not to the file.
  object_definition <-
    tibble::tibble(object_name = ktrs$v21t1 ,
                                     object_user_id = ktrs$v21t1,
                                     object_start_date = lubridate::ymd_hms(ktrs$v16t4),
                                     object_key = as.integer(lubridate::ymd_hms(ktrs$v16t4)),
                                     sub_object_name = ktrs$v21t2,
                                     sub_object_user_id = paste0(ktrs$v21t2, " ", ktrs$v21t3,  ktrs$v21t4),
                                     sub_object_key = 0) %>% distinct()




  Ret <- list(report_header = report_header,
              calibrations = calibrations,
              ktrs = ktrs,
              object_definition = object_definition
              )
  return(Ret)

}







