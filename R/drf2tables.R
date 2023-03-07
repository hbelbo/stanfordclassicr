#' Read StanForD Classic drf-files (machine reports from forest machines)
#'
#' @param filename A file (including path) the function should read
#'
#' @return a list of tables populated with data from the drf report:
#'    report_header, object_definition, individual machine work time, combined machine work time
#' @importFrom rlang .data
#' @export
#'
#' @examples
#'  files <- list.files(system.file("extdata", package = "stanfordclassicr"), full.names = TRUE)
#'  drffiles <- files[stringr::str_detect(files, ".drf")]
#'  drf_report <- read_drf_file(drffiles[1])
#'  drf_report <- read_drf_file(drffiles[2])
#'  drf_report <- read_drf_file(drffiles[5])
read_drf_file <- function(filename){
  # filename <- drffiles[2]
  strng <- file2strng(filename)

  drfspecial <-
    tibble::tibble(
      sfv = c("v329t26","v329t27","v329t30", "v329t31"),
      sfvc = c("integer", "integer", "integer", "integer"))
  sfvardefs <- sfvardefs %>% dplyr::bind_rows(sfvardefs, drfspecial) %>%
    dplyr::distinct()



  drfdf <- sfclassic2df_v2(strng, sfvardefs = sfvardefs)
  #print(names(drfdf))

  selector <- c("v16t4") #list of sfclassic vars we want
  selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present
  if(length(selector)) {
    start_epoch <- as.numeric(lubridate::ymd_hms(stringr::str_replace(drfdf$v16t4, "\n", "")))
  } else {
    start_epoch <- NA_real_
    print("No v16t4 in the drf file")
    }


  # Report orientation (var 315t4)
  if(drfdf$v315t4 == "0") {
    print(paste(stringr::str_trunc(filename, width = 15, side = "left"),"REPORT IS time oriented"))
  } else {
      print(paste(stringr::str_trunc(filename, width = 15, side = "left"), "Report is object oriented"))
  }


  ## Report header
  selector <- c("v1t2", "v3t1", "v3t2", "v3t5", "v3t6",
                "v3t8", "v5t1" , "v6t1", "v12t4") #list of sfclassic vars we want

  selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present

  selected <- drfdf %>% dplyr::select( tidyselect::all_of(selector))
  drf_report_header <- expand_stcvs(tibbl = selected) %>%
    dplyr::mutate( report_type = get0("v1t2", ifnotfound = NA_character_),
                  creation_date = get0("v12t4", ifnotfound = NA_character_),
                  country_code = get0("v6t1", ifnotfound = NA_integer_),
                  base_machine_number = get0("v3t1", ifnotfound = NA_character_),
                  base_machine_id = get0("v3t2", ifnotfound = NA_character_),
                  base_machine_manufacturer = get0("v3t5", ifnotfound = NA_character_),
                  base_machine_model = get0("v3t6", ifnotfound = NA_character_),
                  harvester_head_model = get0("v3t8", ifnotfound = NA_character_),
                  machine_application_verision = get0("v5t1", ifnotfound = NA_character_),
                  filename = stringr::str_extract(filename, pattern = "\\w*.stm")) %>%
    dplyr::select( -tidyselect::all_of(selector))


  ## Object definition
  selector <- c( "v16t4", "v21t1", "v21t2", "v21t3", "v21t4",
                 "v31t1", "v31t2", "v31t3", "v31t1",
                 "v34t2", "v34t3", "v34t5", "v34t5", "v34t6",
                 "v35t1", "v35t2") #list of sfclassic vars we want
  selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present
  selected <- drfdf %>% dplyr::select( tidyselect::all_of(selector))

   drf_object_definition  <-
    expand_stcvs(tibbl = selected) %>%
    dplyr::mutate( object_name = get0("v21t1", ifnotfound = NA_character_),
                  object_user_id = get0("v21t1", ifnotfound = NA_character_),
                  object_start_date = lubridate::ymd_hms(get0("v16t4", ifnotfound = NA_character_)),
                  object_key = as.numeric(get0("start_epoch", ifnotfound = NA_real_)),
                  sub_object_name = get0("v21t2", ifnotfound = NA_character_),
                  sub_object_user_id = paste0(get0("v21t2", ifnotfound = ""),
                                              get0("v21t3", ifnotfound = ""),
                                              get0("v21t4", ifnotfound = "")))  %>% # dplyr::glimpse()
     dplyr::mutate(sub_object_key = dplyr::if_else( !is.na(.data$object_key), 0, NA_real_),
                  logging_org = paste0(unique(c(get0("v31t1", ifnotfound = ""),
                                       get0("v31t2", ifnotfound = ""),
                                       get0("v31t3", ifnotfound = ""),
                                       get0("v31t4", ifnotfound = ""),
                                       get0("v31t5", ifnotfound = ""))), collapse = " "),
                  contractor = paste0(unique(c(get0("v34t2", ifnotfound = ""),
                                      get0("v34t3", ifnotfound = ""),
                                      get0("v34t4", ifnotfound = ""),
                                      get0("v34t5", ifnotfound = ""))), collapse = " "),
                  contract_nr = paste(get0("v35t1", ifnotfound = ""), get0("v35t2", ifnotfound = ""), sep = " ")
                  ) %>% #dplyr::glimpse()
     dplyr::select( -tidyselect::matches("v\\d"))



  ## Species and Product definitions
  # Species
  selector <- c( "v120t1", "v120t3")
  selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present
  if(length(selector)) {
    selected <- drfdf %>% dplyr::select( tidyselect::all_of(selector))
    dfx <- expand_stcvs(selected)
    dfx$tmp_species_nr = as.integer(1:nrow(dfx))
    dfx$start_epoch =  start_epoch #if_else( is.na(start_epoch), 0L ,start_epoch)

    drf_species_group_definition <-
      dfx %>%  dplyr::mutate(species_group_name = .data$v120t1,
                      species_group_user_id =
                        paste0(
                          .data$v120t1, "#",
                          .data$v120t3, "#",
                          stringr::str_replace(
                            get0("drfdf$v2t1", ifnotfound = ""),
                            "[\n\r]{1,2}", "")),
                      species_code = .data$v120t3,
                      species_group_key = as.numeric(
                        paste0(dplyr::if_else(
                          is.na(start_epoch), 0 , start_epoch) , .data$tmp_species_nr))
                      )  %>%
      # dplyr::select( -tidyselect::matches("v\\d", perl =T))
      dplyr::select( -tidyselect::matches("v\\d"))
  } else {
    drf_species_group_definition <-  NULL
    }

  # Help-table of product groups
  if("v125t1" %in% names(drfdf)){
    replicator <- as.integer(unlist(stringr::str_split(drfdf$v125t1, " ")))
    product_grp_species_nr <- rep(1:length(replicator), replicator)
    product_grp_code <- integer()
    for (i in 1:as.integer(drfdf$v111t1)) {
      product_grp_code <- c(product_grp_code, 1:replicator[i])
      }
    product_grp_table <-
      tibble::tibble(product_grp_code,
                     product_grp_species_nr,
                     product_group_name = (expand_stcvs(drfdf %>% dplyr::select(.data$v127t1)) %>%
                                             dplyr::pull(.data))
    )

  } else {
    product_grp_table <-NULL
  }

  # Product definitions
  selector <- c( "v121t1", "v121t2", "v126t1", "v121t6, v126t1")
  selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present
  if(length(selector)){
    selected <- drfdf %>% dplyr::select( tidyselect::all_of(selector))
    prods_per_species <- as.integer(unlist(stringr::str_split(drfdf$v116t1, " ")))
    dfx <- expand_stcvs(selected)

    drf_product_definition <-
      dfx %>%
      dplyr::mutate( product_name = .data$v121t1,
           product_info = .data$v121t2,

           tmp_species_nr = rep(1:as.integer(drfdf$v111t1), prods_per_species),
           tmp_product_nr = as.integer(1:length(.data$v121t1)),
           species_group_name = rep(dplyr::pull(expand_stcvs(drfdf %>% dplyr::select(.data$v120t1))), prods_per_species),
           product_key = as.numeric(paste0(start_epoch, .data$tmp_product_nr)),
           species_group_key = as.numeric(paste0(start_epoch, .data$tmp_species_nr)))

    if( !(is.null(product_grp_table)) & ("v126t1" %in% names(drf_product_definition))) {
      drf_product_definition =
        dplyr::left_join(drf_product_definition,
                     product_grp_table,
                     by = c("tmp_species_nr" = "product_grp_species_nr",
                            "v126t1" = "product_grp_code"))
      }
    drf_product_definition = drf_product_definition %>%
      #dplyr::select( -tidyselect::matches("tmp|v\\d", perl =T), tidyselect::matches("tmp|v\\d", perl =T))
      dplyr::select( -tidyselect::matches("tmp|v\\d"), tidyselect::matches("tmp|v\\d"))

  } else {
    drf_product_definition <- NULL
  }

  # Monitoring settings
  selector <- c( "v315t1", "v315t2", "v315t3", "v315t4")
  selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present
  if(length(selector)){
    selected <- drfdf %>% dplyr::select( tidyselect::all_of(selector))
    monitoring_settings <- expand_stcvs(selected) %>%
      dplyr::rename( MonitoringFilterDown = "v315t1",
             MonitoringFilterTimeRun = "v315t2",
             MonitoringFilterTimeMinimum = "v315t3",
             MonitoringOrientation = "v315t4")
  } else {monitoring_settings <- NULL}

  # OperatorDefinition
  n_opr <- as.numeric(drfdf$v211t2)
  selector <- c(  "v212t1")
  selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present
  if(length(selector)){
    selected <- drfdf %>% dplyr::select( tidyselect::all_of(selector))
    operatordefinition <- expand_stcvs(selected)
    operatordefinition$OperatorKey <- 1:n_opr
    operatordefinition <- operatordefinition %>%
      dplyr::mutate( OperatorUserId =  .data$v212t1) %>%
      dplyr::select( "OperatorKey", "OperatorUserId")
  } else {operatordefinition <- NULL}

  #Combined Machine work time
    n_pr_opr <- as.numeric(unlist(stringr::str_split(drfdf$v316t2, " ")))
    selector <- c("v316t3", "v316t4", "v316t5", "v316t8", "v316t9")
    selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present
    if(length(selector)){
      selected <- drfdf %>%
        dplyr::select(tidyselect::all_of(selector))
      expdd <- expand_stcvs(selected) %>%
        dplyr::mutate(operatornr = rep(1:length(n_pr_opr), times = n_pr_opr) )
      runtime <- expdd
      cmwt.mrt <- runtime %>%  # make this correspond to CombinedMachineWorkTime.CombinedMachineRunTime in mom files
        dplyr::filter( .data$v316t3 %in% c(10, 11, 12, 13, 14)) %>%
        dplyr::select( mrt.category = .data$v316t4, time.sec = .data$v316t5,
                       .data$operatornr) %>%
        dplyr::group_by( .data$operatornr, .data$mrt.category) %>%
        dplyr::summarise( time.sec = sum(.data$time.sec) ) %>%
        tidyr::pivot_wider(  names_from = .data$mrt.category, values_from = .data$time.sec )

      cmwt.omd <- runtime %>% # make this correspond to CombinedMachineWorkTime.OtherMachineData in mom files
        dplyr::filter( .data$v316t3 %in% c(3)) %>% # only worktime observations
        dplyr::select( fuel.consumption = .data$v316t8,
                       driven.distance.km = .data$v316t9, .data$operatornr) %>%
        dplyr::group_by( .data$operatornr) %>%
        dplyr::summarise( fuel.consumption = sum(.data$fuel.consumption),
                         driven.distance.km = sum(.data$driven.distance.km))

      cmwt <- dplyr::full_join(cmwt.mrt, cmwt.omd, by = c("operatornr" = "operatornr"))
    } else {
      cmwt <- NULL
    }



    n_pr_opr <- as.numeric(unlist(stringr::str_split(drfdf$v317t2, " ")))
    selector <- c("v317t3", "v317t4", "v317t5")
    selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present
    if(length(selector)){
    selected <- drfdf %>%
      dplyr::select( tidyselect::all_of(selector))
    expdd <- expand_stcvs(selected) %>%
      dplyr::mutate( operatornr = rep(1:length(n_pr_opr), times = n_pr_opr) )
    irtime <- expdd %>%
      dplyr::group_by( .data$operatornr, .data$v317t4) %>% #v317t4 is description of work time
      dplyr::summarise(time.sec = sum(.data$v317t5)) %>%
      tidyr::pivot_wider( names_from = .data$v317t4,
                         values_from = .data$time.sec)
    } else { irtime <- NULL}




    n_pr_opr <- as.numeric(unlist(stringr::str_split(drfdf$v318t2, " ")))
    selector <- c("v318t3", "v318t4")
    selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present
    if(length(selector)){
      selected <- drfdf %>%
        dplyr::select( tidyselect::all_of(selector))
      expdd <- expand_stcvs(selected) %>%
        dplyr::mutate(
          operatornr = rep(1:length(n_pr_opr), times = n_pr_opr),
          v318t4 = lubridate::ymd_hms(.data$v318t4))
      worktime <- expdd

      owt <- dplyr::bind_cols( #OperatorWorkTime
        worktime %>% dplyr::filter( .data$v318t3 == 1) %>%
          dplyr::rename(MonitoringStartTime = .data$v318t4) %>%
          dplyr::select(.data$operatornr, .data$MonitoringStartTime),
        worktime %>% dplyr::filter(.data$v318t3 == 2) %>%
          dplyr::rename( MonitoringEndTime = .data$v318t4)%>%
          dplyr::select(.data$MonitoringEndTime)
        ) %>%
        dplyr::mutate(
               MonitoringTimeLength =
                 difftime(.data$MonitoringEndTime,
                          .data$MonitoringStartTime, units = "mins"))
    } else { owt <- NULL }


    cmwt <- dplyr::left_join(cmwt, irtime,  by = c("operatornr" = "operatornr"))

    ## Shiftdata --------------

    # Shifts per operator. Shiftwise
    n_pr_opr <- as.numeric(unlist(stringr::str_split(drfdf$v329t1, " ")))
   selector <- c("v329t2", "v329t3", "v329t4", "v329t5", "v329t6")
    selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present
    print(paste("v329t2 til t6:", paste0(selector, collapse = ", ")))
    if(length(selector)){
      selected <- drfdf %>%
        dplyr::select( tidyselect::any_of(selector))
      expdd <- expand_stcvs(selected) %>%
        dplyr::mutate(
          operatornr = rep(1:length(n_pr_opr), times = n_pr_opr),
          v329t2 = lubridate::ymd_hms(.data$v329t2),
          v329t3 = lubridate::ymd_hms(.data$v329t3))
      shiftdata <- expdd
      print("Names shiftdata: ")
      print(paste0(names(shiftdata), collapse = ", "))
      shiftdata <- dplyr::rename_all(shiftdata, dplyr::recode,  v329t2 = "shift_start",
                              v329t3 = "shift_end",
                              v329t5 = "shift_type_txt",
                              v329t6 = "n_subshifts",
                              operatornr = "operatornr")
      print("Names renamed shiftdata: ")
      print(paste0(names(shiftdata), collapse = ", "))

    }

      # Subshiftwise  -------------
      n_pr_shift <- as.numeric(unlist(stringr::str_split(drfdf$v329t6, " ")))
      selector <- c("v329t7", "v329t8", "v329t9", "v329t10", "v329t16", "v329t17" )
      selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present
      print(paste("v329t7 til t17:", paste0(selector, collapse = ", ")))
      if(length(selector)){
        selected <- drfdf %>%
          dplyr::select( tidyselect::any_of(selector))

        expdd <- expand_stcvs(selected)

        subshifts <- expdd
        #   dplyr::rename(sshift_starttime = v329t7,
        #               sshift_endtime = v329t8,
        #               sshift_fuelcons = v329t16,
        #               sshift_drivedist = v329t17)
        print("Names subshiftdata: ")
        print(paste0(names(subshifts), collapse = ", "))

        subshifts <- dplyr::rename_all(subshifts, dplyr::recode,  v329t7 = "sshift_starttime",
                                       v329t8 = "sshift_endtime",
                                       v329t16 = "shift_type_txt",
                          v329t6 = "sshift_fuelcons",
                          v329t17 = "sshift_drivedist")
        print("Names renamed subshifts: ")
        print(paste0(names(subshifts), collapse = ", "))
        print(paste0("nrow subshifts", nrow(subshifts)))
      } else { subshifts <- NULL }

      # per species and subshift
      selector <-  c("v329t11")
      selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present

      if(length(selector)){
      selected <- drfdf %>%
        dplyr::select( tidyselect::any_of(selector))
      expdd <- expand_stcvs(selected)

      subshift_pr_species <- expdd

      } else {  subshift_pr_species <- NULL }

      # Volumes per assortment and subshift
      selector <-  c("v329t12", "v329t13", "v329t14", "v329t15")
      selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present

      if(length(selector)){
      selected <- drfdf %>%
        dplyr::select( tidyselect::any_of(selector))
      expdd <- expand_stcvs(selected)
      subshift_pr_assortment <- expdd
      } else { subshift_pr_assortment = NULL }

      # The ponsse mysterious variables.
      # Shiftwise variables
      selector <-  c("v329t26","v329t27", "v329t31")
      selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present
      if(length(selector)){
        selected <- drfdf %>%
          dplyr::select( tidyselect::any_of(selector))
        expdd <- expand_stcvs(selected )

        ponssebonus_pr_shift <- expdd
      } else { ponssebonus_pr_shift <- NULL }

      # The even longer ponsse mysterious variable
      selector <-  c("v329t30")
      selector <- selector[which(selector %in% names(drfdf))] # Ensure to not select vars not present
      if(length(selector)){
        selected <- drfdf %>%
          dplyr::select( tidyselect::all_of(selector))
        expdd <- expand_stcvs(selected)

        ponssebonus_pr_unknown <- expdd
      } else { ponssebonus_pr_unknown <- NULL }

   imwt <- subshifts


  Ret <- list(report_header = drf_report_header,
              object_definition = drf_object_definition,
              species_group_definition = drf_species_group_definition,
              product_definition = drf_product_definition,
              monitoring_settings = monitoring_settings,
              combined_machine_work_time = cmwt,
              operator_work_time = owt,
              # TO BE COMPLETED
              imwt = imwt
              # cmwt = cmwt
              )
  return(Ret)

}







