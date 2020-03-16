#' Read StanForD Classic .stm-files (mahcine reports from forest machines)
#'
#' @param filename (including path)
#'
#' @return a list of tables populated with data from the stm report: report_header, object_definition, operator_definition, product_definitions, stems, logs
#' @export
#' @details this function reads one stanford stemfile (.stm)
#'
#' @examples
#'  files <- list.files(system.file("extdata", package = "stanfordclassicr"), full.names = T)
#'  stmfiles <- files[stringr::str_detect(files, ".stm")]
#'  read_stm_file(stmfiles[1])
#'  filename <- stmfiles[1]
read_stm_file <- function(filename){
    strng <- file2strng(filename)

    strng_to_v110_1 <- stringr::str_sub(string =  strng,
                                        start = 1, end = stringr::str_locate(string = strng, pattern = "~110 1")[1,1] )

    vls <- sfclassic2list(strng_to_v110_1) #Should correspond to vls
    #start_epoch = as.integer(lubridate::ymd_hms(vls$v16t4))
    df1 <- sfclassic2df_v2(strng_to_v110_1)
    start_epoch = as.integer(lubridate::ymd_hms(stringr::str_replace(df1$v16t4, "\n", "")))


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
             filename = str_extract(filename, pattern = "\\w*.stm")) %>%
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


    # Species and Product definitions
    selector <- c( "v120t1", "v120t3")
    selected <- df1 %>% dplyr::select(., tidyselect::all_of(selector))
    dfx = expand_str(selected)
    species_group_definition <-
      dfx %>%  mutate(., species_group_name = v120t1,
                      species_group_user_id = paste0(v120t1, "#", v120t3, "#", stringr::str_replace( df1$v2t1, "\n", "")),
                      tmp_species_nr = as.integer(v120t3),
                      species_group_key = as.numeric(paste0(start_epoch, tmp_species_nr)))  %>%
      dplyr::select(., -tidyselect::matches("v\\d", perl =T))


    # Help-table of product groups
    product_grp_species_nr <- rep(1:length(vls$v125t1), vls$v125t1)
    product_grp_code <- integer()
    for (i in 1:vls$v111t1) {
      product_grp_code = c(product_grp_code, 1:vls$v125t1[i])
    }
    product_grp_table <-
      tibble::tibble(product_grp_code,
                     product_grp_species_nr,
                     product_group_name = (expand_str(df1 %>% dplyr::select(., v127t1)) %>% dplyr::pull(.))
      )

    # Product definitions
    selector <- c( "v121t1", "v121t2", "v126t1", "v121t6", "v126t1")
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
    stemdat <- stemdat %>% dplyr::select_if(., nanyna)

    is_one_text <- function(x){ all (stringr::str_count(x, "\n") == 1)}
    removeslash <- function(x){ stringr::str_remove(x, "\n")}
    onetextdat = dplyr::select_if(.tbl = stemdat, .predicate = is_one_text) %>%
      mutate_if(is.character, removeslash)

    is_one_number <- function(x){ all ((!stringr::str_detect(x,"\n")) & (!stringr::str_detect(x, " ")))}
    onenumdat = dplyr::select_if(.tbl = stemdat, .predicate = is_one_number)  %>%
      mutate_if(is.character, as.integer)


    stemvars = sfvardefs$sfv[which(sfvardefs$sfv %in% names(stemdat))] # for reordering to varname vartype
    stemdat <-
      stemdat %>% dplyr::select(., -tidyselect::all_of(c(names(onetextdat), names(onenumdat)))) %>%
      dplyr::bind_cols(., onenumdat) %>%
      dplyr::bind_cols(.,onetextdat) %>%
      ## Reorder variables in ascending order
      dplyr::select(., tidyselect::all_of(stemvars)) %>%
      mutate(.,
             object_key = start_epoch,
             stem_number = dplyr::coalesce(get0("v270t3"), get0("v270t1")),
             stem_key = as.numeric(paste0(start_epoch, stem_number)),
             species_nr = get0("v110t1"),
             measurement_date_machine = get0("v18t4"),
             DBHmm = dplyr::coalesce(get0("v281t1"), get0("v281t2"))
      )

    if("v521t1" %in% names(stemdat)){
      stemdat <- stemdat %>%
        dplyr::mutate(.,
                      latitude = as.numeric(v523t1),  # v523t1 COORD Latitude ,
                      longitude = as.numeric(v523t3),  # v523t3 COORD Longitude
                      stem_coordinate_position = dplyr::case_when(
                        v520t1 == "1" ~ "base_machine_pos",
                        v520t1 == "2" ~ "crane_tip_when_felling",
                        v520t1 == "3" ~ "crane_tip_when_processing",
                        TRUE ~ NA_character_  ),
                      coordinate_reference_system = dplyr::case_when(
                        v521t2 == "1" ~ "WGS84", # Coordinate system used in stm file: 1=WGS84 (Default)
                        TRUE ~ NA_character_ ),
                      latitude_category = dplyr::case_when(
                        v523t2 == "1" ~ "North",
                        TRUE  ~ "South"),
                      longitude_category = dplyr::case_when(
                        v523t4 == "1" ~ "East",
                        TRUE ~ "West"),
                      latitude  = dplyr::case_when(
                        coordinate_reference_system ==  "WGS84" ~ latitude/ 100000,
                        TRUE ~ latitude),
                      longitude = dplyr::case_when(
                        coordinate_reference_system == "WGS84" ~ longitude / 100000,
                        TRUE ~ longitude)
        )
    }
    if ("v523t6" %in% stemvars){
      stemdat$coordinate_time = as.numeric(str_remove(stemdat$v523t6, pattern = "\n"))
    }

    if ("v273t1" %in% stemvars) {
      stemdat$stemdiav = stemdat$v273t1
    } else if ("v273t3" %in% stemvars) {
      stemdat$stemdiav = unlist(lapply(X = stemdat$v273t3, FUN = function(X){
        xv = as.numeric(unlist(stringr::str_split(X, " ")))
        xv2 = c(xv[1], xv[1]-cumsum(xv[2:length(xv)]))
        ret = paste0(xv2, collapse = ", ")
        return(ret)
      }))
    }

    stemdat <- dplyr::select(stemdat, -matches("tmp|v\\d", perl =T), matches("tmp|v\\d", perl =T))


    #Stem grade breaks

    stemgrades <- tibble::tibble(stemnr = rep(stemdat$stem_number,  stemdat$v274t1), # NUMGRADEBR
                                 height = unlist(stringr::str_split(paste0(stemdat$v275t1, collapse = " "), pattern = " ")),  # HGHTGRADBRK
                                 gradecode = unlist(stringr::str_split(paste0(stemdat$v276t1, collapse = " "), pattern = " "))) #GRADE code




    #  logs dataset ----

    logselector <- c( "v291t1", "v291t5",
                      "v292t1", "v292t5",
                      "v293t1", "v293t5",
                      "v294t2",
                      "v295t2",
                      "v296t1", "v296t2", "v296t3", "v296t4",
                      "v297t1", "v298t1",
                      "v299t1", "v299t2","v299t3",
                      "v300t1",
                      "v306t1","v306t2" )
    stmlogdat <- stemdat %>% dplyr::select(., which(names(stemdat) %in% logselector))
    logselector = logselector[which(logselector %in% names(stmlogdat))]

    logs  <- expand_str(tibbl = stmlogdat)
    logs$stem_key <- rep(stemdat$stem_key, stemdat$v290t1)
    logs$object_key <- rep(stemdat$object_key, stemdat$v290t1)
    logs$log_key <- sequence(stemdat$v290t1)

    logs <- logs %>%
      mutate(.,

             product_key = as.numeric(paste0(object_key, v296t1)),
             product_name = get0("v296t2"),
             assortment_code = get0("v296t3"),
             price_category_code = get0("v296t4"),
             m3price = get0("v299t1"),
             m3sub = get0("v299t2"),
             m3sob = get0("v299t3"),
             dia_top_ob = dplyr::coalesce(get0("v291t5"),get0("v291t1")),
             dia_top_ub = dplyr::coalesce(get0("v292t5"), get0("v292t5")),
             length_cm = dplyr::coalesce(get0("v293t5"),get0("v293t1")),
             length_class_cm = get0("v295t2"),
             dia_class_mm = get0("v294t2")
      ) %>%
      dplyr::select(.,
                    -tidyselect::matches("v\\d*"), tidyselect::matches("v\\d*")) %>%
      dplyr::select(., stem_key, log_key, product_key, tidyselect::starts_with("m3"), tidyselect::everything())

    Ret <- list(report_header = report_header, species_group_definition = species_group_definition,
                product_definition = product_definition, object_definition = object_definition, stems = stemdat, logs = logs)
    return(Ret)

  }
