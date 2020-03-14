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
read_stm_file <- function(filename){

  strng <- file2strng(filename)

  strng_to_v110_1 <- stringr::str_sub(string =  strng,
                             start = 1, end = stringr::str_locate(string = strng, pattern = "~110 1")[1,1] )

  vls <- sfclassic2list(strng_to_v110_1) #Should correspond to vls

  start_epoch = as.integer(lubridate::ymd_hms(vls$v16t4))

  ## Report header
  selector <- c("v1t2", "v3t1", "v3t2", "v3t5", "v3t6", "v3t8", "v6t1", "v12t4") #list of sfclassic vars we want
  report_header = populateselection(valuelist = vls, selector = selector)

  report_header <-
    rename(report_header,report_type = v1t2,
           creation_date = v12t4,
           country_code = v6t1,
           base_machine_number = v3t1,
           base_machine_id = v3t2,
           base_machine_manufacturer = v3t5,
           base_machine_model = v3t6,
           harvester_head_model = v3t8
    ) %>%
    mutate(., filename = str_extract(filename, pattern = "\\w*.stm"))


  ## Object definition
  selector <- c( "v16t4", "v21t1", "v21t2", "v21t3", "v21t4") #list of sfclassic vars we want
  object_definition = populateselection(valuelist = vls, selector = selector)

  object_definition <-
    object_definition %>% dplyr::mutate(., object_name = v21t1,
            object_user_id = v21t1,
            object_start_date = lubridate::ymd_hms(v16t4),
            object_key = start_epoch,
            sub_object_name = v21t2,
            sub_object_user_id = paste0(v21t2, v21t3, v21t4),
            sub_object_key = 0) %>%
    dplyr::select(., -matches("v\\d", perl =T)) %>% glimpse()



  # Species and Product definitions
  species_group_definition <-
    tibble::tibble(species_group_name = vls$v120t1,
               species_group_user_id = paste0(vls$v120t1, "#", vls$v120t3, "#", vls$v2t1),
               tmp_species_nr = 1:vls$v111t1,
               species_group_key = start_epoch + tmp_species_nr)


  # Help-table of product groups
  product_grp_species_nr <- rep(1:length(vls$v125t1), vls$v125t1)
  product_grp_code <- integer()
  for (i in 1:vls$v111t1) {
    product_grp_code = c(product_grp_code, 1:vls$v125t1[i])
  }
  product_grp_table <-
    tibble::tibble(product_grp_code,
               product_grp_species_nr,
               product_group_name = vls$v127t1)

  product_definition <-
    tibble::tibble(
      tmp_species_nr = rep(1:vls$v111t1, vls$v116t1),
      tmp_product_number = as.integer(1:length(vls$v121t1)),
      product_name = vls$v121t1,
      product_info = vls$v121t2,
      v126t1 = vls$v126t1,

      species_name = rep(vls$v120t1, vls$v116t1),
      v121t6 = ifelse(exists("vls$v121t6"),  as.integer(vls$v121t6), NA)
      # v121t6 is intended to work as a product_key in line with StanForD2010 terms.
      #   But we have not seen how it is implemented to log level data,
      #   therefore we do not use it as a product key yet.
    ) %>% dplyr::rowwise() %>%
    dplyr::mutate(.,
      species_group_key =  as.numeric(paste0(start_epoch, tmp_species_nr)),
      product_key = as.numeric(paste0(as.integer(start_epoch), tmp_product_number, collapse = ""))
    ) %>%
    dplyr::left_join(.,
              product_grp_table,
              by = c("tmp_species_nr" = "product_grp_species_nr",
                     "v126t1" = "product_grp_code")) %>%
    dplyr::select(., -tidyselect::starts_with("tmp")) %>%
    dplyr::select(., -v121t6) # for the moment v121t6 is dropped



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
stemdat <- sfclassic2df(loopstring[1])[NULL, ]
  for (i in seq_along(loopstring)){
    stemdat = dplyr::bind_rows(stemdat, sfclassic2df(loopstring[i]))
  }
stemdat$v110t <- dplyr::coalesce(stemdat$v110t1, stemdat$v110t2)
stemvars <- names(stemdat)

# stemdat <- stemdat[1:20, ]

## Modding the stemdat towards StanFord2010 terminology
# stem_key, stem_number
stemdat <- as.tibble(stemdat)
if ("v270t3" %in% stemvars) {
  stemdat$stem_number =  as.integer(stemdat$v270t3)
  stemdat$stem_key =   as.numeric(paste0(start_epoch, as.integer(stemdat$v270t3)))
} else if ("v270t1" %in% stemvars) {
  stemdat$stem_number =   as.integer(stemdat$v270t1)
  stemdat$stem_key =   as.numeric(paste0(start_epoch, as.integer(stemdat$v270t1)))
} else {
  stemdat$stem_number =   NA
  stemdat$stem_key =   NA_real_
}


if ("v523t1" %in% stemvars){
  stemdat$latitude = as.numeric(stemdat$v523t1)  # v523t1 COORD Latitude, registered according to var521_t1, var521_t2, var520_t1 and var523_t7
  stemdat$longitude = as.numeric(stemdat$v523t3)  # v523t3 COORD Longitude, registered according to var521_t1, var521_t2, var520_t1 and var523_t7. When var521_t1 = 1 this variable (var523_t3) is recorded as the difference from var522_t3.Variable excluded when no

  stemdat = stemdat %>% dplyr::mutate(.,
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


  if ("v523t6" %in% stemvars){
   stemdat$coordinate_time = as.numeric(str_remove(stemdat$v523t6, pattern = "\n"))
   } else { stemdat$coordinate_time = NaN
   }


} else {
stemdat = stemdat %>%
  dplyr::mutate(.,
         latitude = NaN,
         longitude = NaN,
         stem_coordinate_position = NA_character_,
         coordinate_reference_system  = NA_character_,
         latitude_category = NA_character_,
         longitude_category = NA_character_,
         coordinate_time = NaN
  )
}

 #  (sum(as.numeric(stemdat$v290t1)) )

if  ("v281t1" %in% stemvars) {
  stemdat$DBHmm = stemdat$v281t1
} else if ("v281t2" %in% stemvars) {
  stemdat$DBHmm = stemdat$v281t2
} else stemdat$DBHmm = NA_integer_

# stemdiameters
#diavector_from_diffdia <- function(ddv){# to convert from "diff dia vector" to diameter vector
#  xv = as.numeric(unlist(stringr::str_split(ddv, " ")))
#  xv2 = c(xv[1], xv[1]-cumsum(xv[2:length(xv)]))
#  ret = paste0(xv2, collapse = ", ")
#  return(ret)
#  }

if ("v273t1" %in% stemvars) {
  stemdat$stemdiav = stemdat$v273t1
} else if ("v273t3" %in% stemvars) {
  stemdat$stemdiav = unlist(lapply(X = stemdat$v273t3, FUN = function(X){
    xv = as.numeric(unlist(stringr::str_split(X, " ")))
    xv2 = c(xv[1], xv[1]-cumsum(xv[2:length(xv)]))
    ret = paste0(xv2, collapse = ", ")
    return(ret)
  } ))
}

stemdat = stemdat %>% dplyr::select(., -starts_with("v"), starts_with("v"))

#Stem grade breaks

stemgrades <- tibble::tibble(stemnr = rep(stemdat$stem_number,  stemdat$v274t1), # NUMGRADEBR
                        height = unlist(stringr::str_split(paste0(stemdat$v275t1, collapse = " "), pattern = " ")),  # HGHTGRADBRK
                        gradecode = unlist(stringr::str_split(paste0(stemdat$v276t1, collapse = " "), pattern = " "))) #GRADE code




# preparing logs dataset ----
logs <- tibble::tibble(
  stem_key = rep(stemdat$stem_key, as.numeric(stemdat$v290t1)),
  log_key = unlist(sapply(as.integer(stemdat$v290t1), FUN = function(x){1:x})),
  v296t1 = unlist(stringr::str_split(paste0(stemdat$v296t1, collapse = " "), " ")),  #PRICEMATR, registered price matrix per log; 1...var290t1. 0=Reject, 1... = price matrix number
  v296t2 = unlist(stringr::str_split(paste0(stemdat$v296t2, collapse = "\n"), "\n")),  #PRICEMATR,  Description of price matrix, i.e. "Skur", "Massevirke"
  v296t3 = unlist(stringr::str_split(paste0(stemdat$v296t3, collapse = "\n"), "\n")), #PRICEMATR, Assortment code (same code as in var121t2) /log: 1...var290t1
  v296t4 = unlist(stringr::str_split(paste0(stemdat$v296t4, collapse = " "), " ")), # PRICEMATR,  Type of price catergory per log (same codes as in var161t1): 1.var290_t1; 1 = price/m3 volume by small-end diameter; 2= price/m3 solid volume, 3=price/log 4=pris/m3 (norsk kategori) 5= pris/m3 (svensk topp-rot). 6=pris/m3f mittm?tt
  v297t1 = unlist(stringr::str_split(paste0(stemdat$v297t1, collapse = " "), " ")), # LOGGRADE
  v299t1 = unlist(stringr::str_split(paste0(stemdat$v299t1, collapse = " "), " ")), # Paid volume of logs as specified by var296_t4* : 1...var290_t1. 10^-4m3
  v299t2 = unlist(stringr::str_split(paste0(stemdat$v299t2, collapse = " "), " ")), # Solid volume of logs under bark
  v299t3 = unlist(stringr::str_split(paste0(stemdat$v299t3, collapse = " "), " ")), # Solid volume of logs on bark, measured by harvester, 10^-4m3sob
  v293t5 = unlist(stringr::str_split(paste0(stemdat$v293t5, collapse = " "), " ")), # Length of logs, (measured by machine, M1): 1...var290_t1
  v291t5 = unlist(stringr::str_split(paste0(stemdat$v291t5, collapse = " "), " ")), # Top diameter of logs on bark (measured by machine, M1): 1...var290_t1
  v292t5 = unlist(stringr::str_split(paste0(stemdat$v292t5, collapse = " "), " ")), # Top diameter of logs under bark (measured by machine, M1): 1...var290_t1



  ) %>% rowwise() %>%
  dplyr::mutate(.,
    product_key = paste0(start_epoch, v296t1, collapse = ""),
    m3sob = v299t3,
    m3sub = v299t2,
    m3price = v299t1,
    length = v293t5,
    dia_top_ob = v291t5,
    dia_top_ub = v292t5
    ) %>%
  dplyr::select(., stem_key, log_key, product_key, starts_with("m3"), everything()) %>%
  dplyr::select(., -starts_with("v"), starts_with("v"))






   Ret <- list(report_header = report_header, species_group_definition = species_group_definition,
              product_definition = product_definition, object_definition = object_definition, stems = stemdat, logs = logs)
  return(Ret)

}
