#' Read StanForD Classic prd-files (machine reports from forest machines)
#'
#' @param filename
#'
#' @return should return a list of tables populated with data from the prd report:
#'        report_header, object_definition, species_definition, product_definitions, volume_summary, logs_summary
#' @export
#'
#' @examples
#'  files <- list.files(system.file("extdata", package = "stanfordclassicr"), full.names = T)
#'  prdfiles <- files[stringr::str_detect(files, ".prd")]
#'  read_prd_file(prdfiles[1])
read_prd_file <- function(filename){
  #  filename <- prdfiles[1]
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
                  filename = str_extract(filename, pattern = "\\w*.prd")) %>%
    dplyr::select(., -tidyselect::all_of(selector))



  ## Object definition
  selector <- c( "v16t4", "v17t4", "v21t1", "v21t2", "v21t3", "v21t4",
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
                  object_end_date = lubridate::ymd_hms(get0("v17t4", ifnotfound = NA_character_)),
                  object_key = get0("start_epoch", ifnotfound = NA_integer_),
                  sub_object_name = get0("v21t2", ifnotfound = NA_character_),
                  sub_object_key = 0,
                  contract_nr = dplyr::coalesce(get0("v35t2"), get0("v35t1")))

  object_definition$sub_object_user_id <-
    varvals2one(stanford.tibbl = df1,
                vars2use =  c(  "v21t2", "v21t3", "v21t4"))
  object_definition$logging_org <-
    varvals2one(stanford.tibbl = df1,
                 vars2use = c("v31t1", "v31t2", "v31t3", "v31t4", "v31t5"))
  object_definition$contractor <-
    varvals2one(stanford.tibbl = df1,
                 vars2use = c("v34t2","v34t3","v34t4","v34t5"))
  object_definition$forestowner <-
    varvals2one(stanford.tibbl = df1,
                 vars2use = c("v33t1","v33t2","v33t3","v33t4","v33t5"))  %>%
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





  Ret <- list(report_header = report_header,
              object_definition = object_definition,
              species_group_definition = species_group_definition,
              product_definition = product_definition,
              product_grp_table = product_grp_table,
              present_vars = names(df1)

              )
  return(Ret)

}







