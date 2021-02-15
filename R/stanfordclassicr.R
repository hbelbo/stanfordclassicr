#'  Reading forest machine files using stanford classic standard
#'
#' The package provide one function for each type of stanford standard file
#'  and some supplementary functions for internal use in the package.
#'
#'
#'
#' @section Main functions:
#'
#' The main functions takes one file path as argument, and organize data in a list of data frames (tibbles).
#' The list aim to immitate the structure and naming conventions in the successor standard StanForD2010.
#' Along with the data is also all stanford classic variable tags belonging to eahc stem, each
#' log, each cut object etc. This to enable further mapping of stanford classic
#' variable tags to readable variable names.
#'
#' @section Tips:
#' Example files and documentation of the stanford classic standard (pdf)
#' is provided in the supplementary directory, please have a look
#'
#' @examples
#' files = list.files(system.file("extdata", package = "stanfordclassicr"), full.names = TRUE)
#' read_ktr_file(files[stringr::str_detect(files, ".ktr")][1])
#' read_stm_file(files[stringr::str_detect(files, ".stm")][1])
#' read_pri_file(files[stringr::str_detect(files, ".pri")][1])
#'
#' @section Further development:
#' The ambitions is to first improve the mapping from tags and codes to
#' reasonable variable names in current functions
#'
#' Then expand to include .drf, .apt, .prl and .prd files.
#'
#' "Stanford homepage": https://www.skogforsk.se/english/projects/stanford/
#' @docType package
#' @name stanfordclassicr
NULL
