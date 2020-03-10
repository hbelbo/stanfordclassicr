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
#'
#' The list aim to immitate the structure and naming conventions in the successor standard StanForD2010.
#' Along with the data is also all stanford classic variable tags belonging to eahc stem, each
#' log, each cut object etc. This is to enable further mapping of stanford classic
#' variable tags to readable variable names.
#'
#' @section Tips:
#' Example files and documentation of the stanford classic standard (pdf)
#' is provided in the supplementary directory, please have a look
#'
#' @examples
#' files = list.files(system.file("extdata", package = "stanfordclassicr"), full.names = T)
#' files # to see all files and where they are on your computer
#' read_ktr_file(files[str_detect(files, ".ktr")])
#' read_stm_file(files[str_detect(files, ".stm")])
#' read_pri_file(files[str_detect(files, ".pri")])
#'
#' @section Further development
#' The ambitions is to first improve the mapping from tags and codes to
#' reasonable variable names in current functions
#'
#' Then expand to include .drt, .apt, .prl and .prd files.
#'
#' "Stanford homepage": https://www.skogforsk.se/english/projects/stanford/
#' @docType package
#' @name stanfordclassicr
NULL
