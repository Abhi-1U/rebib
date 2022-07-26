
#' @title bibtex reader
#'
#' @param article_dir path to the directory which contains tex article
#' @return list of BibTex references
#' @export
bibtex_reader <- function(article_dir) {
    # get the name of the bibtex file
    tex_file <- get_texfile_name(article_dir)
    bib_file <- get_bib_file(article_dir,tex_file)
    bib_file_path <- paste(article_dir, bib_file, sep = "/")
    bib_tex_references <- split_bibtex_references(bib_file_path)

    return(bib_tex_references)
}

#' @title split BibTex references
#'
#' @param bib_path path to the bibtex file to be read
#'
#' @return list of references separated as types and names based on indices
#' @export
#'
#' @examples
#' wd <-  system.file("article", package = "rebib")
#' rebib::handle_bibliography(wd)
#' bib_path <- paste(wd, "example.bib", sep = "/")
#' references <- rebib::split_bibtex_references(bib_path)
#' print(references)
split_bibtex_references <- function(bib_path) {
    bib_references <- list()
    bib_types <- list()
    bib_names <- list()
    raw_lines <- readLines(bib_path)
    bib_breakpoints <- which(grepl("^@\\s*", raw_lines))
    for (iterator in seq_along(bib_breakpoints)) {
        start_pos <- bib_breakpoints[iterator]
        current_line <-  raw_lines[start_pos]
        bib_types[iterator] <- get_reference_type(current_line)
        bib_names[iterator] <- get_reference_name(current_line)
    }
    bib_references$types <- unlist(bib_types)
    bib_references$names <- unlist(bib_names)
    return(bib_references)
}

#' @title get reference type
#'
#' @param bib_reference first line containing the cite reference
#'
#' @return reference type (str)
#' @export
#'
#' @examples
#' ref_first_line <- "@book{ihaka:1996,"
#' ref_type <- rebib::get_reference_type(ref_first_line)
#' print(ref_type)
get_reference_type <- function(bib_reference) {
    patt <- "\\@\\s*(.*?)\\s*\\{"
    ref_type <- stringr::str_extract(bib_reference, patt)
    ref_type <- gsub("^@", "", ref_type)
    ref_type <- gsub("\\{", "", ref_type)
    return(tolower(ref_type))
}

#' @title get reference name
#'
#' @param bib_reference first line containing the cite reference
#'
#' @return reference name (str)
#' @export
#'
#' @examples
#' ref_first_line <- "@book{ihaka:1996,"
#' ref_name <- rebib::get_reference_name(ref_first_line)
#' print(ref_name)
get_reference_name <- function(bib_reference) {
    patt <- "\\{\\s*(.*?)\\s*\\,"
    ref_name <- stringr::str_extract(bib_reference, patt)
    ref_name <- gsub("\\{", "", ref_name)
    ref_name <- gsub("\\,", "", ref_name)
    ref_name <- paste0("{", ref_name, "}")
    return(ref_name)
}
