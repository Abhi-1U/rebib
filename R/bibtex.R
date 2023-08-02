
#' @title bibtex reader
#'
#' @param article_dir path to the directory which contains tex article
#' @return list of BibTex references
#' @noRd
bibtex_reader <- function(article_dir) {
    # get the name of the bibtex file
    tex_file <- get_texfile_name(article_dir)
    bib_file <- get_bib_file(article_dir,tex_file)
    bib_file_path <- paste(article_dir, bib_file, sep = "/")
    if (is.null(bib_file)) {
        return(FALSE)
    }
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
#' dir.create(your_article_folder <- file.path(tempdir(), "exampledir"))
#' example_files <-  system.file("article", package = "rebib")
#' x <- file.copy(from = example_files,to=your_article_folder,recursive = TRUE)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' bib_path <- paste0(your_article_path,"/example.bib")
#' rebib::handle_bibliography(your_article_path)
#' references <- rebib::split_bibtex_references(bib_path)
#' references
#' unlink(your_article_folder,recursive = TRUE)
split_bibtex_references <- function(bib_path) {
    bib_references <- list()
    bib_types <- list()
    bib_names <- list()
    raw_lines <- readLines(bib_path)
    bib_breakpoints <- which(grepl("^@\\s*", raw_lines))
    bib_endpoints <- list()
    for (iterator in seq_along(bib_breakpoints)){
        if (iterator == length(bib_breakpoints)){
            bib_endpoints[iterator] <- bib_breakpoints[iterator] - 1 +
                which(grepl("\\s*,",raw_lines[bib_breakpoints[iterator]:length(raw_lines)]))[1]
        }
        else{
            bib_endpoints[iterator] <- bib_breakpoints[iterator] - 1 +
                which(grepl("\\s*,",raw_lines[bib_breakpoints[iterator]:bib_breakpoints[iterator+1]]))[1]
        }
    }
    bib_endpoints <- unlist(bib_endpoints)
    for (iterator in seq_along(bib_breakpoints)) {
        start_pos <- bib_breakpoints[iterator]
        end_pos <- bib_endpoints[iterator]
        current_line <-  raw_lines[start_pos:end_pos]
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
#' ref_type
get_reference_type <- function(bib_reference) {
    patt <- "\\@\\s*(.*?)\\s*\\{"
    bib_reference <-paste(unlist(bib_reference),collapse="")
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
#' ref_name
get_reference_name <- function(bib_reference) {
    patt <- "\\{\\s*(.*?)\\s*\\,"
    bib_reference <-paste(unlist(bib_reference),collapse="")
    ref_name <- stringr::str_extract(bib_reference, patt)
    ref_name <- gsub("\\{", "", ref_name)
    ref_name <- gsub("\\,", "", ref_name)
    ref_name <- paste0("{", ref_name, "}")
    return(ref_name)
}
