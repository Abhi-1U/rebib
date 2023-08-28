#' @title get tex file name
#' @description
#' Get the name of the tex file included within wrapper file
#'
#'The wrapper file refers to an external tex file which contains
#'the actual document content.
#' @param article_dir path to the directory which contains tex article
#' @keywords internal
#' @return name of the tex-file (Str)
#' @noRd
get_texfile_name <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    lookup_file <- get_wrapper_type(article_dir)
    wrapper_file <- readLines(file.path(article_dir, lookup_file))
    wrapper_file <- stringr::str_subset(wrapper_file, ".+")
    article_start <- which(grepl(
        "^\\s*\\\\begin\\{article\\}",
        wrapper_file))
    pre_marker <- wrapper_file[seq_len(article_start)]
    post_marker <- wrapper_file[seq_len(article_start) + 1]
    source_line <- setdiff(post_marker, pre_marker)
    tex_file <- gsub("[[:space:]]", "",
                     gsub("\\\\input\\{|\\}", "", source_line))
    if (!grepl(".tex$", tex_file)) {
        tex_file <- paste0(tex_file, ".tex")
    }
    return(tex_file)
}

#' @title get bibtex file name
#' @description
#' finds the bib file in directory which is referenced in the article
#'
#' @param article_dir path to the directory which contains tex article
#' @param file_name name of the tex file
#' @keywords internal
#' @return name of bib file (character)
#' @noRd
get_bib_file <- function(article_dir, file_name) {
    article_dir <- xfun::normalize_path(article_dir)
    file_list <- list.files(article_dir, recursive = FALSE)
    extensions <- c("*.bib$")
    linked_bib <- toString(paste(tools::file_path_sans_ext(file_name),
                                 ".bib", sep = ""))
    bib_file <- unique(grep(paste(extensions, collapse = "|"),
                            file_list, value = TRUE))
    if (identical(bib_file, character(0))) {
        message("No Bib files found !")
        return("")
    }
    if (identical(class(bib_file), "character") &&
        identical(linked_bib, bib_file)) {
        message(paste("Found Bib file ", bib_file))
        return(bib_file)
    } else {
        for (file in bib_file) {
            if (identical(file, linked_bib)) {
                message(paste("Found Bib file ", bib_file))
                return(file)
            }
        }
        return("")
    }
}

#' @title write lines to external file
#' @description
#' quick function to writelines to a file
#'
#' @param file_name name of text file to write contents to
#' @param mode mode of opening
#' @param raw_text the text/ list of lines to be written
#' @keywords internal
#' @return create/append/write a new file
#' @noRd
write_external_file <- function(file_name, mode, raw_text) {
    file_name <- xfun::normalize_path(file_name)
    write_file <- file(file_name, mode)
    writeLines(raw_text, write_file)
    close(write_file)
}



#' @title split string
#' @description
#' a wrapper for stringr::str_split
#' @param x object (Str)
#' @param patt pattern (Regex Str)
#' @keywords internal
#' @return list of vectorized string elements
#' @noRd
str_split <- function(x, patt) {
    return(stringr::str_split(x, patt))
}

#' @title extract substring
#' @description
#' a wrapper for stringr::str_extract
#' @param x object (Str)
#' @param patt pattern (Regex Str)
#' @keywords internal
#' @return list of extracted string elements
#' @noRd
str_extract <- function(x, patt) {
    return(stringr::str_extract(x, patt))
}
