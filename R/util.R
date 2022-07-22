#' function to solve bibliography problems
#'
#' if bibliography exists in bibtex format then (filename.bib) bibtex file will
#' be preferred.
#' else this function will generate a minimal bibliography
#' @param article_dir path to the directory which contains tex article
#' @param override_mode force use parser and ignore BibTeX bibliography.
#' @export bibliography links the bibtex file with latex source code or
#' generates a minimal bibtex file from embedded bibliography and links that
#' file to the latex file
#'
#' @examples
#' wd <-  system.file("article", package = "rebib")
#' rebib::handle_bibliography(wd)
#' cat(readLines(paste(wd,"example.bib",sep="/")),sep = "\n")
handle_bibliography <- function(article_dir, override_mode = FALSE) {
    # checking for RJwrapper and fetching the file name for tex file
    old_wd <- getwd()
    setwd(article_dir)
    file_name <- get_texfile_name(article_dir)
    bib_file <- get_bib_file(article_dir, file_name)
    if (! identical(bib_file, "") && (! override_mode)) {
        link_bibliography_line(article_dir, file_name)
    } else {
        print("will need to convert bbl to .bib")
        bib_items <- extract_embeded_bib_items(article_dir, file_name)
        bibtex_data <- bib_handler(bib_items)
        make_bibtex_file(bibtex_data, file_name)
        link_bibliography_line(article_dir, file_name)
    }
    on.exit(setwd(old_wd), add = TRUE)
}

#' writes bibtex data in a structured format to the .bib file
#'
#' @param bibtex_data a list of minimal bibtex data
#' @param file_name name of the tex file
#'
#' @return
#' @export bibtex_file a bibtex file is generated
#'
#' @examples
make_bibtex_file <-function(bibtex_data,file_name) {
    bib_file_name <- gsub(".tex", ".bib", file_name)
    for (iterator in seq_along(bibtex_data[["book"]])){
        unique_id <- bibtex_data[["book"]][[iterator]]$unique_id
        #print(unique_id)
        author <- bibtex_data[["book"]][[iterator]]$author
        title <- bibtex_data[["book"]][[iterator]]$title
        journal <- bibtex_data[["book"]][[iterator]]$journal
        #print(author)
        #print(title)
        line1 <- sprintf("@book{ %s,", unique_id)
        line2 <- sprintf("author = %s,", author)
        line3 <- sprintf("title = %s,", title)
        line5 <- sprintf("}")
        write_external_file(bib_file_name, "a", toString(line1))
        write_external_file(bib_file_name, "a", toString(line2))
        write_external_file(bib_file_name, "a", toString(line3))
        if (!identical(bibtex_data[["book"]][[iterator]]$URL,NULL)) {
            # at the end of the day publisher/journal field produce similar
            # citation expression
            line4 <- sprintf("publisher = %s,", journal)
            write_external_file(bib_file_name, "a", toString(line4))
            url <- bibtex_data[["book"]][[iterator]]$URL
            line_url <- sprintf("url = {{ %s }}", url)
            write_external_file(bib_file_name, "a", toString(line_url))
        } else {
            # at the end of the day publisher/journal field produce similar
            # citation expression
            line4 <- sprintf("publisher = %s", journal)
            write_external_file(bib_file_name, "a", toString(line4))
        }
        write_external_file(bib_file_name, "a", toString(line5))
    }
}



#' applies minimal bibliography over bib entries generated
#'
#' which are extracted by the extraction function.
#' @param bib_items bib entries extracted from extraction function
#'
#' @return bbl_record nested list
#' @export
#'
#' @examples
bib_handler <- function(bib_items) {
    bbl_record <- list()
    # applies minimal bibliography to all the items
    bbl_record$book <- lapply(bib_items, function(entry) {
        bib_content <- bibliography_parser(entry)
        book <- list(
            unique_id = bib_content$unique_id,
            author = bib_content$author,
            title = bib_content$title,
            journal = bib_content$journal,
            URL = bib_content$URL)
        book
    }
    )
    return(bbl_record)
}



#' export embedded bibliography to a bbl file
#' @description
#' This function will extract the embedded bibliography and store it in .bbl
#' file
#'
#' @param article_dir path to the directory which contains tex article
#' @param file_name name of the tex file
#'
#' @return
#' @export bbl_file
#'
#' @examples
export_embeded_bibliography <- function(article_dir, file_name) {
    src_file_data <- readLines(file.path(article_dir, file_name))
    bbl_start <- which(grepl("^\\s*\\\\begin\\{thebibliography\\}",
                             src_file_data))
    bbl_end <- which(grepl("^\\s*\\\\end\\{thebibliography\\}",
                           src_file_data))
    bbl_data <- src_file_data[bbl_start:bbl_end]
    bbl_file_name <- gsub(".tex", ".bbl", file_name)
    write_external_file(bbl_file_name, "w", bbl_data)
}

#' extract the bibliography in chunks seperated at bibitem
#'
#' @description intended to be an internal function which is used with other
#'  functions in flow.
#' @param article_dir path to the directory which contains tex article
#' @param file_name name of the file
#'
#' @return a list of bib entries separated at bibitem
#' @export
#'
#' @examples
extract_embeded_bib_items <- function(article_dir, file_name){
    src_file_data <- readLines(file.path(article_dir, file_name))
    bbl_start <- which(grepl("^\\s*\\\\begin\\{thebibliography\\}",
                             src_file_data))
    bbl_end <- which(grepl("^\\s*\\\\end\\{thebibliography\\}", src_file_data))
    bbl_data <- src_file_data[bbl_start:bbl_end]
    ##
    bib_ignore <- which(grepl("^\\%%", bbl_data))
    bib_breakpoints <- which(grepl("^\\s*\\\\bibitem\\[", bbl_data))
    bib_items <- list()
    # creating chunks of bibliography entries
    for (i in 1:(length(bib_breakpoints) - 1)) {
        bib_items[length(bib_items)+1] <- list(bbl_data[(bib_breakpoints[i]):(bib_breakpoints[(i+1)]-1)])
        if (i == (length(bib_breakpoints) - 1)) {
            bib_items[length(bib_items)+1] <- list(bbl_data[(bib_breakpoints[i+1]+1):length(bbl_data)-1])
        }
    }
    return(bib_items)
}

#' append the tex file with a line to link bibliography
#'
#' @param article_dir path to the directory which contains tex article
#' @param file_name file name of the tex document
#'
#' @return
#' @export appends the tex file with a line to link bibliography
#'
#' @examples
link_bibliography_line <- function(article_dir, file_name) {
    src_file_data <- readLines(file.path(article_dir, file_name))
    bib_exist <- FALSE
    for (line in src_file_data) {
        if (grepl("^\\\\bibliography", line)) {
            bib_exist <- TRUE
            break
        }
    }
    if (bib_exist) {
        print("\\bibliography{bib_file} exists!")
        return("")
    } else {
        bib_line <- paste("\\bibliography{",
                          toString(tools::file_path_sans_ext(file_name)), "}",
                          sep = "")
    }
    # Backup original wrapper file
    backup_file <- paste(file_name,".bk",sep="")
    write_external_file(backup_file, "w", src_file_data)
    # write to original wrapper file
    write_external_file(file_name, "a", bib_line)
}
