#' function to solve bibliography problems
#'
#' if bibliography exists in bibtex format then (filename.bib) bibtex file will
#' be preferred.
#' else this function will generate a minimal bibliography
#' @param article_dir path to the directory which contains tex article
#' @param override_mode force use parser and ignore BibTeX bibliography.
#' @returns  bibliography links the bibtex file with latex source code or
#' generates a minimal bibtex file from embedded bibliography and links that
#' file to the latex file
#' @export
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
        bibtex_writer(bibtex_data, file_name)
        link_bibliography_line(article_dir, file_name)
    }
    on.exit(setwd(old_wd), add = TRUE)
}

#' @title bibtex writer
#' writes bibtex data in a structured format to the .bib file
#'
#' @param bibtex_data a list of minimal bibtex data
#' @param file_name name of the tex file
#'
#' @return a bibtex file is written
#' @export
bibtex_writer <- function(bibtex_data, file_name) {
    bib_file_name <- gsub(".tex", ".bib", file_name)
    for (iterator in seq_along(bibtex_data[["book"]])) {
        # optional param
        include_year <- FALSE
        include_url <- FALSE
        include_isbn <- FALSE
        include_journal <- FALSE

        # line names
        line_year <- ""
        line_isbn <- ""
        line_url <- ""
        line_journal <- ""

        # unique id of reference
        unique_id <- bibtex_data[["book"]][[iterator]]$unique_id
        line_uid <- sprintf("@book{%s,", unique_id)

        # author field
        author <- bibtex_data[["book"]][[iterator]]$author
        line_author <- sprintf("author = %s,", author)

        # title field
        title <- bibtex_data[["book"]][[iterator]]$title
        line_title <- sprintf("title = %s", title)


        # year field (optional)
        if (!identical(bibtex_data[["book"]][[iterator]]$year,NULL)){
            year <- bibtex_data[["book"]][[iterator]]$year
            line_year <- sprintf("year = {%s}", year)
            include_year <- TRUE
        }

        # URL field (optional)
        if (!identical(bibtex_data[["book"]][[iterator]]$URL,NULL)) {
            url <- bibtex_data[["book"]][[iterator]]$URL
            line_url <- sprintf("url = {%s}", url)
            include_url <- TRUE
        }

        # isbn field (optional)
        if (!identical(bibtex_data[["book"]][[iterator]]$isbn,NULL)) {
            url <- bibtex_data[["book"]][[iterator]]$isbn
            line_isbn <- sprintf("isbn = {%s}", url)
            include_isbn <- TRUE
        }
        # journal/publisher/misc data

        # journal field (optional)
        if (!identical(bibtex_data[["book"]][[iterator]]$journal,NULL)){
            journal <- bibtex_data[["book"]][[iterator]]$journal
            line_journal <- sprintf("publisher = {%s}", journal)
            include_journal <- TRUE
        }

        # ending_line
        line_end <- sprintf("}")

        #write queue
        write_external_file(bib_file_name, "a", toString(line_uid))
        write_external_file(bib_file_name, "a", toString(line_author))
        # if title is the second last line to be written
        if (include_journal | include_url | include_isbn | include_year ) {
            line_title <- paste(line_title,",",sep="")
            write_external_file(bib_file_name, "a", toString(line_title))
        } else {
            write_external_file(bib_file_name, "a", toString(line_title))
        }

        # if journal/publisher details are the second last line to be written
        if (include_journal & ( include_isbn | include_url | include_year)) {
            line_journal <- paste(line_journal,",",sep="")
            write_external_file(bib_file_name, "a", toString(line_journal))
        }
        if (include_journal & ( !include_isbn & !include_url & !include_year)) {
            write_external_file(bib_file_name, "a", toString(line_journal))
        } else {
            #skip
        }

        # if year is the second last line
        if (include_year & (include_url | include_isbn)) {
            line_year <- paste(line_year,",",sep="")
            write_external_file(bib_file_name, "a", toString(line_year))
        }
        if (include_year & (!include_isbn & !include_url)) {
            write_external_file(bib_file_name, "a", toString(line_year))
        } else {
            #skip
        }

        # if url is the second last line
        if (include_url & include_isbn) {
            line_url <- paste(line_url,",",sep="")
            write_external_file(bib_file_name, "a", toString(line_url))
        }
        if (include_url & !include_isbn) {
            write_external_file(bib_file_name, "a", toString(line_url))
        } else {
            #skip
        }

        if (include_isbn) {
            write_external_file(bib_file_name, "a", toString(line_isbn))
        }

        write_external_file(bib_file_name, "a", toString(line_end))

    }
}


#' applies minimal bibliography over bib entries generated
#'
#' which are extracted by the extraction function.
#' @param bib_items bib entries extracted from extraction function
#'
#' @return bbl_record nested list
#' @export
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
            year = bib_content$year,
            URL = bib_content$URL,
            isbn = bib_content$isbn)
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
#' @return bbl_file
#' @export
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
extract_embeded_bib_items <- function(article_dir = "", file_name = "", file_path = ""){
    if( identical(file_path,"")) {
        src_file_data <- readLines(file.path(article_dir, file_name))
    } else {
        # absolute path
        src_file_data <- readLines(file_path)
    }
    bbl_start <- which(grepl("^\\s*\\\\begin\\{thebibliography\\}",
                             src_file_data))
    bbl_end <- which(grepl("^\\s*\\\\end\\{thebibliography\\}", src_file_data))
    bbl_data <- src_file_data[bbl_start:bbl_end]
    ## ignore comments
    bbl_data <- filter_bbl_data(bbl_data)
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
#' @return appends the tex file with a line to link bibliography
#' @export
#'
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

#' @title comment filter for bbl data
#'
#' @description
#' removes commented latex lines to avoid wrong reference data
#' @param bbl_data blocks of bib_data
#'
#' @return filtered bbl_data
#' @export
filter_bbl_data <- function(bbl_data) {
    comment_break_points <- which(grepl("^%%", bbl_data))
    for (pos in comment_break_points) {
        bbl_data[pos] <- ""
    }
    comment_break_points_spaced <- which(grepl("%%", bbl_data))
    for (pos in comment_break_points_spaced) {
        bbl_data[pos] <- ""
    }
    return(bbl_data[nzchar(bbl_data)])
}

#' @title biblio convertor
#' @description a quick convertor for bbl/tex to bib
#' @param file_path provide a file_path with file name to point tex/bbl file
#'
#' @return bib file
#' @export
#'
biblio_convertor <- function(file_path = "") {
    bib_file_path <- toString(paste(tools::file_path_sans_ext(file_path),
                                    ".bib", sep = ""))
    bib_items <- extract_embeded_bib_items(file_path = file_path)
    bibtex_data <- bib_handler(bib_items)
    bibtex_writer(bibtex_data, bib_file_path)
}

#' bbl_file <-  system.file("article/sample.bbl", package = "rebib")
#' rebib::biblio_convertor(file_path = bbl_file)
#' cat(readLines(gsub("bbl","bib",bbl_file)),sep = "\n")
