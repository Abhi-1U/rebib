#' function to solve bibliography problems
#'
#' if bibliography exists in bibtex format then (filename.bib) bibtex file will
#' be preferred.
#' else this function will generate a minimal bibliography
#' @param article_dir path to the directory which contains tex article
#' @param override_mode force use parser and ignore BibTeX bibliography.
#' @param log_rebib option to enable log files for rebib
#' @returns  bibliography links the bibtex file with latex source code or
#' generates a minimal bibtex file from embedded bibliography and links that
#' file to the latex file
#' @export
#' @examples
#' dir.create(your_article_folder <- file.path(tempdir(), "exampledir"))
#' example_files <-  system.file("article", package = "rebib")
#' x <- file.copy(from = example_files,to=your_article_folder,recursive = TRUE)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' rebib::handle_bibliography(your_article_path)
#' unlink(your_article_folder,recursive = TRUE)
handle_bibliography <- function(article_dir, override_mode = FALSE, log_rebib = FALSE) {
    # checking for RJwrapper and fetching the file name for tex file
    old_wd <- getwd()
    article_dir <- xfun::normalize_path(article_dir)
    setwd(article_dir)
    on.exit(setwd(old_wd), add = TRUE)
    date <- Sys.Date()
    file_name <- get_texfile_name(article_dir)
    if (log_rebib) {
        log_file <- paste0("rebib-log-",date,".log")
        log_setup(article_dir, log_file, 2)
        rebib_log(paste0("working directory : ", article_dir), "info", 2)
        rebib_log(paste0("file name : ", file_name), "info", 2)
    }

    bib_file <- get_bib_file(article_dir, file_name)
    if (!identical(bib_file, "") && (!override_mode)) {
        if (log_rebib) {
            rebib_log("BibTeX file exists", "info", 2)
            rebib_log("Wont parse for bibliography", "info", 2)
        }
        else {
            message("BibTeX file exists")
            message("Wont parse for bibliography")
        }
        link_bibliography_line(article_dir, file_name)
    } else {
        if (log_rebib) {
            rebib_log("BibTeX file does not exist", "info", 2)
            rebib_log("will parse for bibliography", "info", 2)
        }
        else{
            message("BibTeX file does not exist")
            message("will parse for bibliography")
        }
        bib_items <- extract_embeded_bib_items(article_dir, file_name)
        bibtex_data <- bib_handler(bib_items)
        bibtex_writer(bibtex_data, file_name)
        link_bibliography_line(article_dir, file_name)
        if (log_rebib) {
            rebib_log(bibtex_data, "debug", 2)
            rebib_log("bibtex file created", "info", 2)
        }
        else{
            message("bibtex file created")
        }

    }

}

#' @title bibtex writer
#' writes bibtex data in a structured format to the .bib file
#'
#' @param bibtex_data a list of minimal bibtex data
#' @param file_name name or path of the tex file
#' @keywords internal
#' @return a bibtex file is written
#' @noRd
bibtex_writer <- function(bibtex_data, file_name) {
    bib_file_name <- xfun::with_ext(file_name,"bib")
    for (iterator in seq_along(bibtex_data[["book"]])) {
        # optional param
        include_pages <- FALSE
        include_year <- FALSE
        include_url <- FALSE
        include_isbn <- FALSE
        include_journal <- FALSE

        # line names
        line_pages <- ""
        line_year <- ""
        line_isbn <- ""
        line_url <- ""
        line_journal <- ""

        # unique id of reference
        unique_id <- bibtex_data[["book"]][[iterator]]$unique_id
        line_uid <- sprintf("@book{%s,", unique_id)

        # author field
        author <- bibtex_data[["book"]][[iterator]]$author
        line_author <- paste0("author = {",gsub("([^a-z]*)([A-Z])([^.].*)",
                           "\\1{\\2\\3}", author),"},")

        # title field
        title <- bibtex_data[["book"]][[iterator]]$title
        if (identical(title,NULL)) {
            title <- ""
        }
        line_title <- sprintf("title = {{%s}}", title)

        # pages field (optional)
        if (!identical(bibtex_data[["book"]][[iterator]]$pages,NULL)) {
            pages <- bibtex_data[["book"]][[iterator]]$pages
            line_pages <- sprintf("pages = {%s}", pages)
            include_pages <- TRUE
        }
        # year field (optional)
        if (!identical(bibtex_data[["book"]][[iterator]]$year,NULL)) {
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
        if (!identical(bibtex_data[["book"]][[iterator]]$journal,NULL)) {
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
        if (include_journal | include_url | include_isbn | include_year | include_pages) {
            line_title <- paste(line_title,",",sep = "")
            write_external_file(bib_file_name, "a", toString(line_title))
        } else {
            write_external_file(bib_file_name, "a", toString(line_title))
        }

        # if journal/publisher details are the second last line to be written
        if (include_journal & ( include_isbn | include_url | include_year | include_pages)) {
            line_journal <- paste(line_journal,",",sep = "")
            write_external_file(bib_file_name, "a", toString(line_journal))
        }
        if (include_journal & ( !include_isbn & !include_url & !include_year & !include_pages)) {
            write_external_file(bib_file_name, "a", toString(line_journal))
        } else {
            #skip
        }
        # if pages is the second last line
        if (include_pages & (include_url | include_isbn | include_year)) {
            line_pages <- paste(line_pages,",",sep = "")
            write_external_file(bib_file_name, "a", toString(line_pages))
        }
        if (include_year & (!include_isbn & !include_url & !include_year)) {
            write_external_file(bib_file_name, "a", toString(line_pages))
        } else {
            #skip
        }
        # if year is the second last line
        if (include_year & (include_url | include_isbn)) {
            line_year <- paste(line_year,",",sep = "")
            write_external_file(bib_file_name, "a", toString(line_year))
        }
        if (include_year & (!include_isbn & !include_url)) {
            write_external_file(bib_file_name, "a", toString(line_year))
        } else {
            #skip
        }

        # if url is the second last line
        if (include_url & include_isbn) {
            line_url <- paste(line_url,",",sep = "")
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
    if (length(bibtex_data) != 0) {
        write_external_file(bib_file_name, "a", toString(""))
    }
}


#' applies minimal bibliography over bib entries generated
#'
#' which are extracted by the extraction function.
#' @param bib_items bib entries extracted from extraction function
#' @keywords internal
#' @return bbl_record nested list
#' @noRd
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
            isbn = bib_content$isbn,
            pages = bib_content$pages)
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
#' @keywords internal
#' @return bbl_file
#' @noRd
export_embeded_bibliography <- function(article_dir, file_name) {
    src_file_data <- readLines(file.path(article_dir, file_name))
    bbl_start <- which(grepl("^\\s*\\\\begin\\{thebibliography\\}",
                             src_file_data))
    bbl_end <- which(grepl("^\\s*\\\\end\\{thebibliography\\}",
                           src_file_data))
    bbl_data <- src_file_data[bbl_start:bbl_end]
    bbl_file_name <- xfun::with_ext(file_name, "bbl")
    write_external_file(bbl_file_name, "w", bbl_data)
}

#' extract the bibliography in chunks seperated at bibitem
#'
#' @description intended to be an internal function which is used with other
#'  functions in flow.
#' @param article_dir path to the directory which contains tex article
#' @param file_name name of the file
#' @param file_path absolute path of file with file_name
#' @keywords internal
#' @return a list of bib entries separated at bibitem
#' @noRd
extract_embeded_bib_items <- function(article_dir = "", file_name = "", file_path = ""){
    if ( identical(file_path,"")) {
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
    # In case of no Bib-BreakPoints
    if (length(bib_breakpoints) == 0) {
        return(bib_items)
    }
    singular_case <- 0
    # creating chunks of bibliography entries
    final <-  length(bib_breakpoints) - 1
    if (length(bib_breakpoints) == 1) {
        final <- 1
        singular_case <- 1
    }
    for (i in 1:final) {
        ## Only One Entry
        if (singular_case == 1) {
            bib_items[length(bib_items) + 1] <- list(bbl_data[(bib_breakpoints[i]):(length(bbl_data) - 1)])
        }
        if (length(bib_breakpoints) > 1) {
            bib_items[length(bib_items) + 1] <- list(bbl_data[(bib_breakpoints[i]):(bib_breakpoints[(i + 1)] - 1)])
        }
        ## Last Entry
        if (i == (length(bib_breakpoints) - 1)) {
            bib_items[length(bib_items) + 1] <- list(bbl_data[(bib_breakpoints[i + 1]):(length(bbl_data) - 1)])
        }

    }
    return(bib_items)
}

#' append the tex file with a line to link bibliography
#'
#' @param article_dir path to the directory which contains tex article
#' @param file_name file name of the tex document
#' @keywords internal
#' @return appends the tex file with a line to link bibliography
#' @keywords internal
#' @noRd
link_bibliography_line <- function(article_dir, file_name) {
    article_dir <- xfun::normalize_path(article_dir)
    src_file_data <- readLines(file.path(article_dir, file_name))
    bib_exist <- FALSE
    for (line in src_file_data) {
        if (grepl("^\\\\bibliography", line)) {
            bib_exist <- TRUE
            break
        }
    }
    if (bib_exist) {
        message("\\bibliography{bib_file} exists!")
        return("")
    } else {
        bib_line <- paste("\\bibliography{",
                          toString(tools::file_path_sans_ext(file_name)), "}",
                          sep = "")
    }
    # Backup original wrapper file
    backup_file <- paste(file_name,".bk",sep = "")
    backup_file_path <- paste(article_dir, backup_file, sep = "/")
    file_path <- paste(article_dir, file_name, sep = "/")
    write_external_file(backup_file_path, "w", src_file_data)
    # write to original wrapper file
    write_external_file(file_path, "a", bib_line)
}

#' @title comment filter for bbl data
#'
#' @description
#' removes commented latex lines to avoid wrong reference data
#' @param bbl_data blocks of bib_data
#' @keywords internal
#' @return filtered bbl_data
#' @keywords internal
#' @noRd
filter_bbl_data <- function(bbl_data) {
    comment_break_points <- which(grepl("^%%", bbl_data))
    for (pos in comment_break_points) {
        bbl_data[pos] <- ""
    }
    comment_break_points_spaced <- which(grepl("%%", bbl_data))
    for (pos in comment_break_points_spaced) {
        bbl_data[pos] <- ""
    }
    # trim unnecessary whitespace
    for (pos in seq_along(bbl_data)) {
        bbl_data[pos] <- trimws(bbl_data[pos], which = "both")
    }
    return(bbl_data[nzchar(bbl_data)])
}

#' @title bibliography converter
#' @description a quick converter for bbl/tex to bib
#' @param file_path provide a file_path with file name to point tex/bbl file
#' @param log_rebib option to enable log files for rebib
#' @return bib file
#' @export
#' @examples
#' test_file <- system.file("standalone/test.bbl", package = "rebib")
#' dir.create(your_article_folder <- file.path(tempdir(), "testdir"))
#' file.copy(test_file, your_article_folder)
#' your_article_path <- xfun::normalize_path(paste(your_article_folder,"test.bbl",sep="/"))
#' rebib::biblio_converter(file_path = your_article_path)
#' head(readLines(xfun::with_ext(your_article_path,"bib")))
#' unlink(your_article_folder,recursive = TRUE)
biblio_converter <- function(file_path = "", log_rebib = FALSE) {
    file_path <- xfun::normalize_path(file_path)
    date <- Sys.Date()
    if (log_rebib) {
        log_file <- paste0("rebib-biblio-",date,".log")
        log_setup(dirname(file_path), log_file, 1)
        rebib_log(paste0("working directory : ", dirname(file_path)), "info", 1)
        rebib_log(paste0("file name : ", basename(file_path)), "info", 1)
    }
    else{
        message(paste0("working directory : ", dirname(file_path)))
    }
    bib_file_path <- toString(paste(tools::file_path_sans_ext(file_path),
                                    ".bib", sep = ""))
    bib_items <- extract_embeded_bib_items(file_path = file_path)
    if (log_rebib) {
        rebib_log(paste0("bib entries : ", length(bib_items)), "info", 1)
    }
    else{
        message(paste0("bib entries : ", length(bib_items)))
    }

    bibtex_data <- bib_handler(bib_items)
    bibtex_writer(bibtex_data, bib_file_path)
    if (log_rebib) {
        rebib_log(paste0("Written BibTeX file : ", bib_file_path), "info", 1)
    }
    else{
        message(paste0("Written BibTeX file : ", bib_file_path))
    }

}

#' @title bibliography exists
#' @description check if embedded bibliography exists in the latex file or not
#' @param article_dir path to the directory which contains tex article
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#' wd <-  system.file("article", package = "rebib")
#' # Only reads the article file
#' rebib::bibliography_exists(wd)
bibliography_exists <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    file_name <- get_texfile_name(article_dir)
    tex_file_path <- paste(article_dir, file_name, sep = "/")
    src_file_data <- readLines(tex_file_path)
    src_file_data <- filter_bbl_data(src_file_data)
    bbl_start <- which(grepl("^\\s*\\\\begin\\{thebibliography\\}",
                         src_file_data))
    bbl_end <- which(grepl("^\\s*\\\\end\\{thebibliography\\}",
                         src_file_data))
    if (identical(bbl_start, integer(0)) | identical(bbl_end, integer(0))) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}
