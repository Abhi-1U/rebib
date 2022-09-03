#' @title aggregate bibliography
#' @description aggregate bibliograhy to fill in the missing references
#' @param article_dir path to the directory which contains tex article
#'
#' @return aggregated bib file
#' @export
aggregate_bibliography <- function(article_dir) {
    article_dir <- normalizePath(article_dir)
    date <- Sys.Date()
    log_file <- paste0("bib-agrr-log-",date,".log")
    log_setup(article_dir, log_file, 1)
    rebib_log(paste0("working directory : ", article_dir), "info", 1)
    file_name <- get_texfile_name(article_dir)
    rebib_log(paste0("file name : ", file_name), "info", 1)
    tex_file_path <- paste(article_dir, file_name, sep = "/")
    bib_file <- get_bib_file(article_dir, file_name)
    bib_file_path <- paste(article_dir, bib_file, sep = "/")
    # bibliography from latex file
    if (! bibliography_exists(article_dir)) {
        rebib_log("Cannot aggregate bibliography as there is no embedded bibliography", "info", 1)
        return(0)
    }
    if (identical(bib_file, "")) {
        # handle_bibliography if aggregation not possible
        rebib_log("Cannot aggregate bibliography as there is no BibTeX bibliography", "info", 1)
        rebib_log(paste0("Further Log entries in rebib-log-", date, ".log file"), "info", 1)
        handle_bibliography(article_dir)
    } else {
        rebib_log("bibliography aggregation possible", "info", 1)
        bib_items <- extract_embeded_bib_items(article_dir, file_name)
        parsed_bbl_data <- bib_handler(bib_items)
        parsed_bib_data <- bibtex_reader(article_dir)
        parsed_bbl_data <- filter_repetition(parsed_bbl_data, parsed_bib_data)
        bibtex_writer(parsed_bbl_data, tex_file_path)
        link_bibliography_line(article_dir, file_name)
        rebib_log("BibTeX file aggregated", "info", 1)
    }
}

#' @title filter repeated entries
#'
#' @param bbl_data parsed embedded bibliography data
#' @param bib_data bibtex reference data
#'
#' @return aggregated bbl_data
filter_repetition <- function(bbl_data, bib_data) {
    bbl_uids <- list()
    for (iterator in seq_along(bbl_data$book)) {
        bbl_uids[iterator] <- paste0("{",bbl_data$book[[iterator]]$unique_id,"}")
    }
    bbl_uids <- unlist(bbl_uids)
    delta <- setdiff(unlist(bbl_uids),bib_data$names)
    rebib_log(paste0("aggregation delta : ", length(delta)), "info", 1)
    delta_bbl_data <- list()
    for (iterator in seq_along(delta)) {
        for (iterator_2 in seq_along(bbl_data$book)){
            if (identical(delta[iterator],
                    paste0("{",bbl_data$book[[iterator_2]]$unique_id,"}"))) {
                delta_bbl_data$book[[iterator]] <- bbl_data$book[[iterator_2]]
            }
        }
    }
    return(delta_bbl_data)
}

