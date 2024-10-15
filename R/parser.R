#' Main parser logic for converting inbuilt latex bibliography
#'
#' A function meant to be used internally or in a sequence as described
#' in the documentation
#' @param single_bib_data a block of bib_data
#'
#' @return bib_record with unique_id, author and
#'         title(also containing other info)
#' @noRd
bibliography_parser <- function(single_bib_data) {
    bib_record <- list()
    start_idx <- NULL
    break_points <- NULL
    if ( length(single_bib_data) == 1) {
        single_bib_data <- single_bib_data[[1]]
    }
    # starting with unique identifier
    mini_iter_stop <- which(grepl("\\\\newblock", single_bib_data))[1]
    if (is.na(mini_iter_stop)) {
        # if there is no new block, we can't parse the data

        concat_lines <- function(single_bib_data) {
            trimmed_lines <- stringr::str_trim(single_bib_data)
            concatenated <- paste(stringr::str_trim(trimmed_lines), collapse = " ")
            return(concatenated)
        }
        bib_data_str <- concat_lines(single_bib_data)

        # get unique_id in \bibitem{unique_id}
        bib_record$unique_id <- stringr::str_match(bib_data_str,
                                          "\\\\bibitem(?:\\[[^\\]]*\\])?\\{([^\\}]+)\\}")[2]
        bib_record$unique_id <- stringr::str_trim(bib_record$unique_id)

        bib_record$author <- stringr::str_match(bib_data_str,
                                        "\\}(.+)\\\\emph\\{")[2]
        bib_record$author <- stringr::str_trim(bib_record$author)
        bib_record$author <- gsub("[.,]+$", "", bib_record$author)


        bib_record$title <- stringr::str_match(bib_data_str,
                                      "\\\\emph\\{([^\\}]+?)\\}")[2]
        bib_record$title <- stringr::str_trim(bib_record$title)

        rest_bib_data <- stringr::str_match(bib_data_str,
                                   "\\\\emph\\{[^\\}]+\\}(.+)")[2]

        bib_record$year <- stringr::str_match(rest_bib_data, "([0-9]{4})")[2]
        rest_bib_data <- stringr::str_match(bib_data_str,
                                   "\\\\emph\\{[^\\}]+\\}(.+)")[2]

        # put all the remaining data in journal
        rest_bib_data <- gsub(bib_record$year, "", rest_bib_data)
        rest_bib_data <- stringr::str_trim(rest_bib_data)
        rest_bib_data <- gsub("^[ ,.]+", "", rest_bib_data)
        rest_bib_data <- gsub("[ ,.]+$", "", rest_bib_data)
        bib_record$journal <- stringr::str_trim(rest_bib_data)

        return(bib_record)
    } else{
        for (line in which(grepl("\\}$", single_bib_data[1:(mini_iter_stop-1)]))){
            if (line == which(grepl("]\\{", single_bib_data))[1]) {
                start_idx <- which(grepl("^\\s*\\\\bibitem", single_bib_data))
                z <- str_split(single_bib_data[line],"]\\{")[[1]]
                bib_record$unique_id <- gsub("\\}","",z[length(z)])
                author_start <- line + 1
                break
            }
        }
    }
    bib_record$unique_id <- gsub("\\}","",z[length(z)])
    break_points <- which(grepl("\\\\newblock", single_bib_data))
    # author_names
    # difference between start of identifier and authors = 2
    if ((break_points[1] - start_idx) == 2) {
        bib_record$author <- gsub("\\.$", "",
                                        single_bib_data[author_start])
    }
    # difference between start of identifier and authors >= 3
    if ((break_points[1] - start_idx) >= 3) {
        author_end <- break_points[1]-1
        bib_record$author <- gsub("\\.$", "",
                    paste(single_bib_data[author_start:author_end],
                            sep=' ',collapse = ' '))
    }

    bib_record$author <- trimws(bib_record$author, which = "both")
    if (length(break_points) == 1) {
       break_points[2] <- length(single_bib_data)
    }
    if((break_points[2] - break_points[1]) == 0) {
        bib_record$title <- NULL
    }
    # difference between the title and publisher is 1
    if ((break_points[2] - break_points[1]) == 1){
        bib_record$title <- gsub("\\\\newblock", "",
                                 single_bib_data[break_points[1]])
        bib_record$title <- gsub("emph", "",bib_record$title)
        bib_record$title <- gsub("\\\\", "",bib_record$title)
        bib_record$title <- gsub("\\{", "",bib_record$title)
        bib_record$title <- gsub("\\}", "", bib_record$title)
        bib_record$title <- gsub("\\.$", "",bib_record$title)
        bib_record$title <- trimws(bib_record$title, which = "both")
    }
    # difference between the title and publisher is >= than 2
    if ((break_points[2] - break_points[1]) >= 2) {
        title_start <- break_points[1]
        title_end <- break_points[2]-1
        bib_record$title <- gsub("\\\\newblock", "", paste(
                                   single_bib_data[title_start:title_end],sep = ' ',collapse = ' '))
        # Filtering unneccesary values
        bib_record$title <- gsub("emph", "",bib_record$title)
        bib_record$title <- gsub("\\\\", "",bib_record$title)
        bib_record$title <- gsub("\\{", "",bib_record$title)
        bib_record$title <- gsub("\\}", "", bib_record$title)
        bib_record$title <- gsub("\\,$", "",bib_record$title)
        bib_record$title <- gsub("\\.$", "",bib_record$title)
        bib_record$title <- gsub("\\,$", "",bib_record$title)
        bib_record$title <- trimws(bib_record$title, which = "both")

    }
    # if year is in title itself
    year_regex <- "((19|20)[0-9][0-9])"
    if (!identical(which(grepl(year_regex,bib_record$title)),integer(0))) {
        bib_record$year <- gsub(",", "", gsub("\\.$", "",
                                    str_extract(bib_record$title, year_regex)))
        bib_record$title <- gsub(bib_record$year, "", bib_record$title)
        bib_record$title <- gsub("\\,$", "",bib_record$title)
        bib_record$title <- gsub("\\.$", "",bib_record$title)
        bib_record$title <- gsub("\\,$", "",bib_record$title)
    }
    pages_regex <- "(\\d+--\\d+)"
    url_regex <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
    remaining_data <- single_bib_data[break_points[2]:length(single_bib_data)]
    #remaining_data <- single_bib_data[break_points[2]:length(single_bib_data)]
    #cat(remaining_data)
    latex_macros <- list(
        "^\\bibitem",
        "newblock",
        "emph",
        "\\penalty0",
        "\\url",
        "\\{",
        "\\}",
        "\\\\",
        "beginscriptsize",
        "endscriptsize"
    )
    filtered_data <- remaining_data
    for (line in seq_along(remaining_data)) {
        filtered_data[line] <- remaining_data[line]
        for (patt in latex_macros){
            if (patt == "newblock"){
                filtered_data[line] <- gsub(patt, "", filtered_data[line])
            } else {
                filtered_data[line] <- gsub(patt, "", filtered_data[line])
            }
        }
    }
    title_line <- ""
    for (line in filtered_data) {
        title_line <- paste(title_line, line, sep = "")
    }
    # fetching URL from remaining data
    if(!identical(which(grepl(url_regex,title_line)),integer(0))){
        bib_record$URL <- gsub(",", "", gsub("\\.$", "",
                                            str_extract(title_line, url_regex)))
        title_line <- gsub(url_regex, "", title_line)
        title_line <- gsub("URL", "",title_line)
    }
    # fetching year from remaining data
    # page_ranges
    if(!identical(which(grepl(pages_regex,title_line)),integer(0))){
        bib_record$pages <- gsub(",", "", gsub("\\.$", "",
                                              str_extract(title_line, pages_regex)))
        title_line <- gsub(bib_record$pages, "", title_line)
    }
    # year_regex is above
    #or "^[12][0-9]{3}$"
    if(!identical(which(grepl(year_regex,title_line)),integer(0))){
        bib_record$year <- gsub(",", "", gsub("\\.$", "",
                                        str_extract(title_line, year_regex)))
        bib_record$year <- trimws(bib_record$year, which = "both")
        title_line <- gsub(bib_record$year, "", title_line)
    }

    # fetching isbn from remaining data
    isbn_regex <- "ISBN"
    if(!identical(which(grepl(isbn_regex,title_line)),integer(0))){
        sp_title_line <- unlist(stringr::str_split(title_line," "))
        slice_point <- stringr::str_which(sp_title_line,"ISBN") +1
        bib_record$isbn <- gsub(",", "", gsub("\\.$", "",
                                                sp_title_line[slice_point]))
        title_line <- gsub(bib_record$isbn, "", title_line)
        title_line <- gsub("ISBN", "", title_line)
    }
    # Filtering stray commas and periods
    title_line <- gsub("\\.$","",title_line)
    title_line <- gsub("\\,$","",title_line)
    title_line <- gsub("\\.+[[:space:]]"," ",title_line)
    title_line <- gsub("\\,+[[:space:]]"," ",title_line)
    if (!grepl("[[:alpha:]]+", title_line)) {
        title_line <- NULL
    }
    # Filtering stray spaces
    title_line <- trimws(title_line, which = "both")
    if (identical(title_line, character(0))) {
        title_line <- NULL
    }
    bib_record$journal <- title_line
    return(bib_record)
}
