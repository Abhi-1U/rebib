#' Main parser logic for converting inbuilt latex bibliography
#'
#' A function meant to be used internally or in a sequence as described
#' in the documentation
#' @param single_bib_data a block of bib_data
#'
#' @return bib_record with unique_id, author and
#'         title(also containing other info)
#' @export
#'
#' @examples
bibliography_parser <- function(single_bib_data) {
    bib_record <- list()
    start_idx <- NULL
    break_points <- NULL
    if(length(single_bib_data) == 1) {
        single_bib_data <- single_bib_data[[1]]
    }
    # starting with unique identifier
    if (which(grepl("\\}$", single_bib_data))[1] ==
        which(grepl("^\\s*\\\\bibitem\\[", single_bib_data))[1]) {
        start_idx <- which(grepl("^\\s*\\\\bibitem\\[", single_bib_data))
        # start_idx =1
        z <- str_split(single_bib_data[1],"\\{")[[1]]
        bib_record$unique_id <- gsub("\\}","",z[length(z)])
        #bib_record$unique_id <- str_split(str_split(gsub("\\\\bibitem\\[|\\]",
        #                                                 "", single_bib_data[start_idx]), "\\{")[[1]][2], "\\}")[[1]][1]
        break_points <- which(grepl("\\\\newblock", single_bib_data))
        # author_names
        # difference between start of identifier and authors = 2
        if ((break_points[1] - start_idx) == 2) {
            bib_record$author <- paste("{{", gsub("\\.$", "",
                                                  single_bib_data[start_idx + 1]), "}}")
        }
        # difference between start of identifier and authors = 3
        if ((break_points[1] - start_idx) == 3) {
            bib_record$author <- paste("{{", gsub("\\.$", "",
                                                  single_bib_data[start_idx + 1]),
                                       gsub("\\.$", "", single_bib_data[start_idx + 2]), "}}")
        }
    }
    if ((which(grepl("\\}$", single_bib_data)) - 1)[1] ==
        which(grepl("^\\s*\\\\bibitem\\[", single_bib_data))[1]) {
        start_idx <- which(grepl("\\}$", single_bib_data))
        bib_record$unique_id <- gsub("\\}$", "",
                                     str_split(single_bib_data[start_idx], "\\{")[[1]][2])
        break_points <- which(grepl("\\\\newblock", single_bib_data))
        # difference between start of identifier and authors = 2
        if ((break_points[1] - start_idx) == 2) {
            bib_record$author <- paste("{{",
                                       gsub("\\.$", "", single_bib_data[start_idx + 1]), "}}")
        }
        # difference between start of identifier and authors = 3
        if ((break_points[1] - start_idx) == 3) {
            bib_record$author <- paste("{{",
                                       gsub("\\.$", "", single_bib_data[start_idx + 1]),
                                       gsub("\\.$", "", single_bib_data[start_idx + 2]),
                                       "}}")
        }
    }
    if (length(break_points) == 1) {
       break_points[2] <- length(single_bib_data)
    }
    # difference between the title and publisher is 1
    if ((break_points[2] - break_points[1]) == 1) {
        bib_record$title <- gsub("\\\\newblock", "",
                                 single_bib_data[break_points[1]])
        bib_record$title <- gsub("emph", "",bib_record$title)
        bib_record$title <- gsub("\\\\", "",bib_record$title)
        bib_record$title <- gsub("\\{", "",bib_record$title)
        bib_record$title <- gsub("\\}", "", bib_record$title)
        bib_record$title <- gsub("\\.$", "",bib_record$title)
        bib_record$title <- paste("{{",bib_record$title,"}}")
    }
    # difference between the title and publisher is 2
    if ((break_points[2] - break_points[1]) == 2) {
        bib_record$title <- gsub("\\\\newblock", "", paste(
                                   single_bib_data[break_points[1]],
                                   single_bib_data[break_points[1] + 1]))
        bib_record$title <- gsub("emph", "",bib_record$title)
        bib_record$title <- gsub("\\\\", "",bib_record$title)
        bib_record$title <- gsub("\\{", "",bib_record$title)
        bib_record$title <- gsub("\\}", "", bib_record$title)
        bib_record$title <- gsub("\\.$", "",bib_record$title)
        bib_record$title <- paste("{{",bib_record$title,"}}")

    }
    # if year is in title itself
    year_regex <- "(?:19|20)\\d{2}"
    if (!identical(which(grepl(year_regex,bib_record$title)),integer(0))) {
        bib_record$year <- gsub(",", "", gsub("\\.$", "",
                                    str_extract(bib_record$title, year_regex)))
        bib_record$title <- gsub(bib_record$year, "", bib_record$title)
        # stray colons
        bib_record$title <- gsub("[[:space:]]+\\:+[[:space:]]","",
                                                            bib_record$title)
        # stray full stops
        bib_record$title <- gsub("[[:space:]]+\\.+[[:space:]]","",
                                                            bib_record$title)
    }
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
        "\\\\"
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
        title_line <- paste(title_line, line)
    }
    #print(filtered_data)
    # fetching URL from remaining data
    if(!identical(which(grepl(url_regex,title_line)),integer(0))){
        bib_record$URL <- gsub(",", "", gsub("\\.$", "",
                                            str_extract(title_line, url_regex)))

        #print(bib_record$URL)
        title_line <- gsub(bib_record$URL, "", title_line)
        title_line <- gsub("URL", "",title_line)
        # stray colons
        title_line <- gsub("[[:space:]]+\\:+[[:space:]]","",title_line)
        # stray full stops
        title_line <- gsub("[[:space:]]+\\.+[[:space:]]","",title_line)
    }
    # fetching year from remaining data
    # year_regex is above
    #or "^[12][0-9]{3}$"
    if(!identical(which(grepl(year_regex,title_line)),integer(0))){
        bib_record$year <- gsub(",", "", gsub("\\.$", "",
                                        str_extract(title_line, year_regex)))
        title_line <- gsub(bib_record$year, "", title_line)
        # stray colons
        title_line <- gsub("[[:space:]]+\\:+[[:space:]]","",title_line)
        # stray full stops
        title_line <- gsub("[[:space:]]+\\.+[[:space:]]","",title_line)
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
        # stray colons
        title_line <- gsub("[[:space:]]+\\:+[[:space:]]","",title_line)
        # stray full stops
        title_line <- gsub("[[:space:]]+\\.+[[:space:]]","",title_line)
        # stray full stops and comma together
        title_line <- gsub("[[:space:]]+\\.,+[[:space:]]","",title_line)
    }
    title_line <- gsub("\\.","",title_line)
    if (!grepl("[[:alpha:]]+", title_line)) {
        title_line <- NULL
    }
    bib_record$journal <- title_line
    return(bib_record)
}
