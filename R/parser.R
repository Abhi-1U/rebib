#' Main parser logic for converting inbuilt latex bibliography
#'
#' A function meant to be used internally or in a sequence as described
#' in the documentation
#' @param single_bib_data
#'
#' @return bib_record with unique_id, author and
#'         title(also containing other info)
#' @export
#'
#' @examples
bibliography_parser <- function(single_bib_data) {
    bib_record <- list()
    # minimal bibliography parser / generator
    if (which(grepl("\\}$", single_bib_data)) ==
        which(grepl("^\\s*\\\\bibitem\\[", single_bib_data))) {
        start_idx <- which(grepl("^\\s*\\\\bibitem\\[", single_bib_data))
        # start_idx =1
        bib_record$unique_id <- str_split(str_split(gsub("\\\\bibitem\\[|\\]",
                                                         "", single_bib_data[start_idx]), "\\{")[[1]][2], "\\}")[[1]][1]
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
    if ((which(grepl("\\}$", single_bib_data)) - 1) ==
        which(grepl("^\\s*\\\\bibitem\\[", single_bib_data))) {
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
    remaining_data <- single_bib_data[break_points[1]:length(single_bib_data)]
    cat(remaining_data)


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
    title_line <- "{{"
    for (line in filtered_data) {
        title_line <- paste(title_line, line)
    }
    title_line <- paste(title_line, "}}")
    #print(filtered_data)
    bib_record$title <- title_line
    return(bib_record)
}
