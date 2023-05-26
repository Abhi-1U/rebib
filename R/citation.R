#' @title citation reader
#' @description  counts/reads  Cite inline elements embedded within the latex
#' file
#'
#' @param file_path path to the LaTeX file
#'
#' @return count of the inline element
#' @export
#'
#' @examples
#' file_path <- system.file("article/example.tex",
#'                  package = "rebib")
#' # Only Reads the example.tex for possible citations
#' cite <- rebib::citation_reader(file_path)
#' cite
citation_reader <- function(file_path) {
    # readLines
    file_path <- xfun::normalize_path(file_path)
    raw_lines <- readLines(file_path)
    raw_words <- str_split(raw_lines," ")
    # filters comments in the given tex file
    comments <- which(grepl("\\%",raw_lines))
    for ( comment in comments) {
        raw_lines[comment] <- ""
    }
    citation_references <- list()
    count <- 0
    begin_patt <- c("\\\\cite\\{",
                    "\\\\citealp\\{",
                    "\\\\citet\\{",
                    "\\\\citep\\{")
    cite_break_points <- which(grepl(begin_patt[1],raw_lines))
    citealp_break_points <- which(grepl(begin_patt[2],raw_lines))
    citet_break_points <- which(grepl(begin_patt[3],raw_lines))
    citep_break_points <- which(grepl(begin_patt[4],raw_lines))
    if (!identical(cite_break_points,integer(0))) {
        for (pos in cite_break_points) {
            raw_words <- str_split(raw_lines[pos]," ")
            for (word in raw_words[[1]]) {
                if (grepl(begin_patt[1], word)) {
                    count = count + 1
                    citation_references <- append(citation_references, word)
                }
            }
        }
    }
    if (!identical(citealp_break_points,integer(0))) {
        for (pos in citealp_break_points) {
            raw_words <- str_split(raw_lines[pos]," ")
            for (word in raw_words[[1]]) {
                if (grepl(begin_patt[2], word)) {
                    count = count + 1
                    citation_references <- append(citation_references, word)
                }
            }
        }
    }
    if (!identical(citep_break_points,integer(0))) {
        for (pos in citep_break_points) {
            raw_words <- str_split(raw_lines[pos]," ")
            for (word in raw_words[[1]]) {
                if (grepl(begin_patt[4], word)) {
                    count = count + 1
                    citation_references <- append(citation_references, word)
                }
            }
        }
    }
    if (!identical(citet_break_points,integer(0))) {
        for (pos in citet_break_points) {
            raw_words <- str_split(raw_lines[pos]," ")
            for (word in raw_words[[1]]) {
                if (grepl(begin_patt[3], word)) {
                    count = count + 1
                    citation_references <- append(citation_references, word)
                }
            }
        }
    }
    citation_references <- unlist(citation_references)
    citation_references <- filter_citation_references(citation_references)
    citation_data <- list()
    citation_data$count <- length(citation_references)
    citation_data$references <- citation_references
    return(citation_data)
}

#' @title filter citation references
#' @description a filter for extracting citation references
#' @param citation_references a vector of citation references(unfiltered)
#'
#' @return a vector of filtered citation references
#' @noRd
filter_citation_references <- function(citation_references) {
    filtered_citation_references <- list()
    for (iterator in seq_along(citation_references)) {
        citation_line <- citation_references[iterator]
        # filtering out things
        citation_line <- stringr::str_extract(citation_line, "(\\{.*?\\})")
        if (grepl(",", citation_line)){
            cites <- stringr::str_split(citation_line,",")
            filtered_citation_references <- append(filtered_citation_references,paste0(cites[[1]][1],"}"))
            end <- length(cites[[1]])-1
            for (value in 2:end) {
                filtered_citation_references <- append(filtered_citation_references,paste0("{",cites[[1]][value],"}"))
            }
            filtered_citation_references <- append(filtered_citation_references,paste0("{",cites[[1]][end+1]))
        } else {
            filtered_citation_references <- append(filtered_citation_references,citation_line)
        }
    }
    return(unlist(filtered_citation_references))
}
