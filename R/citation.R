#' @title count citation elements
#' @description  counts Cite inline elements embedded within the latex file
#' currently supported inlines \\cite,\\citealp, \\citep, \\citet
#'
#' @param file_path path to the LaTeX file
#'
#' @return count of the inline element
#' @export
#'
#' @examples
#' file_path <- system.file("examples/article",
#'                  package = "texor")
#' cite <- texor::count_inline(file_path)
count_inline <- function(file_path) {
    # readLines
    raw_lines <- readLines(file_path)
    raw_words <- str_split(raw_lines," ")
    # filters comments in the given tex file
    comments <- which(grepl("\\%",raw_lines))
    for ( comment in comments) {
        raw_lines[comment] <- ""
    }
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
                }
            }
        }
    }
    return(count)
}
