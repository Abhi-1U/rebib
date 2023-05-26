#' @title rebib log setup
#' @description a wrapper function for logger package to set up log file for
#' logging
#' @param article_dir path to the directory which contains tex article
#' @param file_name name of the log file
#' @param idx index of log level
#' @return null
#' @export
#'
#' @examples
#' dir.create(your_article_folder <- file.path(tempdir(), "exampledir"))
#' example_files <-  system.file("article", package = "rebib")
#' x <- file.copy(from = example_files,to=your_article_folder,recursive = TRUE)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' rebib::log_setup(your_article_path, "log-file.log", 2)
#' unlink(your_article_folder,recursive = TRUE)
log_setup <- function(article_dir, file_name, idx) {
    article_dir <- xfun::normalize_path(article_dir)
    log_file_path <- paste(article_dir, file_name, sep = "/")
    if(! file.exists(log_file_path)) {
        file.create(log_file_path,showWarnings = F)
    } else {
        #pass
    }
    logger::log_threshold(namespace = 'rebib')
    logger::log_appender(logger::appender_file(log_file_path),
                         namespace = "rebib",
                         index=idx)

}



#' @title log messages for various categories
#' @description a wrapper function for logging different types of log entries
#' @param message message to be sent
#' @param category category of the log message
#' @param idx index of log level
#' @return null
#' @export
#'
#' @examples
#' dir.create(your_article_folder <- file.path(tempdir(), "exampledir"))
#' example_files <-  system.file("article", package = "rebib")
#' x <- file.copy(from = example_files,to=your_article_folder,recursive = TRUE)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' rebib::log_setup(your_article_path, "log-file.log", 2)
#' rebib::rebib_log("Hello", "INFO", 2)
#' cat(readLines(paste(your_article_path,"/log-file.log",sep="")),sep="\n")
#' unlink(your_article_folder,recursive = TRUE)
rebib_log <- function(message, category, idx) {
    if (identical(tolower(category), "info")) {
        logger::log_info(message, namespace = 'rebib')
        logger::log_appender(namespace = 'rebib')
    }
    if (identical(tolower(category), "success")) {
        logger::log_success(message, namespace = 'rebib')
        logger::log_appender(namespace = 'rebib')
    }
    if (identical(tolower(category), "warning")) {
        logger::log_warn(message, namespace = 'rebib')
        logger::log_appender(namespace = 'rebib')
    }
    if (identical(tolower(category), "debug")) {
        logger::log_debug(message, namespace = 'rebib')
        logger::log_appender(namespace = 'rebib')
    }
    if (identical(tolower(category), "error")) {
        logger::log_error(message, namespace = 'rebib')
        logger::log_appender(namespace = 'rebib')
    }
    if (identical(tolower(category), "failure")) {
        logger::log_failure(message, namespace = 'rebib')
        logger::log_appender(namespace = 'rebib')
    } else {
        #pass
        #logger::log_info(message)
    }
}
