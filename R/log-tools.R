#' @title rebib log setup
#' @description a wrapper function for logger package to set up log file for
#' logging
#' @param article_dir path to the directory which contains tex article
#'
#' @return null
#' @export
#'
#' @examples
#'  wd <-  system.file("article", package = "rebib")
#' rebib::log_setup(wd)
log_setup <- function(article_dir) {
    date <- Sys.Date()
    log_file <- paste0("rebib-log-",date,".log")
    log_file_path <- paste(article_dir, log_file, sep = "/")
    if(! file.exists(log_file_path)) {
        file.create(log_file_path)
    } else {
        #pass
    }
    logger::log_threshold(index = 2)
    logger::log_appender(logger::appender_file(log_file_path),index=2)

}



#' @title log messages for various categories
#' @description a wrapper function for logging different types of log entries
#' @param message message to be sent
#' @param category category of the log message
#'
#' @return null
#' @export
#'
#' @examples
#'  wd <-  system.file("article", package = "rebib")
#' rebib::log_setup(wd)
#' rebib::rebib_log("Hello", "INFO")
#' cat(readLines(paste(wd,"/rebib-log-",Sys.Date(),".log",sep="")),sep="\n")
rebib_log <- function(message, category) {
    if (identical(tolower(category), "info")) {
        logger::log_info(message)
        logger::log_appender()
    }
    if (identical(tolower(category), "success")) {
        logger::log_success(message)
        logger::log_appender()
    }
    if (identical(tolower(category), "warning")) {
        logger::log_warn(message)
        logger::log_appender()
    }
    if (identical(tolower(category), "debug")) {
        logger::log_debug(message,index =2)
        logger::log_appender()
    }
    if (identical(tolower(category), "error")) {
        logger::log_error(message)
        logger::log_appender()
    }
    if (identical(tolower(category), "failure")) {
        logger::log_failure(message)
        logger::log_appender()
    } else {
        #pass
        #logger::log_info(message)
    }
}
