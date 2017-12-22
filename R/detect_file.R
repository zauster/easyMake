#' Detect file
#' @description Reads a file, and detects any filename arguments passed
#' to any of the listed functions.
#'
#' @param this.file The file to be searched.
#' @param function_list The list of functions to search within.
#' @return A dataframe showing the functions, their filename
#'     arguments, adn the searched file.
#' 
#' @export
#' @importFrom dplyr group_by summarise filter left_join %>% select data_frame select bind_rows
#' @importFrom stringr str_replace_all str_extract str_extract_all
detect_file <- function(this.file, function_list) {
 
    function_list <- unique(function_list)

    ## read the file
    if (tools::file_ext(this.file) %in% c("Rmd", "rmd")) {
        text <- parse(knitr::purl(this.file, output = tempfile(),
                                  documentation = 0)) %>% 
            as.character() %>% paste0(collapse = "\n")
    } else {
        text <- this.file %>% readLines(warn = FALSE) %>%
            # delete lines starting with "#"
            str_replace_all("^ *#+.*", "") %>% 
        paste0(collapse = "\n")
    }

    ## Dummy value for zero-line files
    if (nchar(text) > 0) {
        ## parse the file: look for the possible exports/imports
        temp <- text %>%
            str_extract_all(paste0(function_list, "\\(.*\\)")) %>%
            unlist %>%
            str_extract_all("(\".*?\\.*?\")") %>%
            unlist %>%            # needed if there are multiple matches in ( ... )
            ## str_extract("(\".*?\\.*?\")") %>%
            str_replace_all(pattern = "\\\"", "")
        
        ## keep only strings with an "." in between
        ## so that we only keep filenames and
        ## drop all character options
        temp <- temp[grepl("\\.", temp)]

        if(length(temp) > 0) {
            df <- data.frame(object = temp,
                             r_file = str_replace_all(this.file, "//", "/"),
                             stringsAsFactors = FALSE)

            ## return that data.frame
            df
        }
    }
}
