#' Detect Dependencies
#'
#' @description Detect dependencies within files. This is accomplished by
#' reading the filenames passed to the various input and output R functions,
#' then matching those filenames between the different R files. The assumption
#' is that all the files imported to an R script are pre-requisites for that
#' script, and the R script is the pre-requiste for all exported files.
#'
#' @param path A string listing the filepath to search, defaults to
#'     the working directory.
#' @param import_functions A character vector listing the import
#'     functions. This defaults to a pre-populated list, but you can
#'     pass your own list if you only want to detect certain
#'     dependencies.
#' @param export_functions A character vector listing the export
#'     functions. This defaults to a pre-populated list, but you can
#'     pass your own list if you only want to detect certain
#'     dependencies.
#' @param source_detect Logical. Do you want to detect dependencies
#'     between R files when one R script sources another one?
#' @param detect_cycle Do you want easyMake to automatically warn you
#'     when a script depends on its own output?  If FALSE this
#'     function will run faster, but may lead to invalid Makefiles.
#'
#'
#' @examples
#' detect_dependencies(system.file('test_project', package = 'easyMake'))
#'
#' @return
#' A dataframe showing the edge list of dependencies between files.
#' @import data.table
#' @importFrom dplyr %>%
#' @export
detect_dependencies <- function(path = getwd(),
                                import_functions = c("fread",
                                                     "import",
                                                     "load",
                                                     "read_delim",
                                                     "read_file",
                                                     "read_fw",
                                                     "read_lines",
                                                     "read_log",
                                                     "read_table",
                                                     "read.csv",
                                                     "read.dta",
                                                     "read.systat",
                                                     "read.table",
                                                     "read.xlsx",
                                                     "read_excel",
                                                     "readRDS",
                                                     "sasxport.get",
                                                     "spss.get")
                               ,
                                export_functions = c("export",
                                                     "fwrite",
                                                     "save",
                                                     "saveRDS",
                                                     "write_csv",
                                                     "write.csv",
                                                     "write.csv2",
                                                     "write.dcf",
                                                     "write.dta",
                                                     "write.foreign",
                                                     "write.ftable",
                                                     "write.table",
                                                     "write.tsv",
                                                     "write.xlsx")
                               ,
                                source_detect = TRUE,
                                detect_cycle = TRUE) {

    ## get all files that are to be parsed
    files <- list.files(path = path,
                        recursive = TRUE, full.names = TRUE)
    R_files <- files[tools::file_ext(files) %in% c("R", "r", "Rmd",
        "rmd", "RMD")]

    ## 1.
    ## find exports from the files/scripts
    export_list <- lapply(R_files, detect_file,
                          function_list = export_functions)

    if (length(export_list) == 0) {
        exports <- dplyr::data_frame(file = NA, pre_req = NA)
    } else {
        exports <- dplyr::bind_rows(export_list) %>%
            filter(!is.na(object)) %>%
            dplyr::select(file = object, pre_req = r_file)
    }

    ## 2.
    ## find imports from the files/scripts
    import_list <- lapply(R_files, detect_file,
                          function_list = import_functions)

    if (length(import_list) == 0) {
        imports <- dplyr::data_frame(file = NA, pre_req = NA)
    } else {
        imports <- dplyr::bind_rows(import_list) %>%
            filter(!is.na(object)) %>%
            dplyr::select(file = r_file, pre_req = object)
    }

    dependencies <- dplyr::bind_rows(imports, exports)

    if (source_detect) {
        source_list <- lapply(R_files, detect_file,
                              function_list = "source")

        if(all(sapply(source_list, is.null))) {
            sourced <- dplyr::data_frame(file = NA, pre_req = NA)
        } else {
            sourced <- dplyr::bind_rows(source_list) %>%
                dplyr::filter(!is.na(object)) %>%
                dplyr::select(file = r_file, pre_req = object)
            dependencies <- dplyr::bind_rows(dependencies, sourced)
        }
    }
    dependencies$file <- gsub(paste0(path, "/"), "",
                              x = dependencies$file)
    dependencies$pre_req <- gsub(paste0(path, "/"), "",
                                 x = dependencies$pre_req)
    dependencies <- dplyr::distinct(dependencies)

    if (detect_cycle) {
        df <- dependencies
        df2 <- df
        names(df2)[2] <- "working"
        for (i in seq_len(nrow(df))) {
            df2 <- dplyr::left_join(df2, df, by = c("working" = "file"))
            names(df2)[ncol(df2)] <- "working"
            names(df2)[ncol(df2) - 1] <- paste0("pre_req", i)
        }

        if (!all(is.na(df2$working))) {
            cycle_files <- df2 %>%
                dplyr::filter(!is.na(working)) %>%
                .$file %>%
                unique() %>%
                paste(collapse = ", ")
            warning(paste("Circular dependencies detected in the following files:",
                          cycle_files))
        }
    }

    ## set it to a data.table
    setDT(dependencies)

    return(dependencies)
}
