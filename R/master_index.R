# URL of SEC archive
archives_full_index_url <- "https://www.sec.gov/Archives/edgar/full-index"

# helper to ensure HTTP response code is nothing other than 200
get_check_parse_xml <- function(url) {
  response <- httr::GET(url)
  # check if status code is fine
  assertive::assert_are_set_equal(httr::status_code(response), 200, severity = "stop")
  parsed <- httr::content(response, as = "text", encoding = "UTF-8")

  xml2::read_xml(parsed)
}


construct_master_index_links <- function(year){

  xml <- "index.xml"
  link_to_quarter_dir <- paste(archives_full_index_url, year, xml, sep = "/")

  quarters_dir_xml <- get_check_parse_xml(url = link_to_quarter_dir)
  quarters_text <- quarters_dir_xml %>%
    xml2::xml_find_all("//directory/item/name") %>%
    xml2::xml_text()

  paste(archives_full_index_url, year, quarters_text, "master.idx", sep = "/")
}

dl_master_index <- function(link) {

  # construct download message
  message(crayon::cyan(glue::glue(
    "Getting Q", readr::parse_number(stringr::str_extract(link, "QTR[0-9]+")), " master index ",
    "from ", {link})))


  master_index <- vroom::vroom(
    file = link,
    skip = 11, delim = "|",
    col_names = c("cik", "company_name", "form_type", "date_filed", "filename"),
    col_types = c("cccnD")
  )
}

# memoise function `dl_master_index()` for continued usage
cache_fs <- memoise::cache_filesystem("~/.rcache")
mem_dl_master_index <- memoise::memoise(dl_master_index, cache = cache_fs)

# test <- get_master_index(year = c("2015"))
get_master_index <- function(year = NULL){

  message(crayon::blue(glue::glue("Searching SEC filings from ", {year}, ".")))

  # stop if year argument is null
  assertive::assert_is_not_null(year)

  # generate links to master files
  master_index_links <- purrr::map(year, construct_master_index_links) %>%
    purrr::as_vector()

  # message about caching
  message(crayon::red(glue::glue(
      "tidysec caches the SEC's master index files for the time period you've downloaded up to now at", {path.expand("~/.rcache")},
      " .
      ", "Run `forget_tidysec_cache()` to delete the master indices if you no longer you're done."
    )
  ))

  master_index_lst <- purrr::map(master_index_links, mem_dl_master_index)
  master_index_dt <- data.table::rbindlist(master_index_lst)

}

# helper for `get_list_of_filings()`
# takes input from `get_list_of_filings()`
construct_link_to_filing_dir <- function(cik, dt, xml_index = TRUE) {

  file_names_without_ext <- basename(tools::file_path_sans_ext(dt$filename))
  # remove dashes to construct last part of filing directory
  last_part_of_dir_path <- stringr::str_remove_all(file_names_without_ext, "-")

  archives_data_url <- "https://www.sec.gov/Archives/edgar/data"
  xml <- "index.xml"

  if (xml_index) {
    paste(archives_data_url, cik, last_part_of_dir_path, xml, sep = "/")
  } else if (!xml_index) {
    paste(archives_data_url, cik, last_part_of_dir_path, sep = "/")
  }
}

# takes input from `get_master_index()`
get_list_of_filings <- function(master_index_dt, form_type_in = "all", cik_company){

  assertive::assert_is_not_null(form_type_in)

  # conditionally subset filings depending on function arguments
  if(is.null(form_type_in)|form_type_in == "all"){
    links <- master_index_dt[cik == cik_company]
  } else{
    # must be `&` not `,` to concatenate conditions!
    links <- master_index_dt[form_type %in% form_type_in & cik %in% cik_company]
  }

  # # TODO: stop execution in smart way when this fails, no mere `stop()`
  if(rlang::is_empty(links)){
    message(crayon::red(
    "No filings found. Did you correctly specify the CIK (maybe check for excess leading zeroes) and the filing type?
    Tip: The company you are looking for be may be filing under a different CIK due to corporate restructuring."))
  }
  print(links)

}

# ubs_2013 <- "1114446"
# test_glof <- get_list_of_filings(master_index_dt = test, cik_company = ubs_2013, form_type_in = "13F-HR")

# helper for `get_13f_link()`
# search for xml info table file
check_for_and_get_xml_infotable_file <- function(link_to_filing_dir, link_to_filing_txt){

  file_names <- get_check_parse_xml(paste(link_to_filing_dir, "index.xml", sep = "/")) %>%
    xml2::xml_find_all("item/name") %>%
    xml2::xml_text()

  res <- grep(pattern = "InfoTable.xml", x = file_names)
  # if there a regex match for `InfoTable.xml` in the filenames,
  # subset to that position in the vector, otherwise
  if(!rlang::is_empty(res)){file_names[res]} else {link_to_filing_txt}

}

# takes input from `get_list_of_filings()`
get_13f_link <- function(list_of_filings) {
  list_of_filings_13f <- list_of_filings[form_type == "13F-HR"]
  links_to_filing_dirs <- list_of_filings_13f %>%
    dplyr::mutate(link_to_filing_dir = construct_link_to_filing_dir(
      dt = list_of_filings_13f, cik = 1114446, xml_index = TRUE
    ))
  # print(list_of_filings_13f)
  # print(links_to_filing_dirs)
  # get InfoTable link if any xml documents in directory
  # # check if there's an InfoTable.xml file in each directory supplied
  list_of_filings_plus_xml_link <- links_to_filing_dirs %>%
    dplyr::mutate(link_to_filing = purrr::map2_chr(link_to_filing_dir, filename, check_for_and_get_xml_infotable_file))

  # make full path for xlm files
  list_of_filings_plus_xml_link %>%
    dplyr::mutate(link_to_filing = dplyr::if_else(
      base::endsWith(link_to_filing, "xml"),
      # what to do with XML files
      paste(stringr::str_remove_all(link_to_filing_dir, "/index.xml"),  link_to_filing, sep = "/"),
      # what to do with txt files
      paste("https://www.sec.gov/Archives", link_to_filing, sep = "/")
    )) %>%
    dplyr::select(-c(filename, link_to_filing_dir))

  # print(list_of_filings_plus_xml_link)
}

# get_13f_link(test_glof)

# get XML for 13F if available (only after sometime in 2013)


#' List all filings submitted by company to the SEC.
#'
#' \code{get_filings_info} searches the EDGAR archive for any
#' filings a company has submitted over time.
#'
#' The function returns a tibble with all meta information about all filings,
#' links to the filing itself and the meta information.
#' @param cik Central index key of a company.
#' @param clean_col_names Remove camel case from column names.
#' @examples
#' # Retrieve filings made by the Swiss National Bank
#' cik_snb <- "0001582202"
#' get_filings_info(cik = cik_snb, clean_col_names = TRUE)
#' @export


#' Clear tidysec cache of master index files.
#'
#' \code{clear_tidysec_cache} clears the cache
#' of master index files in which the links to all filings are
#' document.

#' @export
clear_tidysec_cache <- function(){
  result <- memoise::forget(mem_dl_master_index)
  if(result){message("tidysec cache of master indices deleted.")}
}

# clear_tidysec_cache()
