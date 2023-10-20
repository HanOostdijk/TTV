#' Generate VCF information from list
#'
#' @name gen_vcf
#' @param NL Named list with information about a person. See Details
#' @return character string with information for one person in VCF format
#' @export
#' @section Details:
#' The named list can contain fields with the following names (case insensitive) while other fields will be ignored
#' - fn : first name
#' - sn : second name
#' - group : character vector (or string with comma separated values) with group the person belongs to
#' - email* : with * one of email, email_home, email_work or email_other
#' - phone* : with * one of phone, phone_home, phone_home2, phone_work, phone_work2 or phone_mobile
#' - street* : with * one of home work other
#' - place* : with * one of home work other
#' - state* : with * one of home work other
#' - postcode* : with * one of home work other
#' - country* : with * one of home work other
#' @examples
#' \dontrun{
#' gen_vcf(
#'   list(fn="Han",sn="Oostdijk",
#'     street_work="Runstreet 1224",place_work="Amstelveen",
#'     postcode_work="1181XX",state_work="NH",country_work="NL",
#'     email_work="han@hanoostdijk.nl",phone_work="020-1234567")
#'  )
#' # BEGIN:VCARD
#' # VERSION:3.0
#' # N:Oostdijk;Han;;;
#' # FN:Han Oostdijk
#' # TEL;TYPE=WORK:020-1234567
#' # ADR;TYPE=WORK:;;Runstreet 1224;Amstelveen;NH;1181XX;NL
#' # END:VCARD
#' }

gen_vcf <- function(NL)
 {
  names(NL) <- tolower(names(NL))
  glue::glue_collapse(
    purrr::keep(
      c(
        "BEGIN:VCARD" ,
        "VERSION:3.0" ,
        gen_vcf_names(NL),
        gen_vcf_line(NL, "email"),
        gen_vcf_line(NL, "email_home"),
        gen_vcf_line(NL, "email_work"),
        gen_vcf_line(NL, "email_other"),
        gen_vcf_line(NL, "phone"),
        gen_vcf_line(NL, "phone_home"),
        gen_vcf_line(NL, "phone_home2"),
        gen_vcf_line(NL, "phone_work"),
        gen_vcf_line(NL, "phone_work2"),
        gen_vcf_line(NL, "phone_mobile"),
        gen_vcf_address(NL),
        gen_vcf_cats(NL),
        "END:VCARD"
      )
    ,function(x) nchar(x) > 0
    )
  , sep = "\n")
}

#' Convert a data.frame with VCF information to VCF text format
#'
#'
#' @name convert_df_vcf
#' @param df1 data.frame with information about persons.
#'  Only field names described in the **Details** section  \code{\link{gen_vcf}} are converted
#' @param write_vcf Logical value indicating if information is written to `vcf_filename`
#' @param vcf_filename Character string with name of `.VCF` file to which the text information is written to
#'  if `write_vcf==T`
#' @return Character string with the information in VCF format if not written to file otherwise `NULL`
#' @export
#' @examples
#' \dontrun{
#' df1 <- tibble::tribble(
#'   ~fn , ~sn, ~phone_work ,
#'   "han", "oostdijk", "020-1234567",
#'   "john","smith","+31207654321")
#'
#' convert_df_vcf(df1,write_vcf=F)
#' # BEGIN:VCARD
#' # VERSION:3.0
#' # N:oostdijk;han;;;
#' # FN:han oostdijk
#' # TEL;TYPE=WORK:020-1234567
#' # END:VCARD
#' # BEGIN:VCARD
#' # VERSION:3.0
#' # N:smith;john;;;
#' # FN:john smith
#' # TEL;TYPE=WORK:+31207654321
#' # END:VCARD
#'
#' res <- convert_df_vcf(df1,write_vcf=T,vcf_filename="df1.vcf" )
#' is.null(res)
#' # [1] TRUE
#' }

convert_df_vcf <- function(df1, write_vcf = T, vcf_filename=NULL ) {
  vcf1 <- df1 %>%
    dplyr::nest_by(dplyr::row_number()) %>%
    dplyr::mutate(data = list(as.list(data)),
                  vcf = gen_vcf(data)) %>%
    dplyr::pull(vcf)
  if (write_vcf == T) {
    con <- file(vcf_filename, "w", encoding = "UTF-8")
    on.exit(expr = close(con))
    glue::glue_collapse(vcf1, sep = "\n") %>%
      writeLines(con)
  } else {
    return(vcf1)
  }
}

`%>%` <- magrittr::`%>%`

gen_vcf_line <- function(mylist,fieldname) {
  value = null_field(mylist,fieldname)
  if ( nchar(value) == 0 ) return (value)
  switch(EXPR = fieldname,
         email = glue::glue("EMAIL;TYPE=INTERNET;TYPE=HOME:{value}"),
         email_home = glue::glue("EMAIL;TYPE=INTERNET;TYPE=HOME:{value}"),
         email_work = glue::glue("EMAIL;TYPE=INTERNET;TYPE=WORK:{value}"),
         email_other = glue::glue("EMAIL;TYPE=INTERNET;TYPE=OTHER:{value}"),
         phone = glue::glue("TEL;TYPE=HOME:{value}"),
         phone_home = glue::glue("TEL;TYPE=HOME:{value}"),
         phone_home2 = glue::glue("TEL;TYPE=HOME2:{value}"),
         phone_work = glue::glue("TEL;TYPE=WORK:{value}"),
         phone_work2 = glue::glue("TEL;TYPE=WORK2:{value}"),
         phone_mobile = glue::glue("TEL;TYPE=MOBILE:{value}")
  )
}

gen_vcf_address <- function(mylist) {
  adresslist <- c()
  for (type1 in c("home", "work", "other")) {
    adr_TF <- F
    adr <- glue::glue("ADR;TYPE={type1a}:;",type1a=toupper(type1))
    for (type2 in c("street", "place", "state", "postcode", "country")) {
      v1 = null_field(mylist, paste(type2, type1, sep = "_"))
      if (nchar(v1) > 0)
        adr_TF <- T
      adr <- glue::glue("{adr};{v1}")
    }
    if (adr_TF) {
      adresslist <- c(adresslist, adr)
    }
  }
  adresslist
}

gen_vcf_cats <- function(mylist) {
  v1 <- glue::glue_collapse(mylist$group, sep = ",")
  if (length(v1) == 0) return("")
  cats <- glue::glue("CATEGORIES:{v1}")
  cats <- HOQCutil::hard_split(cats, 76)
  cats <- paste0(c("", rep(" ", length(cats) - 1)), cats)
  cats
}

gen_vcf_names <- function(mylist) {
  v1 = null_field(mylist, "sn")
  v2 = null_field(mylist, "fn")

  c(
  glue::glue("N:{v1};{v2};;;"),
  glue::glue("FN:{v2} {v1}")
  )
}

null_field <- function(mylist,myfield) {
  res <- getElement(mylist,myfield)
  res <- ifelse(is.null(res)||is.na(res),"",res)
  res
}


