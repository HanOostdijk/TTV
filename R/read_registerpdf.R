#' read pdf with registration info for TTV Amstelveen
#'
#' @name read_registerpdf
#' @param filename Character string with the name of the pdf file
#' @return A data.frame with the fields of the pdf file: 1 row with 17 columns
#' @export
#' @examples
#' \dontrun{
#' df1 <- read_registerpdf(filename)
#' }

read_registerpdf <-  function (filename) {

  `%>%` <- magrittr::`%>%`
  # retrieve textboxes for first (and only) page of the pdf
  df1 <- pdftools::pdf_data(filename, font_info = FALSE, opw = "", upw = "")[[1]]
  # variable number of comments lines between phrase1 and phrase2 !
  # make distinction between NL and EN registration form
  if (df1$text[1] == "Inschrijfformulier") {
    phrase1 <- "^Eventuele opmerkingen:"
    phrase2 <- "^Datum aanvraag lidmaatschap"
    phrase3 <- "^Geboortedatum"
    lang <- "NL"
  } else {
    phrase1 <- "^Any comments:"
    phrase2 <- "^Date membership request"
    phrase3 <- "^Date of birth"
    lang <- "EN"
  }
  # user filled data starts in fields with x>= 257 (except for the comments lines)
  df2 <- df1 %>%
    dplyr::select(x,y,text) %>% # skip unnecessary fields
    dplyr::filter(y > 144) %>%  # skip header lines
    dplyr::mutate(col = ifelse(x<257,1,2), # (potential) user filled fields
           linenr = dplyr::row_number())   # for removing comment lines in later stage
  # combine text of all fields per line (user filled not relevant here)
  df3 <- df2 %>%
    dplyr::nest_by(y) %>%
  dplyr::mutate(
    linenr = data$linenr[1],
    text = paste(data$text, collapse = " ", sep = " ")
  )  %>%
  dplyr::ungroup() %>%
  dplyr::select(-data)
  # row number of first line after phrase1 and last line before phrase2
  c1 <- head(stringr::str_which(df3$text, phrase1),1) + 1
  c2 <- tail(stringr::str_which(df3$text, phrase2),1) - 1
  comments <- "" ; comlinenr <- c()
  # if c2 >= c1 we have comment lines :
  # extract comments and determine linenr-s of the comment parts
  if (c2 >= c1) {
    df3a <- df3 %>%
      dplyr::slice(seq(c1,c2,1))
    comments <- paste(dplyr::pull(df3a,text), collapse = " ", sep = " ")
    comlinenr <- as.numeric(df3a$linenr)
    comlinenr <- seq(min(comlinenr),dplyr::pull(df3,linenr)[c2+1]-1,1)
  }
  # remove the rows of the comment parts
  # then  user filled field is identical to col == 2
  # combine text of all fields per line (user filled IS relevant here)
  df4 <- df2 %>%
    dplyr::filter(!(linenr %in% comlinenr)) %>%
    dplyr::nest_by(y, col) %>%
    dplyr::mutate(
      x = data$x[1],
      text = paste(data$text, collapse = " ", sep = " ")
    )  %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(x,data))
  # now we have 'text' per line ('y') and per column ('col')
  # pivot per line and remove lines without user filled data
  df5 <- tidyr::pivot_wider(df4,id_cols=y,names_from=col,names_prefix='col',values_from = text) %>%
    dplyr::filter(!(y %in% c(286,343,371,428,456,470))) %>%
    dplyr::select(-y)
  # insert comments
  c1 <- stringr::str_which(df5$col1, phrase1)
  df5$col2[c1] <- comments
  # remove lines after line with phrase2 and replace NA with empty string
  c2 <- stringr::str_which(df5$col1, phrase2)
  df5 <- df5 %>%
    dplyr::filter (dplyr::row_number() <= c2) %>%
    dplyr::mutate (col2 = ifelse(is.na(col2),"",col2))
  # change dates to English notation
  EN <- c( "Jan" , "Feb", "Mar", "Apr", "May", "Jun",
		                     "Jul" , "Aug", "Sep", "Oct", "Nov", "Dec" )
	NL <- c( "jan" , "feb", "maart", "april", "mei", "juni",
		                     "jul" , "aug", "sep", "okt", "nov", "dec"  )
	tr <- EN ; names(tr) <- NL
	c1 <- stringr::str_which(df5$col1, paste0(phrase3,"|",phrase2)) # date lines
	df5$col2[c1] <- stringr::str_replace_all(df5$col2[c1],tr) %>%
	  stringr::str_remove_all(" ")
	# pivot and convert date characters to 'proper' dates
  df6 <- df5 %>%
  tidyr::pivot_wider(names_from=col1,values_from = col2) %>%
  dplyr::mutate(
    dplyr::across(c(3,16),~lubridate::dmy(.)) ,
    lang = !!lang
  )
  # force NL headers
  names(df6) <- c(
    "Achternaam", "Voorna(a)m(en)", "Geboortedatum", "Geslacht", "Straatnaam en Huisnummer",
    "Postcode en Woonplaats", "Mobiel nummer", "Email adres", "Mobiel nummer ouders/verzorgers",
    "Email adres ouders/verzorgers", "Heb je al eerder getafeltennist?", "Wat was/is je NTTB bondsnummer?",
    "Wil je meedoen aan de competitie?", "Heb je al eerder competitie gespeeld?", "Eventuele opmerkingen:",
    "Datum aanvraag lidmaatschap", "lang"
  )
  df6
}

#' convert registration info read from form to data.frame for member admin of TTV Amstelveen
#'
#' @name convert_reginfo_to_memadm
#' @param df1 data.frame read by read_registerpdf
#' @return A data.frame with the converted to format fit for member administration
#' @export
#' @examples
#' \dontrun{
#' df2 <- convert_reginfo_to_memadm(df1)
#' }

convert_reginfo_to_memadm <- function(df1){
  `%>%` <- magrittr::`%>%`
  stopifnot(nrow(df1) == 1)
  df2 <- df1 %>%
    dplyr::mutate(
      volgnr = 1,
      Bondsnr = "?",
      CG = "?",
      `Para-TT` = "?" ,
      `Geb.datum` = Geboortedatum ,
      age_now = lubridate::as.duration(
        lubridate::interval(Geboortedatum,lubridate::today())) %/%
        lubridate::as.duration(lubridate::years(1)) ,
      `J/S` = paste(ifelse(age_now<19,"?J","?S"),": nu",age_now,"jaar"),
      Categorie = "?" ,
      ELO = "?",
      LJ = "?",
      LS = "?",
      `M/V` = ifelse(Geslacht == "man","M","V"),
      Naam = paste0(Achternaam,", ",`Voorna(a)m(en)`," ?"),
      Voornaam = `Voorna(a)m(en)`,
      Adres = `Straatnaam en Huisnummer`,
      Postcode = stringr::str_sub(`Postcode en Woonplaats`,1,7) %>%
        toupper() %>%
        stringr::str_remove_all(" ") %>%
        (function (x) paste(stringr::str_sub(x,1,4),stringr::str_sub(x,5,6))),
      Woonplaats = stringr::str_sub(`Postcode en Woonplaats`,8) %>%
        stringr::str_trim() ,
      Telefoon = "?",
      Mobiel1 = `Mobiel nummer`,
      `Mobiel2 (ouders)` = `Mobiel nummer ouders/verzorgers`,
      Email1  = `Email adres`,
      `Email2 (ouders)`  = `Email adres ouders/verzorgers` ,
      `Lid sinds` = `Datum aanvraag lidmaatschap`,
      Lang_inschr = lang
    ) %>%
    dplyr::select(-age_now) %>%
    dplyr::select(volgnr:Lang_inschr)
}

#' write converted registration info to Excel sheet for member admin of TTV Amstelveen
#'
#' @name write_reginfo_to_memadm
#' @param df1 data.frame created by convert_reginfo_to_memadm
#' @param wbname Character string with name of workbook to write to
#' @return invisible(0)
#' @section Details:
#' The sheetname is "Nederlands" or "Engels" depending on the language of the (converted) registration form
#' @export
#' @examples
#' \dontrun{
#' df1 <- read_registerpdf(filename)
#' df2 <- convert_reginfo_to_memadm(df1)
#' write_reginfo_to_memadm(df2)
#' }
write_reginfo_to_memadm <- function(df1,wbname="inschrijf.xlsx") {
  wb <- openxlsx::createWorkbook("inschrijf")
  if (df1$Lang_inschr[1] == "NL") {
    sheetname <- "Nederlands"
  } else {
    sheetname <- "Engels"
  }
  openxlsx::addWorksheet(wb, sheetname)
  openxlsx::writeData(wb, sheet = 1, df1)
  openxlsx::saveWorkbook(wb, wbname, overwrite = TRUE)
}

