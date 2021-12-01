#' Read \code{.pdf} files into R
#'
#' @description Reads \code{.pdf} files into R as text and splits into sections.
#' @param x \code{character}. File path to the \code{.pdf} file.
#' @return A \code{list} object.
#' @author Matt Lewis
#' @export

read_pdf <-
  function(x){
    # Check inputs
    assertthat::assert_that(file.exists(x))
    assertthat::assert_that(assertthat::has_extension(x, "pdf"))

    # read in
    pdf_tab <-
      x %>%
      read_pdf(
        x = pdftools::pdf_data(., font_info = T),
        size = pdftools::pdf_pagesize(.)
      )


    ## Beyond here is partly completed

    # check number of characters between each word
    pdf_text_tab <-
      pdf_text %>%
      count_spaces(combine_single_spaces = T)

    pdf_text2 <- pdf_text_tab[0,]
    for(i in 1:nrow(pdf_text2)){

    }

    # If we have multiple empty rows then infer a new paragraph, if not then paste text around line breaks
    pdf_text2 <- pdf_text[1]
    count <- 2
    for(i in 2:length(pdf_text)){
      if(pdf_text[i] == ""){
        if(pdf_text2[count-1] != "\n"){
          pdf_text2[count] <- "\n"
          count <- count +1
        }
      }else{
        if(pdf_text2[count-1] == "\n"){
          pdf_text2[count] <- pdf_text[i]
          count <- count +1
        }else{
          pdf_text2[count-1] <-
            paste(
              pdf_text2[count-1],
              pdf_text[i]
            )
        }
      }
    }
    pdf_text3 <- c()
    # trying to split into multiple columns based on spaces
    for(i in 1:length(pdf_text2)){
      tmp <-
        pdf_text2[i] %>%
        count_spaces()

      # look for three spaces or more
      if(!grepl("   ", pdf_text2[i])){
        next()
      }else{
        tmp <-
          pdf_text2[i] %>%
          strsplit("   ") %>%
          unlist() %>%
          stringr::str_squish() %>%
          # get rid of just pipes or just emptiness
          .[. != "|"] %>%
          .[. != ""]
      }
    }

    pdf_text2 <-
      pdf_text2 %>%
      stringr::str_squish()
  }
