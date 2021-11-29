#' Count number of spaces preceding a character input
#'
#' @description For a given \code{character} input, split based on spaces (\code{" "}), and output a \code{dataframe}
#' with the number of preceding spaces before each word.
#' @param x A \code{character} input to be split.
#' @param combine_single_spaces A \code{logical} input. Should strings separated by only 1 space be recombined? Defaults to \code{FALSE}.
#' @return A \code{data.frame}
#' @author Matt Lewis
#' @keywords internal

count_spaces <-
  function(x, combine_single_spaces = FALSE){
    # check inputs
    assertthat::assert_that(assertthat::is.string(x),
                            assertthat::is.flag(combine_single_spaces))

    # split based on spaces
    x_split <-
      x %>%
      strsplit(" ") %>%
      unlist() %>%
      # get rid of just pipes
      .[.!= "|"]
    preceding_spaces <- c()
    count <- 0
    for(i in 1:length(x_split)){
      if(x_split[i] == ""){
        count <- count +1
        next()
      }else{
        preceding_spaces <-
          c(preceding_spaces, count)
        count <- 1
      }
    }
    ret <-
      data.frame(
        string = x_split[x_split != ""],
        preceding_spaces = preceding_spaces
      )

    if(combine_single_spaces){
      ret2 <- ret[1,]
      count <- 1
      for(i in 2:nrow(ret)){
        if(ret$preceding_spaces[i] == 1){
          ret2$string[count] <-
            paste(
              ret2$string[count],
              ret$string[i]
            )
        }else{
          count <- count +1
          ret2[count,] <- ret[i,]
        }
      }
      ret<- ret2
    }

    return(ret)
}


#' Assemble pdf data into text sections
#'
#' @description For data read in by \code{pdftools::pdf_data()}, assemble into text sections based on x and y
#' positioning, height and width.
#' @param x A \code{list} of \code{data.frames} as read by \code{pdftools::pdf_data()}.
#' @return A \code{list}.
#' @author Matt Lewis
#' @keywords internal
assemble_pdf_text <-
  function(x){
    # Check inputs
    assertthat::assert_that(
      is.list(x),
      is.data.frame(x[[1]])
      )
    assertthat::assert_that(
      all(names(x[[1]]) == c("width", "height", "x", "y", "space", "text", "font_name", "font_size")),
      msg =
        paste0(
          '`x` is not in the right format.\n',
          'Expected column names: width, height, x, y, space, text, font_name, font_size\n',
          'Actual column names: ',
          paste(names(x[[1]]), collapse = ", ")
        )
    )

    # get rid of rows which are just pipes
    x <-
      x %>%
      lapply(
        function(xx){
          xx <-
            xx %>%
            dplyr::filter(
              text != "|"
            )
        }
      )


    # for each sheet of the pdf
    x2 <- list()
    for(i in 1:length(x)){
      tmp <-
        x[[1]]
      x2[[i]] <- tmp[0,]

      # for each row
      for(j in 1:length(unique(tmp$y))){
        tmp2 <-
          tmp %>%
          dplyr::filter(y == unique(tmp$y)[j])

        cut_breaks <-
          which(tmp2$space == F)
        for(k in 1:length(cut_breaks)){
          if(k==1){
            1:cut_breaks[k]
          }else{
            (cut_breaks[k-1]+1):cut_breaks[k]
          }
        }

          cut(
            1:nrow(tmp2),
            breaks =
              ifelse(1 %in% ,
                     list(which(tmp2$space == F)),
                     list(1, which(tmp2$space == F))) %>%
              unlist(),
            right =
              ifelse(
                1 %in% which(tmp2$space == F),
                F, T),
            include.lowest = T,
            labels = F
          )
          # group based on whether followed by a space - if not then end of section
          dplyr::mutate(
            group =
              ifelse(
                nrow(.[.$space == F,]) > 1L,
                ,
                1
              )
          )

        for(k in 1:length(unique(tmp2$group))){
          tmp3 <-
            tmp2 %>%
            dplyr::filter(group == unique(group)[k])

          tmp3$text[1]<-
            tmp3 %>%
            dplyr::pull(text) %>%
            paste(collapse = " ")

          tmp3 <-
            tmp3 %>%
            dplyr::mutate(
              width = NA,
              space = F
            ) %>%
            dplyr::select(-group) %>%
            dplyr::slice(1)

          x2[[i]] <-
            rbind(x2[[i]], tmp3)
        }
      }

    }
  }
