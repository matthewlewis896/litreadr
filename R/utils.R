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


#' Estimate space width
#'
#' @description Estimate width of a space in a given font.
#' @param x A \code{data.frame} or \code{tibble} passed by \code{split_pdf_text()}.
#' @return A \code{data.frame} with a column for average space width added.
#' @author Matt Lewis
#' @keywords internal
estimate_space_width <-
  function(x){
    assertthat::assert_that(
      is.data.frame(x),
      all(c("font_name", "font_size", "width", "height") %in% names(x))
    )

    x <-
      x %>%
      dplyr::group_by(font_name, font_size) %>%
      dplyr::mutate(
        # get ratio of text width to pdf width to width of text rendering in figure window
        text_ratio = mean(rel_width / graphics::strwidth(text, "figure")),
        # use this to work out what a space would correspond to
        space_rel_width =
          graphics::strwidth(" ", "figure") *text_ratio,

        space_width = ceiling(space_rel_width*(width/rel_width))
      ) %>%
      dplyr::ungroup()

    return(x)
  }
