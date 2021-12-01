#' Split pdf data into text sections
#'
#' @description For data read in by \code{pdftools::pdf_data()}, split into text sections based on x and y
#' positioning, height and width.
#' @param x A \code{list} of \code{data.frames} as read by \code{pdftools::pdf_data(font_info = T)}.
#' @param size A \code{data.frame} as read by \code{pdftools::pdf_pagesize()}.
#' @return A \code{list}.
#' @author Matt Lewis
#' @keywords internal
split_pdf_text <-
  function(x, size){
    # Check inputs
    assertthat::assert_that(
      is.list(x),
      is.data.frame(x[[1]]),
      is.data.frame(size)
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

    ## get rid of rows which are just pipes
    # x <-
    #   x %>%
    #   lapply(
    #     function(xx){
    #       xx <-
    #         xx %>%
    #         dplyr::filter(
    #           text != "|"
    #         )
    #     }
    #   )


    # for each sheet of the pdf
    x2 <- list()
    for(i in 1:length(x)){
      tmp <-
        x[[1]] %>%
        # add height range to capture sub- and super-script characters
        dplyr::mutate(
          ymin = y + height -1, # assuming y gives coordinate of top of text box in px from top.
          #-1 for rogue overlaps
          xmax = x +width,
          rel_width = width / size$width[i],
          rel_height = height / size$height[i],
          row = NA
        ) %>%
        dplyr::rename(
          ymax = y,
          xmin = x
        ) %>%
        dplyr::relocate(ymin, .before = ymax,) %>%
        dplyr::relocate(xmax, .after = xmin) %>%
        dplyr::relocate(rel_width, .after = width) %>%
        dplyr::relocate(rel_height, .after = height) %>%
        estimate_space_width() %>%
        dplyr::relocate(row, .after = dplyr::last_col())

      # add grouping
      row_num <- 1
      tmp$row[1] <- row_num
      for(j in 2:nrow(tmp)){
        if(
          # not superscript
          !(tmp$ymin[j] < tmp$ymax[j-1])  &
          # not subscript
          !(tmp$ymax[j] < tmp$ymin[j-1])
        ){
          row_num <- row_num +1
        }
        tmp$row[j] <- row_num
      }
      tmp <-
        tmp %>%
        dplyr::arrange(
          row, xmin
        )

      x2[[i]] <- tmp[0,]

      # for each row
      for(j in unique(tmp$row)){
        tmp2 <-
          tmp %>%
          dplyr::filter(row == j) %>%
          dplyr::mutate(
            new_font =
              ifelse(
                dplyr::row_number()==1,
                T,
                ifelse(
                  font_name == dplyr::lag(font_name),
                  F,
                  T
                )
              ),
            space_gap =
              ifelse(
                dplyr::row_number() == 1,
                0,
                xmin - dplyr::lag(xmax)
              ),
            group = NA
          )

        for(k in 1:nrow(tmp2)){
          tmp2$group[k] <-
            ifelse(
              # row 1 is group 1
              k==1,
              1,
              ifelse(
                # if new font then must be a new group
                tmp2$new_font[k] == T,
                tmp2$group[k-1]+1,
                # if not
                ifelse(
                  # if space gap exceeds estimated space size then make a new group
                  tmp2$space_gap[k] > tmp2$space_width[k-1],
                  tmp2$group[k-1]+1,
                  # otherwise same group
                  tmp2$group[k-1]
                )

              )
            )
        }
        ## find how to split data
        # infer split based on font groupings
        cut_breaks <-
          data.frame(
            breaks = which(tmp2$space == F),
            join = NA
          )

        # check whether these cut breaks should join to previous or next based on x gap
        for(k in 1:nrow(cut_breaks)){
          # if very first item then must attach to  next
          if(cut_breaks$breaks[k] == 1){
            cut_breaks$join[k] <- 1
          }else if(k == nrow(cut_breaks)){
            # if last item then much attach to previous
            cut_breaks$join[k] <- -1
          }else{
            gap_prev <-
              tmp2$xmin[cut_breaks$breaks[k]]-
              tmp2$xmax[cut_breaks$breaks[k]-1]
            gap_next <-
              tmp2$xmin[cut_breaks$breaks[k]+1] -
              tmp2$xmax[cut_breaks$breaks[k]]
            cut_breaks$join[k] <-
              ifelse(
                gap_prev > gap_next,
                1,
                -1
              )
          }
        }
        cut_breaks2 <- list()
        for(k in 1:(nrow(cut_breaks)-1)){
          if(cut_breaks$join[k] == 1){
            cut_breaks2 <-
              cut_breaks2 %>%
              rlist::list.append(
                c(
                  # start
                  ifelse(
                    k==1,
                    1,
                    ifelse(
                      cut_breaks$join[k-1] == -1,
                      cut_breaks$breaks[k-1]+1,
                      cut_breaks$breaks[k]
                    )
                  ):
                    #end
                    ifelse(
                      cut_breaks$join[k+1] == 1,
                      cut_breaks$breaks[k+1]-1,
                      cut_breaks$breaks[k+1]
                    )
                )
              )
          }
        }

        # group
        for(k in 1:length(cut_breaks2)){
          tmp2$group[unlist(cut_breaks2[[k]])] <- k
        }

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
