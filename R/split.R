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
        dplyr::relocate(row, .after = dplyr::last_col()) %>%
        # get rid of space column because it's actually not useful
        dplyr::select(-space)

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
            new_font_size =
              ifelse(
                dplyr::row_number()==1,
                T,
                ifelse(
                  font_size == dplyr::lag(font_size),
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
                #if not
                ifelse(
                  # if new font size then new group
                  tmp2$new_font_size[k] == T,
                  tmp2$group[k-1]+1,
                  # if not
                  ifelse(
                    # if space gap >2x estimated space size then make a new group
                    tmp2$space_gap[k] > (2*tmp2$space_width[k-1]),
                    tmp2$group[k-1]+1,
                    # otherwise same group
                    tmp2$group[k-1]
                  )
                )
              )
            )
        }

        tmp2 <-
          tmp2 %>%
          # get rid of rows which are just pipes
          dplyr::filter(
            text != "|"
          ) %>%
          # get rid of space gap column and new font columns now not needed
          dplyr::select(-space_gap, -new_font, -new_font_size)

        tmp3 <-
          tmp2 %>%
          dplyr::group_by(group) %>%
          dplyr::mutate(
            xmin = min(xmin),
            xmax = max(xmax),
            width = xmax - xmin,
            rel_width = width / size$width[i],
            ymin = min(ymin),
            ymax = max(ymax),
            height = ymin - ymax,
            rel_height = height / size$height[i],
            text = paste(text, collapse = " ")
          ) %>%
          dplyr::distinct() %>%
          dplyr::ungroup() %>%
          dplyr::select(-group)

        x2[[i]] <-
          rbind(x2[[i]], tmp3)
      }
      ## Identify cross-row grouping
      # first change the way we record row
      x2[[i]] <-
        x2[[i]] %>%
        dplyr::mutate(
          row_start = row, row_end = row
        ) %>%
        dplyr::select(-row)
      for(j in unique(x2[[i]]$row_start)){
        if(j ==1){
          next()
        }else{
          tmp <-
            x2[[i]] %>%
            dplyr::filter(row_start == j)
          tmp_prev <-
            x2[[i]] %>%
            dplyr::filter(row_end == j-1)
          for(k in 1:nrow(tmp)){
            tmp_match <-
              tmp_prev %>%
              dplyr::filter(
                # exact match of font name and font size
                font_name == tmp$font_name[k],
                font_size == tmp$font_size[k],
                # exact match of xmin OR
                # match within +2 spaces if bulleted
                # putting minimum character cut off of 2 so as to not match
                # to just numbers or just `2.`
                xmin == tmp$xmin[k] |
                  ((tmp$xmin[k] < (xmin + 2* space_width)) &
                   tmp$xmin[k] > xmin &
                   grepl("[0-9]",strsplit(text, 1, 1)) &
                   nchar(text) >2L),
                # check that xmax !> previous (within a space)
                # as this would not suggest col layout
                (xmax+space_width) >= tmp$xmax[k]
              )
            # see if we match
            if(nrow(tmp_match) > 0L){
              new_tmp <-
                tmp_match %>%
                rbind(tmp[k,]) %>%
                dplyr::mutate(
                  dplyr::across(
                    .cols = c("width", "rel_width",
                              "xmax", "ymin", "row_end"),
                    .fns = max
                  ),
                  dplyr::across(
                    .cols = c("height", "rel_height"),
                    .fns = sum
                  ),
                  dplyr::across(
                    .cols = c("ymax", "row_start"),
                    .fns = min
                  ),
                  text =
                    ifelse(
                      # see if we end with a hyphon and cut it
                      substr(tmp_match$text,
                             nchar(tmp_match$text),
                             nchar(tmp_match$text)
                             )!= "-",
                      paste(text, collapse = " "),
                      paste0(
                        substr(tmp_match$text,
                               1,
                               nchar(tmp_match$text)
                        ),
                        tmp$text[k]
                      )
                    )
                  ) %>%
                dplyr::distinct()

              x2[[i]] <-
                x2[[i]] %>%
                rbind(new_tmp) %>%
                # remove old rows
                dplyr::anti_join(tmp_match) %>%
                dplyr::anti_join(tmp[k,]) %>%
                suppressMessages()
            }
          }
        }
      }
      x2[[i]] <-
        x2[[i]] %>%
        dplyr::arrange(row_start, xmin)
    }
  }
