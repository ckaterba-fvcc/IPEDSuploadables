#' Make Graduation Rates Part C
#'
#' @param df A dataframe of student/degree information
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_gr2_part_D <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #everyone
  partD_line10 <- df %>%
                    dplyr::mutate(LINE = 10) %>%
                    dplyr::select("UNITID",
                                  "PELLGRANT",
                                  "DIRECTLOAN",
                                  "LINE")


  #everyone by 150 and exclusions
  partD_toline45 <- df %>%
                    dplyr::mutate(LINE = case_when(.data$COMPLETED150 == 1 ~ 29,
                                                   .data$ISEXCLUSION == 1 ~ 45)
                                  ) %>%
                    dplyr::select("UNITID",
                                  "PELLGRANT",
                                  "DIRECTLOAN",
                                  "LINE")

  #put it together
  partD <- dplyr::bind_rows(partD_line10,
                            partD_toline45) %>%
           #remove extraneous rows
           dplyr::filter((.data$PELLGRANT == 1 | .data$DIRECTLOAN == 1)) %>%
           dplyr::filter(!is.na(.data$LINE)) %>%
           #clean up Pell vs DIRECTLOAN
           dplyr::mutate(DIRECTLOANONLY = case_when(.data$PELLGRANT == 1 ~ 0,
                                                    .data$DIRECTLOAN == 1 ~ 1,
                                                    TRUE ~ 0)
                         ) %>%
           #aggregate and count
           dplyr::group_by(.data$UNITID,
                           .data$LINE) %>%
           dplyr::summarize(TOTALPELL = sum(.data$PELLGRANT, na.rm = TRUE),
                            TOTALLOAN = sum(.data$DIRECTLOANONLY, na.rm = TRUE)) %>%
           dplyr::ungroup() %>%
           #format for upload
           dplyr::transmute(UNITID = .data$UNITID,
                            SURVSECT = "GR2",
                            PART = "D",
                            LINE = .data$LINE,
                            PELLGRANT_RCPT = .data$TOTALPELL,
                            DIRECTLOAN_RCPT = .data$TOTALLOAN
           )

}
