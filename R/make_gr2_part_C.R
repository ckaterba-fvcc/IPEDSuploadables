#' Make Graduation Rates 2-Year College Part C
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

make_gr2_part_C <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  #everyone
  partC_initial <- df %>%
                    dplyr::filter(.data$COMPLETED100 == 1) %>%
                    dplyr::mutate(COMP10011 = if_else(.data$CURRENTPROGRAMTYPE == 1, 1, 0)
                                  , COMP10012 = if_else(.data$CURRENTPROGRAMTYPE == 2, 1, 0)
                                  ) %>%
                    dplyr::select("UNITID"
                                  , "COMP10011"
                                  , "COMP10012"
                                  )



  #put it together
  partC <- dplyr::bind_rows(partC_initial) %>%
           #aggregate and count
           dplyr::group_by(.data$UNITID,
                           ) %>%
           dplyr::summarize(COMP10011 = sum(.data$COMP10011, na.rm = TRUE),
                            COMP10012 = sum(.data$COMP10012, na.rm = TRUE)) %>%
           dplyr::ungroup() %>%
           #format for upload
           dplyr::transmute(UNITID = .data$UNITID,
                            SURVSECT = "GR2",
                            PART = "C",
                            COMP10011 = .data$COMP10011,
                            COMP10012 = .data$COMP10012
           )

}
