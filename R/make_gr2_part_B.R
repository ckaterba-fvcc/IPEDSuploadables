#' Make Graduation Rates 2-Year Colleges Part B
#'
#' @param df A dataframe of student/degree information
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by summarize ungroup bind_rows arrange transmute n
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_gr2_part_B <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  ###
  # Lines to Prep:
  # 10 - Revised cohort of full-time, first-time degree or certificate seeking students
  # 11 - Completers of programs of less than 2 years within 150% of normal time
  # 12 - Completers of programs of at least 2 but less than 4 years within 150% of normal time
  #
  # 30 - Total transfer-out students (non-completers)
  # 45 - Total exclusions
  # 51 - Still enrolled
  #
  ###

  #####
  #prep part B (revised cohort totals)
  #prep line 10 (everyone by RE/SEX)
  partB_line10 <- df %>%
                          dplyr::mutate(LINE = 10) %>%
                          dplyr::select("UNITID",
                                        "RACEETHNICITY",
                                        "SEX",
                                        "LINE")

  #####
  #prep section 2
  #lines 11 & 12 (all completers by current program type/pct + RE/SEX)
  partB_toline12 <- df %>%
                              dplyr::mutate(LINE = case_when(
                                                        .data$CURRENTPROGRAMTYPE == 1 & .data$COMPLETED150 == 1 ~ 11,
                                                        .data$CURRENTPROGRAMTYPE == 2 & .data$COMPLETED150 == 1 ~ 12
                                                      )
                                            ) %>%
                              dplyr::select("UNITID",
                                            "RACEETHNICITY",
                                            "SEX",
                                            "LINE")


  ###
  #prep part B
  #lines 30-51 (non-completers by RE/SEX)
  partB_toline51 <- df %>%
                             dplyr::filter(.data$ENTERINGPROGRAMTYPE < 3 ) %>%
                             dplyr::mutate(
                                           LINE = case_when(
                                             .data$ISTRANSFEROUT == 1 ~ 30,
                                             .data$ISEXCLUSION == 1 ~ 45,
                                             .data$ISSTILLENROLLED == 1 ~ 51
                                           )
                             ) %>%
                             dplyr::select("UNITID",
                                           "RACEETHNICITY",
                                           "SEX",
                                           "LINE")


  #put it all together and count things up
  partB <- dplyr::bind_rows(partB_line10,
                            partB_toline12,
                            partB_toline51,
                            ) %>%
          #remove extraneous rows
          dplyr::filter(!is.na(.data$LINE)) %>%
          #aggregate
          dplyr::group_by(.data$UNITID,
                          .data$LINE,
                          .data$RACEETHNICITY,
                          .data$SEX) %>%
          dplyr::summarize(COUNT = dplyr::n()) %>%
          dplyr::ungroup() %>%
          #sort for easy viewing
          dplyr::arrange(.data$LINE,
                         .data$RACEETHNICITY,
                         .data$SEX) %>%
          #format for upload
          dplyr::transmute(UNITID = .data$UNITID,
                           SURVSECT = "GR2",
                           PART = "B",
                           LINE = .data$LINE,
                           RACE = .data$RACEETHNICITY,
                           SEX = .data$SEX,
                           COUNT = .data$COUNT
          )

}
