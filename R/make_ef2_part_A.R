#' Make Fall Enrollment Part A
#'
#' @description Breakdown of students level and demographics; also by designated CIPs in required years
#'
#' @param df A dataframe of student information
#'
#' @importFrom rlang .data
#'
#' @importFrom dplyr select group_by summarize arrange transmute n mutate bind_rows
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_ef2_part_A <- function(df) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partA_prep <- df %>%
                dplyr::select("UNITID",
                              "ISFULLTIME",
                              "ISFIRSTTIME",
                              "ISTRANSFER",
                              "ISDEGREECERTSEEKING",
                              "STUDENTLEVEL",
                              "RACEETHNICITY",
                              "SEX"
                              ) %>%
                dplyr::mutate(LINE = dplyr::case_when(
                                      .data$ISFULLTIME == 1 & .data$ISFIRSTTIME == 1 & .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 1,
                                      .data$ISFULLTIME == 1 & .data$ISTRANSFER == 1 & .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 2,
                                      .data$ISFULLTIME == 1 & .data$ISFIRSTTIME == 0 & .data$ISTRANSFER == 0 & .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 3,
                                      .data$ISFULLTIME == 1 & .data$ISDEGREECERTSEEKING == 0 & .data$STUDENTLEVEL == "Undergraduate" ~ 7,
                                      .data$ISFULLTIME == 0 & .data$ISFIRSTTIME == 1 & .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 15,
                                      .data$ISFULLTIME == 0 & .data$ISTRANSFER == 1 & .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 16,
                                      .data$ISFULLTIME == 0 & .data$ISFIRSTTIME == 0 & .data$ISTRANSFER == 0 & .data$ISDEGREECERTSEEKING == 1 & .data$STUDENTLEVEL == "Undergraduate" ~ 17,
                                      .data$ISFULLTIME == 0 & .data$ISDEGREECERTSEEKING == 0  & .data$STUDENTLEVEL == "Undergraduate" ~ 21,
                                    )
                              )

  partA_all <- partA_prep %>%
               dplyr::mutate(MAJORCIP = "99.0000") %>%
               dplyr::group_by(.data$UNITID,
                               .data$MAJORCIP,
                               .data$LINE,
                               .data$RACEETHNICITY,
                               .data$SEX) %>%
               dplyr::summarize(COUNT = n()) %>%
               #sort for easy viewing
               dplyr::arrange(.data$LINE,
                              .data$RACEETHNICITY,
                              .data$SEX) %>%
               dplyr::ungroup()

  partA <- partA_all %>%
      #format for upload
      dplyr::transmute(UNITID = .data$UNITID,
                       SURVSECT = "EF2",
                       PART = "A",
                       CIPCODE = .data$MAJORCIP,
                       LINE = .data$LINE,
                       RACE = .data$RACEETHNICITY,
                       SEX = .data$SEX,
                       COUNT = .data$COUNT
                      )

}
