#' Make Fall Enrollment Part H (gender details)
#'
#' @param df A dataframe of student enrollment information
#' @param ugender A boolean: TRUE means you are collecting and able to report
#'   "another gender" for undergraduate completers, even if you have no (or few)
#'   such students. Set as FALSE if necessary
#'
#' @importFrom rlang .data
#' @importFrom dplyr select group_by summarize ungroup arrange transmute n
#'   distinct
#' @importFrom utils write.table
#' @importFrom stringr str_to_upper
#'
#' @return A dataframe with the required IPEDS structure for this survey part
#' @export
#'

make_ef2_part_H <- function(df, ugender) {

  colnames(df) <- stringr::str_to_upper(colnames(df))

  partH_counts <- df %>%
    dplyr::select("UNITID",
                  "STUDENTID",
                  "STUDENTLEVEL",
                  "GENDERDETAIL"  #Binary = 1, 2;  Unknown = 3, Another = 4
    ) %>%
    #break into UG and GR levels
    dplyr::mutate(UGPB = 'UG') %>%
    dplyr::select(-"STUDENTLEVEL") %>%
    #deduplicate
    dplyr::distinct() %>%
    #aggregate and count
    dplyr::group_by(.data$UNITID,
                    .data$UGPB,
                    .data$GENDERDETAIL) %>%
    dplyr::summarize(COUNT = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #sort for easy viewing
    dplyr::arrange(.data$UGPB, .data$GENDERDETAIL)

  #set up the final DF
  partH <- data.frame(UNITID = unique(partH_counts$UNITID),
                      SURVSECT = "EF2",
                      PART = "H",
                      EFGU01 = 0,  #UG detail reporting?
                      EFGU011 = 0, #UG Unknown
                      EFGU012 = 0 #UG Another
                      )

  #ugly way to get the right counts in each bit.
  #I am sure there is a much nicer way to do it with a pivot
  # and maybe with a dummy table for all values. I dunno. This works.

  # #No UG in the data
  if(ugender == FALSE){
    partH$EFGU01 <- 2
    if(sum(partH_counts$UGPB == 'UG' &
           partH_counts$GENDERDETAIL == 3) == 1){
      partH$EFGU011 <- partH_counts$COUNT[partH_counts$UGPB == 'UG' &
                                           partH_counts$GENDERDETAIL == 3]
    }
    partH$EFGU012 <- -2

    # #UG in the data, another being reported, another/unknown might or might not exist
  } else {
    partH$EFGU01 <- 1

    if(sum(partH_counts$UGPB == 'UG' &
           partH_counts$GENDERDETAIL == 3) == 1){
      partH$EFGU011 <- partH_counts$COUNT[partH_counts$UGPB == 'UG' &
                                           partH_counts$GENDERDETAIL == 3]
    }
    if(sum(partH_counts$UGPB == 'UG' &
           partH_counts$GENDERDETAIL == 4) == 1){
      partH$EFGU012 <- partH_counts$COUNT[partH_counts$UGPB == 'UG' &
                                           partH_counts$GENDERDETAIL == 4]
    }
    #BUT -- New in 2023 - mask if < 5 and set initial inquiry as "small N"
    if(partH$EFGU012 < 5){
      partH$EFGU012 <- -2
      partH$EFGU01 <- 3
    }
  }

  return(partH)

}



