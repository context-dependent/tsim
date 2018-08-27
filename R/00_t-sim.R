#' T-test the difference in means of multiple variables by an explanatory
#' variable. Groups in input data frame are respected in output.
#'
#' @param dat
#' @param .ind
#' @param .exp
#' @param order
#' @param .grp
#' @param reps
#' @param browse
#'
#' @return
#' @export
#'
#' @examples
t_sim <- function(dat,
                  .ind,
                  .exp,
                  order,
                  .grp = groups(dat),
                  reps = 500,
                  browse = FALSE) {

  if(browse) browser()

  ind <- .ind
  exp <- enquo(.exp)

  if(is.null(.grp)) {

    grp <- quo(var)

  } else {

    grp <- c(quo(var), .grp)

  }

  if(is.null(dat$var)) {

    dat_00 <- dat %>%
      select(!!!c(ind, exp, .grp)) %>%
      gather(var, val, !!!ind) %>%
      mutate(
        exp = !!exp
      ) %>%
      mutate(val = as.numeric(val))

  } else {

    dat_00 <- dat %>%
      select(!!!c(ind, exp, grp)) %>%
      mutate(
        exp = as.factor(!!exp)
      ) %>%
      mutate(val = as.numeric(val))
    cat("\nUsing existing var, val in dat")

  }

  dat_01 <-

    dat_00 %>%

      filter(!is.na(val)) %>%
      group_by(!!!grp) %>%
      nest() %>%
      mutate(
        exp_var = data %>%
          map_lgl(
            ~ all(order %in% (.x %>% pull(!!exp)))
          ),
        ind_var = data %>%
          map_lgl(
            ~ length(unique(.x %>% pull(!!exp))) > 1
          )
      )

  dat_01_rejected <-

    dat_01 %>%

      filter(
        !exp_var,
        !ind_var
      )

  dat_02 <-

    dat_01 %>%

      filter(
        exp_var,
        ind_var
      )

  cat("Rejected cases:\n\n")

  print(dat_01_rejected)

  dat_02 <-

    dat_02 %>%

      mutate(
        prop_sim = data %>%
          map(
          ~ infer::specify(
              .x,
              val ~ exp
            ) %>%
            infer::generate(
              reps = reps,
              type = "bootstrap"
            ) %>%
            infer::calculate(
              stat = "diff in means",
              order = order
            ) %||%

            tibble()
        )
      )

  dat_02 <-

    dat_02 %>%

      mutate(
        prop_test = data %>%
          map(
          ~ infer::t_test(
              .x,
              val ~ exp,
              order = order
            )
          )
      )

  dat_02_est <-

    dat_00 %>%
    group_by(!!!c(grp, exp)) %>%
    summarize(
      val_bar = mean(val, na.rm = TRUE)
    ) %>%
    spread(
      !!exp,
      val_bar
    ) %>%
    mutate(
      diff_obs = !!sym(order[1]) - !!sym(order[2])
    )


  dat_02_sim <-

    dat_02 %>%
    select(!!!c(grp, quo(prop_sim))) %>%
    unnest()

  dat_02_test <-

    dat_02_est %>%
      left_join(
        dat_02 %>%
        select(!!!c(grp, quo(prop_test))) %>%
        unnest()
      )

  list(
    sim = dat_02_sim,
    test = dat_02_test
  )

}
