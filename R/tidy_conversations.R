#' Rectangle Conversation Data
#'
#' @param conversations A list returned by [slackthreads::conversations()].
#'   Right now we actually are working with a list of such lists (one list per
#'   channel). I'll likely add methods to this to deal with that.
#' @param threads An optional list the same shape as conversations, with replies
#'   to those conversations.
#'
#' @return A tibble of question data.
#' @export
tidy_conversations <- function(conversations, threads = NULL) {
  convos_tbl <- conversations |>
    .rectangle_conversations() |>
    .reply_zeros()

  if (length(threads) == length(conversations)) {
    convos_tbl <- .append_replies(convos_tbl, threads)
  }

  # I can't remove the useless rows until AFTER adding threads.
  convos_tbl <- convos_tbl |>
    .remove_useless() |>
    .simplify_pins() |>
    .simplify_edited() |>
    .simplify_files()

  return(convos_tbl)
}

.rectangle_conversations <- function(conversations) {
  convos_tbls <- purrr::map(
    conversations,
    .rectangle_conversation_single
  )

  return(
    purrr::list_rbind(
      convos_tbls,
      names_to = "channel_name"
    )
  )
}

.rectangle_conversation_single <- function(conversation) {
  # Drop the special class since we don't use it.
  class(conversation) <- "list"
  this_tbl <- tibble::tibble(
    channel_id = attr(conversation, "channel"),
    conversations = as.list(conversation)
  ) |>
    tidyr::unnest_wider("conversations")

  # Drop the special classes from columns, too.
  for (col_name in colnames(this_tbl)) {
    class(this_tbl[[col_name]]) <- setdiff(
      class(this_tbl[[col_name]]),
      c("conversation", "reply")
    )
  }

  return(this_tbl)
}

.reply_zeros <- function(convos_tbl) {
  if ("reply_count" %in% colnames(convos_tbl)) {
    convos_tbl$reply_count <- tidyr::replace_na(
      convos_tbl$reply_count, 0L
    )
  }
  if ("reply_users_count" %in% colnames(convos_tbl)) {
    convos_tbl$reply_users_count <- tidyr::replace_na(
      convos_tbl$reply_users_count, 0L
    )
  }

  return(convos_tbl)
}

.remove_useless <- function(convos_tbl) {
  # Ideally most/all of this should go behind flags, so this can be more
  # generally useful (not R4DS-specific).

  # This used to be hidden in internal data. It really should be an argument,
  # with defaults. Really these are only "bad" when I'm specifically looking for
  # questions for R4DS.
  .bad_subtypes <- c(
    "channel_join",
    "channel_name",
    "channel_purpose", # Long term we might want this but not at first.
    "channel_topic", # Long term we might want this but not at first.
    "bot_add",
    "bot_message",
    "thread_broadcast" # We should probably keep this and deal with it.
  )

  # Some columns are specific to the user who fetched this data and should be
  # removed. And some are never different.
  convos_tbl$subscribed <- NULL
  convos_tbl$last_read <- NULL
  convos_tbl$team <- NULL
  convos_tbl$is_locked <- NULL
  convos_tbl$upload <- NULL
  convos_tbl$display_as_bot <- NULL

  if ("user" %in% colnames(convos_tbl)) {
    convos_tbl <- convos_tbl |>
      dplyr::filter(.data$user != "USLACKBOT")
  }
  if ("type" %in% colnames(convos_tbl)) {
    if (any(convos_tbl$type != "message")) {
      stop(
        "Weird type(s) found! Types: ",
        sort(setdiff(unique(convos_tbl$type), "message"))
      )
    }
    convos_tbl$type <- NULL
  }
  if ("subtype" %in% colnames(convos_tbl)) {
    convos_tbl <- convos_tbl |>
      dplyr::filter(!(.data$subtype %in% .bad_subtypes))
    if (all(is.na(convos_tbl$subtype))) {
      convos_tbl$subtype <- NULL
    }
    if (
      "purpose" %in% colnames(convos_tbl) &&
      all(is.na(convos_tbl$purpose))
    ) {
      convos_tbl$purpose <- NULL
    }
    if (
      "topic" %in% colnames(convos_tbl) &&
      all(is.na(convos_tbl$topic))
    ) {
      convos_tbl$topic <- NULL
    }
  }
  if (
    "root" %in% colnames(convos_tbl) &&
    purrr::every(
      convos_tbl$root,
      \(convos_tbl) all(is.null(unlist(convos_tbl)))
    )
  ) {
    convos_tbl$root <- NULL
  }

  if ("hidden" %in% colnames(convos_tbl)) {
    if (any(isTRUE(convos_tbl$hidden))) {
      warning("A hidden message wasn't removed!")
    } else {
      convos_tbl$hidden <- NULL
    }
  }

  if ("parent_user_id" %in% colnames(convos_tbl)) {
    if (any(!is.na(convos_tbl$parent_user_id))) {
      warning("Non-NA parent_user_id!")
    } else {
      convos_tbl$parent_user_id <- NULL
    }
  }

  return(convos_tbl)
}

.simplify_pins <- function(convos_tbl) {
  convos_tbl$pinned_to <- convos_tbl$pinned_to |>
    purrr::map_chr(
      \(value) {
        if (is.null(value)) {
          return(NA_character_)
        } else {
          return(unlist(value))
        }
      }
    )

  # "pinned_to" and "pinned_info$channel" are redundant. Getting the info about
  # who pinned it and when is useful, though.
  convos_tbl <- convos_tbl |>
    tidyr::hoist(
      "pinned_info",
      pinned_by = "pinned_by",
      pinned_ts = "pinned_ts"
    ) |>
    dplyr::select(-"pinned_info")

  return(convos_tbl)
}

.simplify_edited <- function(convos_tbl) {
  if (length(convos_tbl$edited)) {
    convos_tbl <- convos_tbl |>
      dplyr::mutate(
        edited_at = purrr::map_chr(
          .data$edited,
          \(this_edit) {
            if (length(this_edit) < 2) {
              return(NA_character_)
            } else {
              return(
                this_edit[[2]]
              )
            }
          }
        ),
        .after = "edited"
      ) |>
      dplyr::mutate(
        edited = purrr::map_lgl(
          .data$edited,
          \(x) !is.null(x)
        )
      )
  } else {
    convos_tbl <- convos_tbl |>
      dplyr::mutate(
        edited = FALSE,
        edited_at = NA_character_
      )
  }
  return(convos_tbl)
}

.simplify_files <- function(convos_tbl) {
  # TODO: Implement this. Files have a whoooole lot of redundant information. I
  # think it's mostly for backward compatibility. Get rid of almost all of it.
  return(convos_tbl)
}

.append_replies <- function(convos_tbl, threads) {
  all_replies_list <- purrr::map(
    threads,
    .rectangle_channel_replies
  ) |>
    purrr::list_c()

  convos_tbl <- convos_tbl |>
    dplyr::mutate(replies = all_replies_list)

  return(convos_tbl)
}

.rectangle_channel_replies <- function(channel_threads) {
  return(
    purrr::map(
      channel_threads,
      \(replies) {
        this_tbl <- .rectangle_conversation_single(replies)
        # The thread_ts column is redundant at this point. I might keep it for
        # joining, but for now I want to put these into the tbl as a list
        # column.
        this_tbl$thread_ts <- NULL

        if (
          "parent_user_id" %in% colnames(this_tbl) &&
          length(unique(
            this_tbl$parent_user_id[!is.na(this_tbl$parent_user_id)]
          )) > 1
        ) {
          stop("More than 1 parent user.")
        }
        this_tbl$parent_user_id <- NULL

        # TODO: This will remove thread_broadcasts, which technically shouldn't
        # be removed here.
        this_tbl <- .remove_useless(this_tbl)

        this_tbl$channel_id <- NULL

        return(this_tbl)
      }
    )
  )
}
