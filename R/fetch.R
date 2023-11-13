#' Fetch Channel Conversations
#'
#' Fetch all conversations that are available for the given channels. If
#' channels are not provided, all conversations for all non-archived channels
#' are fetched.
#'
#' @inheritParams .normalize_channels
#'
#' @return A named list of the `conversations.history` objects returned by
#'   [slackthreads::conversations], one for each channel. The names of are the
#'   channel names. The list has additional class `multichannel.history`.
#' @export
fetch_all_channel_conversations <- function(channels) {
  # I'm going to let slackteams/slackcalls handle error messages if they don't
  # have a token.
  channels <- .normalize_channels(channels)

  conversations <- purrr::map(
    channels,
    slackthreads::conversations
  )

  class(conversations) <- c("multichannel.history", "list")
  return(conversations)
}

#' Clean Up channels parameter
#'
#' @param channels An optional character vector of channel names or ids.
#'
#' @return A character vector of channel ids, with the channel names as names of
#'   the vector.
#' @keywords internal
.normalize_channels <- function(channels) {
  if (missing(channels)) {
    # By default we get all non-archived channels (not IMs).
    channels <- dplyr::filter(
      slackteams::get_conversations_list(
        type = c("public_channel", "private_channel")
      ),
      !.data$is_archived
    )
    channels <- rlang::set_names(channels$id, channels$name)
  } else {
    # Even if it's already named, we need to validate it.
    channels <- unname(channels)
    channels <- vctrs::vec_cast(channels, character())

    all_channels <- slackteams::get_conversations_list(
      type = c("public_channel", "private_channel")
    )
    all_channels <- rlang::set_names(all_channels$id, all_channels$name)
    if (all(channels %in% all_channels)) {
      channels <- all_channels[all_channels %in% channels]
    } else if (all(channels %in% names(all_channels))) {
      channels <- all_channels[names(all_channels) %in% channels]
    } else {
      cli::cli_abort(
        c(
          "{.var channels} must contain valid names or ids",
          "Cannot find {.var channels} on the active Slack team."
        )
      )
    }
  }
  return(channels)
}

#' Fetch Replies for Everything
#'
#' Fetch replies for all conversations in an object returned by
#' [fetch_all_channel_conversations()].
#'
#' @param conversations An object returned by
#'   [fetch_all_channel_conversations()], or (soon) a list with a similar
#'   structure. And I'll probably allow a tidy conversation tibble before I
#'   consider this package done.
#'
#' @return A list of the objects returned by
#'   [slackthreads::all_conversation_replies()]. The names of the list are the
#'   channel from which the conversations were fetched, and the object has
#'   additional class `multichannel.replies`.
#' @export
fetch_all_conversation_replies <- function(conversations) {
  UseMethod("fetch_all_conversation_replies")
}

#' @export
fetch_all_conversation_replies.multichannel.history <- function(conversations) {
  threads <- purrr::map(
    conversations,
    slackthreads::all_conversation_replies
  )
  class(threads) <- c("multichannel.replies", "list")
  return(threads)
}

#' @export
fetch_all_conversation_replies.list <- function(conversations) {
  # While this is currently identical to the default method, I'm carving it off
  # to remember that I really should implement this method, and just do more
  # checks to make sure the list has the required structure.
  cli::cli_abort(
    c(
      "{.var conversations} must be a `multichannel.replies` object.",
      "Retrieve conversations with `fetch_all_channel_conversations()`"
    )
  )
}

#' @export
fetch_all_conversation_replies.default <- function(conversations) {
  cli::cli_abort(
    c(
      "{.var conversations} must be a `multichannel.replies` object.",
      "Retrieve conversations with `fetch_all_channel_conversations()`"
    )
  )
}
