#' Send email from within R
#'
#' @param to_email_address A recipient email address.
#' @param cc_email_address A recipient to copy.
#' @param bcc_email_address A recipient to blind copy.
#' @param from_email_address Email address to send from.
#' @param from_email_password Password for the from_email_address.
#' @param from_email_username Username for the from_email_address. If no argument provided the
#' from_email_address will be used.
#' @param attachment_path A path to a file to attach to the outgoing message.
#' @param email_subject A subject for the outgoing message.
#' @param email_body The body of the outgoing message.
#' @param email_body_type Either "plain" or "html". Pass "html" to use custom html formatting
#' in the body of the message
#' @param from_host from_email_address host
#' @param port Port to send the outgoing message through
#'
#' @import emayili
#' @importFrom magrittr "%>%"
#' @return Sends an email with content based on the arguments provided
#' @export
#'
#' @examples \dontrun{send_email(email_body = 'Hello world')}

send_email <- function(to_email_address,
                       cc_email_address = NULL,
                       bcc_email_address = NULL,
                       from_email_address = NULL,
                       from_email_password,
                       from_email_username = NULL,
                       attachment_path = NULL,
                       email_subject = "",
                       email_body = "",
                       email_body_type = "plain",
                       from_host = "smtp.gmail.com",
                       port = 465) {

  if(is.null(from_email_address)) {

    from_email_address <- to_email_address

  }

  if(email_body_type == "html") {

    email <- emayili::envelope() %>%
      emayili::from(from_email_address) %>%
      emayili::to(to_email_address) %>%
      emayili::subject(email_subject) %>%
      emayili::html(email_body)

  } else {

    email <- emayili::envelope() %>%
      emayili::from(from_email_address) %>%
      emayili::to(to_email_address) %>%
      emayili::subject(email_subject) %>%
      emayili::text(email_body)
  }

  if(!is.null(cc_email_address)) {

    email <- email %>%
      emayili::cc(cc_email_address)
  }

  if(!is.null(bcc_email_address)) {

    email <- email %>%
      emayili::bcc(bcc_email_address)
  }

  if(!is.null(attachment_path)) {

    email <- email %>%
      emayili::attachment(attachment_path)
  }

  if(is.null(from_email_username)) {

    from_email_username <- from_email_address
  }

  smtp <- emayili::server(host = from_host,
                          port = port,
                          username = from_email_username,
                          password = from_email_password)

  smtp(email, verbose = T)

}
