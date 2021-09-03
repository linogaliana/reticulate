
#' @export
print.pandas.data.frame <- function(x, ...){
  
  headers <- colnames(x)
  index <- rownames(x)
  
  css_head <- "<table border=\"1\" class=\"dataframe\">\n"
  
  # HEAD ---------------------
  
  headers_formatted <- as.character(
    lapply(headers, function(s) sprintf("     <th>%s</th>\n", s))
  )
  headers_formatted <- paste(headers_formatted, collapse = " ")
  if (!is.null(index)) headers_formatted <- sprintf("     <th></th>\n %s", headers_formatted)
  
  head <- sprintf("<thead>\n    <tr style=\"text-align: right;\">\n %s    </tr>\n  </thead>\n",
                  headers_formatted
  )
  
  # BODY -------------------
  
  bod <- lapply(1:nrow(x), function(i){
    body_formatted <- as.character(x[i,])
    body_formatted <- as.character(
      lapply(body_formatted, function(el) sprintf("     <td>%s</td>\n", el))
    )
    if (!is.null(index)) body_formatted <- c(sprintf("  <th>%s</th>\n", index[i]), body_formatted)
    body_formatted <- paste(body_formatted, collapse = " ")
    body_formatted <- sprintf("    <tr>\n    %s     </tr>\n", body_formatted)
    
  })
  bod <- paste(bod, collapse = " ")
  
  body <- sprintf(" <tbody>\n%s  </tbody>", bod)
  
  
  # COMBINE TOGETHER ----------------
  
  table <- sprintf("%s %s %s %s",
                   css_head, 
                   head,
                   body,
                   "\n </table>")
  
  table
}
