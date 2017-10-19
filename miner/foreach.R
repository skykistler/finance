

url <- 'localhost:3000'
symbol <- 'MU'
span   <- 'day'


########################

historicals <- fromJSON( getURL(paste0(url, '/?symbol=', symbol, '&span=', span)) )
