require(rvest)

url = read_html("http://www.oddsshark.com/nba/odds")

books = url %>%
  html_nodes("img") %>%
  html_attr("alt")
books = books[which(books=="Opening"):which(books=="Wynn")]

matchups = url %>%
            html_nodes(".op-matchup-team") %>%
              html_text()

dat = url %>%
        html_nodes(".op-item") %>%
         html_attr("data-op-moneyline")

dat = dat[!is.na(dat)]
test = gsub(pattern = '\"', replacement = "", dat)
test = gsub("[{}]", "", test)

segment = "firstquarter:"

end_ind = sapply(gregexpr(",", substring(test, regexpr(segment, test))), min)+regexpr(segment, test)-1
end_ind[end_ind==-1] = 1000000
ml = as.numeric(substring(test, regexpr(segment, test)+nchar(segment), end_ind-1))

ml_ls = list()
for (i in 1:(length(matchups)/2)) {
  ml_ls[[i]] = data.frame(matrix(data = ml[((i-1)*(length(books)*2)+1):(i*length(books)*2)], nrow = 2, ncol = length(books)))
  rownames(ml_ls[[i]]) = matchups[(i*2-1):(i*2)]
  colnames(ml_ls[[i]]) = books
}
ml_ls


test
sapply(gregexpr(",", substring(test, unlist(gregexpr("fullgame:", test)))), min)


