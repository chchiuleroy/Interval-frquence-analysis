rm(list = ls())

library(rvest)

data = function(url, l, j) {
  
  h = read_html(url)
  date = h%>%html_nodes(
  xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "auto-style5", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]')%>%
    html_text()
  main = h%>%html_nodes(
    xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "auto-style5", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]')%>%
    html_text()
  special = h%>%html_nodes(
    xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "auto-style5", " " )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]')%>%
    html_text()
  dim = length(date) - 1
  main = gsub("[[:space:]]", "", main[-1])%>%strsplit(",") %>%unlist()%>%as.integer()%>%matrix(l, dim)%>%t()
  if (j == 1) {dataset = main} else if (j == 2) {dataset = special[-1]%>%as.integer()%>%as.matrix()} else {dataset = date[-1]}
  return (dataset)
}

sqe = function(z, p) { s1 = 1; s2 = 0; g = {}
  
  for (i in rev(1 : length(z))) {
    
    if (z[i] == p) {g[s1] = s2 + 1; s2 = s2 + 1} else {s2 = 0; s1 = s1 + 1}
    
  }
 return(na.omit(g))
}

count = function(z, num) {
  z = as.numeric(z); l = matrix(NA, num, 1); l[z] = 1; l[-z] = 0
  return(l)
}

final = function(y, l, j, num) {
  
  dataset = rbind(data(url[y, 1], l[y], j), data(url[y, 2], l[y], j))
  k = l[y] + 1
  count_table = sapply(1:dim(dataset)[1], function(x) {count(dataset[x, ], num)})%>%t()
  interval_0 = sapply(1:dim(count_table)[2], function(x) {sqe(count_table[, x], 0)})
  interval_1 = sapply(1:dim(count_table)[2], function(x) {sqe(count_table[, x], 1)})
  mean = sapply(1:num, function(x) {mean(interval_0[[x]])})%>%round()
  max_0 = sapply(1:num, function(x) {max(interval_0[[x]])})%>%as.integer()
  max_1 = sapply(1:num, function(x) {max(interval_1[[x]])})
  last_count = sapply(1:num, function(x) {interval_0[[x]][length(interval_0[[x]])]})%>%as.integer(); last_count[dataset[dim(dataset)[1], ]] = 0
  next_show =  sapply(1:num, function(x) {ppois(last_count[x], mean[x])})
  results = data.frame(rbind(max_0, mean, last_count, next_show), 
                       row.names = c('最長連續未開間距', '平均開出間距', '至今未開出間距', '下期可能開出機率'))
  colnames(results) = sapply(1:length(last_count), function(x) {paste("號碼", x, sep = "")})
  
  return(results)
} 


url = matrix(NA, 4, 2)
url[1, 1] = 'http://www.lotto-8.com/listltobig.asp' ## 大樂透
url[2, 1] = 'http://www.lotto-8.com/listlto539.asp' ## 今彩
url[3, 1] = 'http://www.lotto-8.com/listltodof.asp' ## 大福彩
url[4, 1] = 'http://www.lotto-8.com/listlto.asp' ## 威力彩
url[1, 2] = 'http://www.lotto-8.com/listltobig.asp?indexpage=2&orderby=new'
url[2, 2] = 'http://www.lotto-8.com/listlto539.asp?indexpage=2&orderby=new'
url[3, 2] = 'http://www.lotto-8.com/listltodof.asp?indexpage=2&orderby=new'
url[4, 2] = 'http://www.lotto-8.com/listlto.asp?indexpage=2&orderby=new'

num = c(49, 39, 40, 38)
l = c(6, 5, 7, 6)

A = final(1, l, 1, 49)
B = final(2, l, 1, 39)
C = final(3, l, 1, 40)
D = final(4, l, 1, 38)

E = final(4, l, 2, 8)

write.csv(A, "大樂透近200期相關統計資訊_一般號碼.csv")
write.csv(B, "今彩近200期相關統計資訊_一般號碼.csv")
write.csv(C, "大福彩近200期相關統計資訊_一般號碼.csv")
write.csv(D, "威力彩近200期相關統計資訊_一般號碼.csv")
write.csv(E, "威力彩近200期相關統計資訊_特別號碼.csv")


