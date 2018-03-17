rm(list = ls())

library(rvest)

data = function(url, l) {
  
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
  main_t = gsub("[[:space:]]", "", main[-1])%>%strsplit(",") %>%unlist()%>%matrix(l, dim)%>%t()
  dataset = cbind(date[-1], main_t, special[-1])
  colnames(dataset) = c(date[1], sapply(1:dim(main_t)[2], function(x) {paste(main[1], "_", x, sep = "")}), special[1])
  return (dataset)
}

sqe = function(z, p) { s1 = 1; s2 = 0; g = {}
  
  for (i in 1 : length(z)) {
    
    if (z[i] == p) {g[s1] = s2 + 1; s2 = s2 + 1} else {s2 = 0; s1 = s1 + 1}
    
  }
 return(na.omit(g))
}

count = function(z, num) {
  z = as.numeric(z); l = matrix(NA, num, 1); l[z] = 1; l[-z] = 0
  return(l)
}

final = function(y) {
  
  dataset = rbind(data(url[y, 1], l[y]), data(url[y, 2], l[y]))
  k = l[y] + 1
  count_table = sapply(1:dim(dataset)[1], function(x) {count(dataset[x, 2:k], num[y])})%>%t()
  interval_0 = sapply(1:dim(count_table)[2], function(x) {sqe(count_table[, x], 0)})
  interval_1 = sapply(1:dim(count_table)[2], function(x) {sqe(count_table[, x], 1)})
  # median = sapply(1:num[y], function(x) {median(interval_0[[x]])})
  mean = sapply(1:num[y], function(x) {mean(interval_0[[x]])})%>%round()
  # (std = sapply(1:num[2], function(x) {var(interval[[x]])^0.5}))
  max_0 = sapply(1:num[y], function(x) {max(interval_0[[x]])})
  max_1 = sapply(1:num[y], function(x) {max(interval_1[[x]])})
  last_count = sapply(1:num[y], function(x) {interval_0[[x]][length(interval_0[[x]])]}); last_count[as.numeric(dataset[dim(dataset)[1], 2:k])] = 0
  next_show =  mean - last_count
  u = which(next_show < 0)
  if (length(u) > 0) {
    next_prob = matrix(NA, 1, num[y])
    next_prob[1, u] = sapply(1:length(u), function(x) {1 - sum(unlist(interval_0[u[x]])>last_count[u[x]])/length(interval_0[[u[x]]])})%>%round(3)
    next_prob[1, -u] = 'Non'
  } else {next_prob = rep('Non', each = length(last_count))}
  
  results = data.frame(rbind(max_0, max_1, mean, last_count, next_prob), 
                       row.names = c('最長連續未開號碼間距', '最長連續出現號碼間距', '預期開出號碼間距', '至今未開出號碼間距', '下期可能開出號碼機率'))
  colnames(results) = sapply(1:length(last_count), function(x) {paste("號碼", x, sep = "")})
  
  return(results)
} 

final_s = function(y) {
  
  dataset = rbind(data(url[y, 1], l[y]), data(url[y, 2], l[y]))
  k = l[y] + 2
  num = 8
  count_table = sapply(1:dim(dataset)[1], function(x) {count(dataset[x, k], num)})%>%t()
  interval_0 = sapply(1:dim(count_table)[2], function(x) {sqe(count_table[, x], 0)})
  interval_1 = sapply(1:dim(count_table)[2], function(x) {sqe(count_table[, x], 1)})
  # median = sapply(1:num[y], function(x) {median(interval_0[[x]])})
  mean = sapply(1:num, function(x) {mean(interval_0[[x]])})%>%round()
  # (std = sapply(1:num[2], function(x) {var(interval[[x]])^0.5}))
  max_0 = sapply(1:num, function(x) {max(interval_0[[x]])})
  max_1 = sapply(1:num, function(x) {max(interval_1[[x]])})
  last_count = sapply(1:num, function(x) {interval_0[[x]][length(interval_0[[x]])]}); last_count[as.numeric(dataset[dim(dataset)[1], k])] = 0
  next_show =  mean - last_count
  u = which(next_show < 0)
  if (length(u) > 0) {
    next_prob = matrix(NA, 8, 1)
    next_prob[u, 1] = sapply(1:length(u), function(x) {1 - sum(unlist(interval_0[u[x]])>last_count[u[x]])/length(interval_0[[u[x]]])})%>%round(3)
    next_prob[-u, 1] = 'Non'
  } else {next_prob = rep('Non', each = length(last_count))}
  
  results = data.frame(rbind(max_0, max_1, mean, last_count, next_prob%>%t()), 
                       row.names = c('最長連續未開號碼間距', '最長連續出現號碼間距', '預期開出號碼間距', '至今未開出號碼間距', '下期可能開出號碼機率'))
  colnames(results) = sapply(1:length(last_count), function(x) {paste("號碼", x, sep = "")})
  
  return(results)
  
} 

