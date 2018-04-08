data = read.table('app.txt', 'sep' = ',', header =TRUE)
user = unique(data$user_id)
subject = unique(data$subject_id)
uidx = match(data$user_id, user)
iidx = match(data$subject_id, subject)

M = matrix(0, length(user),  length(subject))
i = cbind(uidx, iidx)
M[i] = 1

Mmod = colSums(M^2) ^0.5
MM = M %*% diag(1/mod)
s = crossprod(MM)
R = M %*% s
R = apply(R, 1, FUN=sort, decreasing=TRUE, index.return=TRUE)
k = 5
res = lapply(R,FUN=function(r)return(subject[r$ix[1:k]]))
write.table(paste(user, res, sep=":"), file = 'result.txt', quote=FALSE, row.name=FALSE, col.names = FALSE)

