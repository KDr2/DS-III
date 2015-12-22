#
# R version of
#   http://www.igvita.com/2007/01/15/svd-recommendation-system-in-ruby/
#
#

m <- matrix(c(5,5,3,0,5,5,5,0,4,0,4,4,0,3,0,5,4,5,5,4,3,3,5,5), c(6,4))

#  m
#       Ben  Tom  John Fred
#       [,1] [,2] [,3] [,4]
# [1,]    5    5    0    5  #season 1
# [2,]    5    0    3    4  #season 2
# [3,]    3    4    0    3  #season 3
# [4,]    0    0    5    3  #season 4
# [5,]    5    4    4    5  #season 5
# [6,]    5    4    5    5  #season 6

m.svd <- svd(m) # u * d * v

u2 <- m.svd$u[, c(1, 2)]
v2 <- m.svd$v[, c(1, 2)]
eig2 <- matrix(c(m.svd$d[1], 0, 0, m.svd$d[2]), c(2, 2))

# new user
bob <- matrix(c(5,5,0,0,0,5), c(6, 1))
bobEmbed <- t(bob) %*% u2 %*% solve(eig2)

# new season
s7 <- matrix(c(4,5,4,5), c(1, 4))
s7Embed <- s7 %*% v2 %*% solve(eig2)

user_sim <- list()
for(i in seq(dim(v2)[1])){
  user_sim[[i]] = sum(t(bobEmbed) * (v2[i,])) /
    (norm(v2[i,], '2') * norm(bobEmbed, '2'))
}

print(user_sim)

# plot
plot(u2[,1], u2[,2], xlim=c(-1,0), ylim=c(-1,1), col="red", pch=8)
abline(h=0, v=0, lty=3)
text(u2[,1], u2[,2], paste("s", 1:6, sep=""), cex=0.6, pos=4, col="red")
lines(v2[,1], v2[,2], xlim=c(-1,0), ylim=c(-1,1), col="darkgreen", type="p", pch=7)
text(v2[,1], v2[,2], c("Ben","Tom","Jhon","Fred"), col="darkgreen", pos=2, cex=0.6)
lines(bobEmbed, type="p", pch=13, col="blue")
lines(s7Embed, type="p", pch=13, col="yellow")
