#!/bin/env julia
#
# julia version of
#   http://www.igvita.com/2007/01/15/svd-recommendation-system-in-ruby/
#
#

users = { 1 => "Ben", 2 => "Tom", 3 => "John", 4 => "Fred" }

m = [
     #Ben, Tom, John, Fred
     [5,5,0,5], # season 1
     [5,0,3,4], # season 2
     [3,4,0,3], # season 3
     [0,0,5,3], # season 4
     [5,4,4,5], # season 5
     [5,4,5,5]  # season 6
     ]

m = float(reshape(m, 4, 6))'

u, s, vt = svd(m)

u2 = u[1:end, 1:2]
v2 = vt[1:end, 1:2]
eig2 = reshape([s[1],0, 0, s[2]], 2, 2)

# new user
bob = reshape([5,5,0,0,0,5], 6, 1)
bobEmbed = bob' * u2 * inv(eig2)

# new season
s7 = reshape([4,5,4,5], 1, 4)
s7Embed = s7 * v2 * inv(eig2)


user_sim = Dict{Int64, Float64}()

for i = 1:size(v2, 1)
    user_sim[i] =
    sum((bobEmbed') .* (v2[i,1:end]')) / (norm(v2[i,1:end]) * norm(bobEmbed))
end

println(user_sim)
