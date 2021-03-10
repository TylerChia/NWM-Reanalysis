# set up the interval
ints = 1:100

# run function over the interval and add each element to a list
vec = list()
for(i in 1:length(ints)){
  vec[[i]] = recurrence_int((ints[i]))
}

# row bind the list into a dataframe
dataframe = dplyr::bind_rows(vec)

# export the dataframe
write.csv(dataframe, "recurrence_int.csv", row.names = F)
