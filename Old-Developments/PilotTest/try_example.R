inputs = list(1, 2, -5 , 4, 'oops', 0, 10)

for(input in inputs) {
   print(paste("log of", input, "=", log(input)))
}

for(input in inputs) {
  try(print(paste("log of", input, "=", log(input))))
}