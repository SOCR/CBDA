# % of missing data
a=ifelse((is.na(pilot_data)),1,0);
100*sum(a)/(dim(pilot_data)[1]*dim(pilot_data)[2])
