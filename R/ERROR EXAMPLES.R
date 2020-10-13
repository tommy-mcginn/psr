# If you enter the following code, the TE function should produce an error because Subject 3 has a Trial 4 but no Trial 3 measurement
subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
trial <- c('Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 4')
metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
metric_3 <- c(1272, 1493, 1072, 1046, 1198, 1165, 1478, 1370, 1335)
TE(subject, trial, metric_1, metric_2, metric_3)

# If you enter the following code, the TE function should produce an error because metric 1 is not entered as a numeric vector
subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
trial <- c('Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3')
metric_1 <- c("abc", "def", "ghi", "jkl", "mno", "pqr", "stu", "vwx", "yzz")
metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
metric_3 <- c(1272, 1493, 1072, 1046, 1198, 1165, 1478, 1370, 1335)
TE(subject, trial, metric_1, metric_2, metric_3)

# If you enter the following code, the TE function should produce an error because Subject 3 has duplicate Trial 2 measurements
subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
trial <- c('Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 2')
metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
metric_3 <- c(1272, 1493, 1072, 1046, 1198, 1165, 1478, 1370, 1335)
TE(subject, trial, metric_1, metric_2, metric_3)
