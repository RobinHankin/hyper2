test_that("suplist tests", {

W <- hyper2(pnames=letters[1:5])
W1 <- ggrl(W, 'a', letters[2:3],'d')  # 2-element list
W2 <- ggrl(W, 'e', letters[1:3],'d')  # 6-element list
W3 <- ggrl(W, 'c', letters[4:5],'a')  # 2-element list

W1+W2+W3

a <- lsl(list(W1,W2,W3),4:6)
loglik_lsl(equalp(W),a,log=TRUE)

} )
