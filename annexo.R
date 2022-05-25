APM <- c(40,-25,60,0,-30,-12.5, -12.5, -30, 50, 30, -50)
APM <- matrix(APM, ncol = 1)
betas <- paste0("beta",1:10)
betas <- matrix(betas, ncol = 1)

players_M <- c(1,1,1,0,0,-1,-1,-1,0,0,
               1,1,1,0,0,-1,-1,0,-1,0,
               1,0,0,1,1,-1,0,0,-1,-1,
               1,0,0,1,1,0,0,-1,-1,-1,
               0,1,1,1,0,0,-1,-1,-1,0,
               0,1,1,0,1,0,-1,-1,0,-1,
               1,1,1,0,0,0,-1,0,-1,-1,
               1,1,1,0,0,-1,0,0,-1,-1,
               0,0,1,1,1,-1,0,0,-1,-1,
               1,0,1,1,0,-1,-1,-1,0,0,
               1,0,0,1,1,0,-1,-1,-1,0)

DF <- matrix(players_M, ncol = 10, byrow = TRUE)

## A=BX

a <- APM
B <- DF
B_t <- t(B)       #traspuesta

## C=DX

c <- B_t %*% a    #1-col
D <- B_t %*% B    #matrix 10:10
D_1 <- ginv(D)    #Inversa

det(D)!=0

X <- D_1 %*% c

cbind(betas, X)


## FITTING

df_APM <- cbind(a,B)
colnames(df_APM) <- c("PM", paste0("x",1:10))

df_APM <- as.data.frame(df_APM)

mod <- lm(PM ~ . , data=df_APM)
summary(mod)

