############################################################# 
## Stat 202A - Homework 3
## Author: 
## Date : 
## Description: This script implements the lasso
#############################################################

#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to 
## double-check your work, but MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not change the working directory anywhere
## in your code. If you do, I will be unable to grade your 
## work since Python will attempt to change my working directory
## to one that does not exist.
#############################################################

import numpy as np
import sys

#####################################
## Function 1: Lasso solution path ##
#####################################

def myLasso(X, Y, lambda_all):
  
  # Find the lasso solution path for various values of 
  # the regularization parameter lambda.
  # 
  # X: Array of explanatory variables.
  # Y: Response array
  # lambda_all: Array of regularization parameters. Make sure 
  # to sort lambda_all in decreasing order for efficiency.
  #
  # Returns an array containing the lasso solution  
  # beta for each regularization parameter.
  
  n, p = X.shape

  #if(p <= 10):
  #     s = p
  #else:
  #     s = 10
  
  T = 10

  lambda_all = np.sort(lambda_all)[::-1]

  L = lambda_all.size
  
  #beta_true = np.ones((p,1))*0
  #for m in range(0,s):
  #    beta_true[m] = m+1

  beta = np.ones((p,1),float)*0
  beta_all = np.ones((p*L,1),float)*0
  beta_all = beta_all.reshape((p,L))

  R = np.copy(Y)

  ss = np.ones((p,1), float)*0

  for j in range(0,p):
      ss[j] = np.sum((X[:,j])**2)

  #err = np.ones((L,1),float)*0

  for l in range(0,L):
       mylambda = lambda_all[l]
       for t in range(0,T):
           for j in range(0,p):
               db = np.sum(R*X[:,j])/ss[j]
               b = beta[j]+db
               b = np.sign(b)*max(0, abs(b)-mylambda/ss[j])
               db = b - beta[j]
               R = R - X[:,j]*db
               beta[j] = b
       beta_all[:,l] = beta[:,0]
       #err[l] = np.sum((beta-beta_true)**2)
       

  ## Function should output the array beta_all, the 
  ## solution to the lasso regression problem for all
  ## the regularization parameters. 
  ## beta_all is p x length(lambda_all)
  return(beta_all)


#n = 2
#p = 3
#s = 2
#X = np.array([[0.5529790, -1.2592706, -0.1905513],[0.7024872, -0.3136742, -0.9590715]])
#Y = np.array([-2.0486022, 0.6229098])
#lambda_all = np.array(range(1,101))*10   
#print myLasso(X,Y,lambda_all)
