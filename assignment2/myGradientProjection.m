%--------------------------------------------------------------------------
% DO NOT MODIFY THE FILE NAME!
%--------------------------------------------------------------------------
%
%--------------------------------------------------------------------------
% This function is the gradient projection method as in Algorithm 2.23.
%--------------------------------------------------------------------------
%
%--INPUTS------------------------------------------------------------------
%
% f          -     objective function
% gf         -     gradient of objective function
% projfun    -     user-supplied function that projects a point y to the 
%            -     admissible set, to be called with syntax x=projfun(y)
% x          -     initial guess
% beta       -     parameter for Armijo rule
% sigma      -     parameter for Armijo rule
% epstol     -     error tolerance 
% maxstep    -     maximal number of steps allowed to be carried out
%
%--OUTPUT------------------------------------------------------------------
%
% x          -     approximation of solution of variational inequality
% step       -     number of steps carried out
%
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
% DO NOT MODIFY THE FUNCTION DEFINITION!
%--------------------------------------------------------------------------

function [x,step]=myGradientProjection(f,gf,projfun,x,beta,sigma,epstol,maxstep)

%--------------------------------------------------------------------------
% WRITE YOUR CODE BELOW THIS LINE!
%--------------------------------------------------------------------------

        