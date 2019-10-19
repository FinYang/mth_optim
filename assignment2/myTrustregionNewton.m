%--------------------------------------------------------------------------
% DO NOT MODIFY THE FILE NAME!
%--------------------------------------------------------------------------
%
%--------------------------------------------------------------------------
% This is the trust-region scheme from Algorithm 1.5.1.
%--------------------------------------------------------------------------
%
%--INPUTS------------------------------------------------------------------
%
% f          -     function handle of objective function
% gf         -     function handle of gradient of objective function
% Hf         -     function handle of Hessian of objective function
% subsolve   -     solver for the trust-region subproblem (5.1) to be
%                  called with arguments subsolve(f,gf,Hf,x,Delta)
% x          -     initial guess
% Delta      -     initial trust-region radius
% gamma1     -     parameter 
% gamma2     -     parameter 
% eta1       -     parameter 
% eta2       -     parameter 
% DeltaMin   -     parameter 
% tol        -     terminate when norm of gradient smaller or equal tol
% maxstep    -     terminate when maxstep steps have been carried out
%
%--OUTPUT------------------------------------------------------------------
%
% x          -     approximation of a critical point
% Delta      -     current trust-region radius
% k          -     number of iterations carried out
%
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
% DO NOT MODIFY THE FUNCTION DEFINITION!
%--------------------------------------------------------------------------

function [x,Delta,k]=myTrustregionNewton(f,gf,Hf,subsolve,x,Delta,...
    gamma1,gamma2,eta1,eta2,DeltaMin,tol,maxstep)
 
%--------------------------------------------------------------------------
% WRITE YOUR CODE BELOW THIS LINE!
%--------------------------------------------------------------------------
