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
qf = @(x, d) f(x) + gf(x)'*d + 0.5*d'*Hf(x)*d;
k=0;
d = zeros([size(x), maxstep]);
pred = zeros(maxstep,1);
ared = zeros(maxstep,1);
rho = zeros(maxstep,1);

while (gf(x) ~= 0)
    if(norm(gf(x)) <= tol || k>=maxstep)
        break
    end
    k = k+1;
    d(:,:,k) = subsolve(f,gf,Hf,x,Delta);
    pred(k,1) = f(x) - qf(x,d(:,:,k));
    ared(k,1) = f(x) - f(x+d(:,:,k));
    rho(k,1) = ared(k,1)/pred(k,1);
    if(rho(k,1)>= eta1)
        x = x+d(:,:,k);
    end
    
    if(rho(k,1)< eta1)
        Delta = gamma1 * Delta;
    elseif(rho(k,1)>= eta2)
        Delta = max(DeltaMin, gamma2*Delta);
    else
        Delta = max(DeltaMin, Delta);
    end
    
    
    
end


end