%--------------------------------------------------------------------------
% DO NOT MODIFY THE FILE NAME!
%--------------------------------------------------------------------------
%
%--------------------------------------------------------------------------
% This function is projecting a point onto a box.
%--------------------------------------------------------------------------
%
%--INPUTS------------------------------------------------------------------
%
% y          -     point
% a          -     lower limits of box
% b          -     upper limits of box
%
%--OUTPUT------------------------------------------------------------------
%
% x          -     projection of y to box [a,b]
%
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
% DO NOT MODIFY THE FUNCTION DEFINITION!
%--------------------------------------------------------------------------

function x=myProjectBox(y,a,b)

%--------------------------------------------------------------------------
% WRITE YOUR CODE BELOW THIS LINE!
%--------------------------------------------------------------------------

x = min(max(y, a), b);

end