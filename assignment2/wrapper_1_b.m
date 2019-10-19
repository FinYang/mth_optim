function wrapper_1_b()
    clc
    disp('Hi! This is wrapper_1_b. Have you met my brother wrapper_1_a?');
    disp('Now let us run your trust-region Newton with a dogleg subsolver.');
    disp(' ');
    disp('In Figure 1, we look at the Rosenbrock function again. The red lines');
    disp('and crosses depict the trajectory of the method. The black circles');
    disp('show the current trust region. The small red circle is the dogleg');
    disp('point. Sometimes your trust-region method is happy with the dogleg');
    disp('point and moves on. Sometimes, it is unhappy, reduces the trust-region');
    disp('radius and tries again. Zoom in, and you will discover why. Guess what');
    disp('will happen in step 9! (No need to submit anything.)');
    disp(' ');

    plotf=@(x,y)(1-x).^2+100*(y-x.^2).^2;
    f=@(z)(1-z(1))^2+100*(z(2)-z(1)^2)^2;
    gf=@(z)[-2+2*z(1)-400*(z(1)*z(2)-z(1)^3);200*(z(2)-z(1)^2)];
    Hf=@(z)[2-400*(z(2)-3*z(1)^2),-400*z(1);-400*z(1),200];

    [X,Y]=meshgrid([-3:0.05:3],[-3:0.05:3]);
    Z=log(plotf(X,Y)-plotf(1,1)+0.05);

    x0=[-1.5;2];
    Delta0=1;
    steps=8;
    allx=zeros(2,steps+1);
    allD=zeros(1,steps+1);
    for k=0:steps    
        [allx(:,k+1),allD(k+1)]=myTrustregionNewton(f,gf,Hf,@myDogLeg,x0,...
            Delta0,0.5,2,0.33,0.66,0.1,-1,k);
        subplot(3,3,k+1)
        hold on
        contour(X,Y,Z,80)
        plot(allx(1,1:k+1),allx(2,1:k+1),'-xr');    
        rectangle('Position',[allx(1,k+1)-allD(k+1),allx(2,k+1)-allD(k+1),2*allD(k+1),2*allD(k+1)],'Curvature',1);
        plot(1,1,'go','MarkerFaceColor','g');
        z=allx(:,k+1)+myDogLeg(f,gf,Hf,allx(:,k+1),allD(k+1));
        plot(z(1),z(2),'ro');
        xlabel('x')
        ylabel('y')
        axis equal
        axis([-3,3,-3,3])
        s=sprintf('k=%d',k);
        title(s)
    end
    figure(1)
end

%--------------------------------------------------------------------------
% YOU ARE NOT REQUIRED TO WORK THROUGH THE FOLLOWING CODE.
%--------------------------------------------------------------------------

function d=myDogLeg(f,gf,Hf,x,Delta)
    gfx=gf(x);
    Hfx=Hf(x);
    pmin=-Hfx\gfx;    
    %global minimiser inside ball?
    if (norm(pmin)<=Delta)
        d=pmin;        
        return
    end
    pg=-((gfx'*gfx)/(gfx'*Hfx*gfx))*gfx; 
    %dirty hack: quadratic model not "sufficiently" positive definite
    if (gfx'*pg+0.5*pg'*Hfx*pg<=gfx'*pmin+0.5*pmin'*Hfx*pmin)
        fprintf('--Warning-----------------------------------------------\n');
        fprintf('- Quadratic model not positive definite.\n')
        fprintf('- Fall back to gradient step.\n\n');
        disp(x)
        disp(Hf(x))
        disp(eig(Hf(x)))
        fprintf('--------------------------------------------------------\n\n');
        d=Delta*pg/norm(pg);
        return
    end
    %examine geometry of [pg,pmin] and ball in terms of a quadratic polynomial
    a=pg'*pg-2*pg'*pmin+pmin'*pmin;
    b=-2*pg'*pg+2*pg'*pmin;
    c=pg'*pg-Delta^2;
    %Do pg and pmin coincide?
    if (a==0)
        d=Delta*pg/norm(pg);
        return
    end
    radicant=(b/(2*a))^2-c/a;
    %Does the pg-pmin leg intersect the ball?
    if (radicant>=0) 
        t=-b/(2*a)+sqrt(radicant);
        %really?
        if ((t>0)&&(t<=1))
            d=(1-t)*pg+t*pmin;  
        else
            d=Delta*pg/norm(pg);
        end             
    else
        d=Delta*pg/norm(pg);
    end
    %In all other cases, the [0,pg] leg did the job.
end
        