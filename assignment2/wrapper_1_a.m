function wrapper_1_a
    clc
    disp('Hi! This is wrapper_1_a. Guten Tag!');
    disp(' ');
    disp('The dogleg is a bit tricky, so I visualise what it does before my');
    disp('brother wrapper_1_b uses it to test your trust region code. Please');
    disp('RUN ME SEVERAL TIMES to get a good picture of what is going on.');
    disp(' ');
    disp('As discussed in the lectures, the dogleg is a hack to find a good');
    disp('approximation of the minimiser of the trust-region subproblem quickly.');
    disp('We wish to minimise a quadratic function (level sets shown) over the');
    disp('ball plotted in black. To do this, we first run along the blue line');
    disp('from the centre to the minimiser along the gradient direction, and');
    disp('then from there along the red line to the global minimizer of the');
    disp('quadratic function.');
    disp(' ');
    disp('In the subplot on the right you see the values of the quadratic func-');
    disp('tion plotted over the blue and the red line. They decrease as predicted');
    disp('by theory, so we pick the "rightmost" point that is within the black');
    disp('ball. Note that strange things can happen. For example, the blue line');
    disp('can leave the ball, and the red line can reenter and even cross it.');
    disp('Though finding the dogleg point does not require any high-level maths,');
    disp('it takes a bit of time to figure out how to do it.');
    disp(' ');
    disp('Have a look at a few random situations. Zoom in to see how good the');
    disp('dogleg point really is. Is it far from the global optimum? How does');
    disp('it compare to the Cauchy point (not shown)?');

    %generate a simple random model problem
    g=4*randn(2,1);
    success=0;
    while (success==0)
        H=randn(2);
        H=H+H';
        e=eig(H);
        if (e(1)>0&&e(2)>0)
            success=1;
        end
    end
    f=@(x)0.5*x'*H*x+g'*x;
    gf=@(x)H*x+g;
    Hf=@(x)H;
    %compute gradient search minimiser, global minimiser and dogleg point
    pmin=-H\g;
    pg=-((g'*g)/(g'*H*g))*g;
    d=myDogLeg(f,gf,Hf,[0;0],2);

    %graphical output
    ssz=6;
    figure(1)
    clf
    subplot(1,2,1)
    hold on
    [X,Y]=meshgrid([-ssz:0.1:ssz],[-ssz:0.1:ssz]);
    Z=zeros(size(X));
    for k=1:size(X,1)
        for l=1:size(X,2)
            Z(k,l)=log(f([X(k,l);Y(k,l)])-f(pmin)+0.1);
        end
    end
    contour(X,Y,Z,20)

    rectangle('Position',[-2,-2,4,4],'Curvature',1)
    plot([0,pg(1)],[0,pg(2)],'b-x');
    plot([pg(1),pmin(1)],[pg(2),pmin(2)],'r-x');
    plot(d(1),d(2),'ko')
    axis equal
    axis([-ssz,ssz,-ssz,ssz])
    title('contours of quadratic and dogleg path');
    subplot(1,2,2)
    s=[0:0.01:1];
    t=zeros(size(s));
    u=t;
    for k=1:length(s)
        t(k)=f(s(k)*pg);
        u(k)=f(pg+s(k)*(pmin-pg));
    end
    plot(s,t,'b',1+s,u,'r')
    title('function values along dogleg path');
    xlabel('s')
    ylabel('\phi_k(s)')
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
        