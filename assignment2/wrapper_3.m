disp('Hi! Wrapper_2 here. Now we play with your projected gradient method.');
disp(' ');
disp('Let us minimise a few quadratics over the circle B_1(0) and the square');
disp('[-1,1]. The graphics should be self-explanatory. Or not? Have a look');
disp('at sufficiently many random samples to get a clear idea of what is');
disp('going on.');
disp(' ');
disp('I terminate with relative crude precision, and sometimes I simply give');
disp('up after a thousand steps. In which situations does your method struggle?');


figure(1)
clf
xvec=[-1.3:0.01:1.3];
[X,Y]=meshgrid(xvec,xvec);
Z=zeros(size(X));
for i=1:4
    subplot(2,2,i)
    hold on
    rectangle('Position',[-1,-1,2,2],'Curvature',1);
    x=randn(2,1);
    x=0.9*rand*x/norm(x);
    plot(x(1),x(2),'rx')
    Q=0.2*[3,-1;-1,2];
    c=randn(2,1); 
    f=@(z)0.5*z'*Q*z+c'*z;
    gf=@(z)Q*z+c;
    for k=1:size(X,1)
        for l=1:size(X,2)
            Z(k,l)=f([X(k,l);Y(k,l)]);
        end
    end
    contour(X,Y,Z,30);
    colorbar
    for k=1:20
        z=myGradientProjection(f,gf,@(y)myProjectBall(y,[0;0],1),x,0.5,0.5,-1,1);
        plot([x(1),z(1)],[x(2),z(2)],'-.r');
        x=z;
    end
    x=myGradientProjection(f,gf,@(y)myProjectBall(y,[0;0],1),x,0.5,0.5,0.01,1000);
    plot(x(1),x(2),'ro')
    hold off
    axis square
    drawnow
end

figure(2)
clf
for i=1:4
    subplot(2,2,i)
    hold on
    rectangle('Position',[-1,-1,2,2]);
    a=[-1;-1];
    b=[1;1];
    x=randn(2,1);
    x=myProjectBox(x,0.9*a,0.9*b);
    plot(x(1),x(2),'rx')
    Q=randn(2);
    c=randn(2,1); 
    f=@(z)0.5*z'*Q*z+c'*z;
    gf=@(z)Q*z+c;
    for k=1:size(X,1)
        for l=1:size(X,2)
            Z(k,l)=f([X(k,l);Y(k,l)]);
        end
    end
    contour(X,Y,Z,30);
    colorbar
    for k=1:20
        z=myGradientProjection(f,gf,@(y)myProjectBox(y,a,b),x,0.5,0.5,-1,1);
        plot([x(1),z(1)],[x(2),z(2)],'-.r');
        x=z;
    end
    
    x=myGradientProjection(f,gf,@(y)myProjectBox(y,a,b),x,0.5,0.5,0.1,1000);
    plot(x(1),x(2),'ro')
    hold off
    axis([-1.3,1.3,-1.3,1.3])
    axis square
    drawnow
end