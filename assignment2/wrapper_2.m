disp('Hi! I am wrapper_2! How are you?');
disp(' ');
disp('My job is to visualise results your code produces. This helps you check');
disp('whether your code is correct, and sometimes you may be shown some');
disp('phenomena that help you understand what a method is actually doing');
disp('or how it performs.');
disp(' ');
disp('In this task, I will first show you rather straight-forward projections');
disp('onto randomly generated objects. In Figure 1, I generate random hyper-');
disp('planes (black) and random points (blue). I depict the projection');
disp('computed by your myProjectHyperplane function in green. The red line');
disp('should be perpendicular to the black hyperplane.');
disp(' ');

figure(1)
clf
for i=1:4
    subplot(2,2,i)
    y=randn(2,1);
    v=randn(2,1);
    v=v/norm(v);
    alpha=randn(1);
    x=myProjectHyperplane(y,v,alpha);
    hold on
    plot(y(1),y(2),'bx');    
    s=norm(x-alpha*v);
    vperp=[v(2);-v(1)];
    z1=alpha*v+1.5*s*vperp;
    z2=alpha*v-1.5*s*vperp;            
    plot([z1(1),z2(1)],[z1(2),z2(2)],'k')
    plot([x(1),y(1)],[x(2),y(2)],'-.r')
    plot(x(1),x(2),'go');
    hold off
    titlestr=sprintf('v=(%0.2f,%0.2f), alpha=%0.2f',v(1),v(2),alpha);
    title(titlestr);
    axis equal
end

disp('In Figure 2, I generate a few random boxes (black) and points (blue),');
disp('and I show its projection onto the box computed by your MyProjectBox');
disp('function. You can probably determine without further visual aids');
disp('whether your algorithm yields the correct result.');

figure(2)
clf
for i=1:4
    subplot(2,2,i)
    f=randn(2,1);
    g=randn(2,1);
    y=randn(2,1);
    a=[min(f);min(g)];
    b=[max(f);max(g)];
    x=myProjectBox(y,a,b);
    hold on
    rectangle('Position',[a(1),a(2),b(1)-a(1),b(2)-a(2)]);
    plot(y(1),y(2),'bx');    
    plot(x(1),x(2),'go');
    hold off
    titlestr=sprintf('a=(%0.2f,%0.2f), b=(%0.2f,%0.2f)',a(1),a(2),b(1),b(2));
    title(titlestr);
    axis equal
end

disp('In Figure 3, I generate random circles (black) and random points (blue)');
disp('and I connect these points with your computed projections (green) with');
disp('a red line. This line must be perpendicular when the blue point is located');
disp('outside the circle.');
disp(' ');

figure(3)
clf
for i=1:4
    subplot(2,2,i)
    z=randn(2,1);
    R=rand+0.001;
    y=randn(2,1);
    x=myProjectBall(y,z,R);
    hold on
    rectangle('Position',[z(1)-R,z(2)-R,2*R,2*R],'Curvature',1);
    plot([x(1),y(1)],[x(2),y(2)],'-.r')
    plot(y(1),y(2),'bx');    
    plot(x(1),x(2),'go');
    hold off
    titlestr=sprintf('z=(%0.2f,%0.2f), R=%0.2f',z(1),z(2),R);
    title(titlestr);
    axis equal
end

disp('Now let us do some cyclic projections in Figure 4. I will generate');
disp('a hyperplane, a box and a circle and try to find a point in their');
disp('intersection. The algorithm is still doing something reasonable when');
disp('the intersection is empty. We will not discuss this (for good reason).');
disp('Please run me several times to get a complete picture of the behaviour');
disp('of the cyclic projection algorithm. You may have to zoom in.');

figure(4)
clf
for i=1:4
    subplot(2,2,i)
    hold on

    %hyperplane data
    v=randn(2,1);
    v=v/norm(v);
    alpha=0.5;
    s=norm(x-alpha*v);
    vperp=[v(2);-v(1)];
    z1=alpha*v+max(2.5*s,2)*vperp;
    z2=alpha*v-max(2.5*s,2)*vperp;            
    plot([z1(1),z2(1)],[z1(2),z2(2)],'k');
    %box data
    f=randn(2,1);
    g=f+0.3+abs(randn(2,1));
    a=[min(f);min(g)];
    b=[max(f);max(g)];
    rectangle('Position',[a(1),a(2),b(1)-a(1),b(2)-a(2)]);
    %ball data
    z=0.3*randn(2,1);
    R=1.3;
    rectangle('Position',[z(1)-R,z(2)-R,2*R,2*R],'Curvature',1);

    y=[5;3];
    plot(y(1),y(2),'or');
    for k=1:20
        x=myProjectBall(y,z,R);
        plot([x(1),y(1)],[x(2),y(2)],'-.r');
        y=x;
        x=myProjectHyperplane(y,v,alpha);
        plot([x(1),y(1)],[x(2),y(2)],'-.r');
        y=x;
        x=myProjectBox(y,a,b);
        plot([x(1),y(1)],[x(2),y(2)],'-.r');
        y=x;
    end 
    axis equal
    title('cyclic projections with random hyperplane, box and circle');
    hold off
end