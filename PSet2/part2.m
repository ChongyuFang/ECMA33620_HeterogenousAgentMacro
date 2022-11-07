% The University of Chicago
% Spring 2022
% ECMA 33620 Intro to Heterogeneous Agent Macroeconomics
% Instructor: Prof. Greg Kaplan

% Problem Set 2
% Part 2
% Chongyu Fang

clear;
close all;

%% PARAMETERS

% preferences
risk_aver   = 1.5;
beta        = 0.975;

% returns
r           = 0.02;
R = 1+ r;

% income risk: AR(1) for log income using Rouwenhorst
mu_y    = 0;
sd_y    = 0.1;
rho     = 0.97;
ny      = 5;
tau     = 0.15;
% tau   = 0.30;
T       = 0.15;
% T     = 0.30;

% asset grids
na          = 100;
amax        = 50; % amax = 100 
borrow_lim  = 0;
agrid_par   = 0.5; % 1 for linear, 0 for L-shaped; more grid points close to binding

% computation
max_iter    = 1000;
tol_iter    = 1.0e-6;
Nsim        = 50000;
Tsim        = 1000;

%% OPTIONS
Display     = 1;
DoSimulate  = 1;
MakePlots   = 1;

%% DRAW RANDOM NUMBERS
rng(2022);
yrand = rand(Nsim,Tsim);
%yrand = readtable('yrand.csv');
%yrand = yrand{:,2:1001};


%% SET UP GRIDS
% assets
agrid = linspace(0,1,na)';
agrid = agrid.^(1./agrid_par);
agrid = borrow_lim + (amax-borrow_lim).*agrid;

% income: rowenhurst
[logygrid, ytrans, ydist] = rouwenhorst(ny, -0.5*sd_y^2, sd_y, rho);
ygrid = exp(logygrid)/(exp(logygrid)'*ydist);
ycumdist = cumsum(ydist);
ycumtrans = cumsum(ytrans,2);


%% UTILITY FUNCTION
u = @(c)(c.^(1-risk_aver)-1)./(1-risk_aver);
u1 = @(c) c.^(-risk_aver);
u1inv = @(u) u.^(-1./risk_aver);


%% INITIALIZE CONSUMPTION FUNCTION
conguess = zeros(na,ny);
for iy = 1:ny
    conguess(:,iy) = r.*agrid+ygrid(iy);
end 

%% ITERATE ON EULER EQUATION WITH ENDOGENOUS GRID POINTS
con = conguess;

iter = 0;
cdiff = 1000;

while iter <= max_iter && cdiff>tol_iter
    iter = iter + 1;
    sav = zeros(na,ny);

    conlast = con;

    emuc = u1(conlast)*ytrans';
    muc1 = beta.*R.*emuc;
    con1 = u1inv(muc1);

    % loop over income
    for iy = 1:ny

        ass1(:,iy) = (con1(:,iy) + agrid -(1-tau).*ygrid(iy) - T)./R;

        % loop over current period assets
        for ia  = 1:na
            if agrid(ia)<ass1(1,iy) %borrowing constraint binds
                sav(ia,iy) = borrow_lim;

            else %borrowing constraint does not bind;
                sav(ia,iy) = lininterp1(ass1(:,iy),agrid,agrid(ia));

            end
        end
        con(:,iy) = R.*agrid + (1-tau).*ygrid(iy) + T - sav(:,iy);

    end

    cdiff = max(max(abs(con-conlast)));
    disp(['Iteration no. ' int2str(iter), ' max con fn diff is ' num2str(cdiff)]);
end 

%% GET VALUE FUNCTION
Vguess = u(con);
V = Vguess;
Vdiff = 1;
iter = 0;

while iter <= max_iter && Vdiff > tol_iter

    iter = iter + 1;
    Vsaving = zeros(na,ny);

    for iy = 1:ny
        for ia = 1:na
          
          % V is on agrid, saving is not
          % Use interpolation here
          Vsaving(ia,iy) =  lininterp1(agrid,V(:,iy),sav(ia,iy));  

        end
    end 

    savind = zeros(na,ny);
    Vchoice = zeros(na,ny);

    % loop over assets
    for ia = 1:na
        % loop over income
        for iy = 1:ny
            
            Vchoice(ia,iy) = u(con(ia,iy)) + beta.*(Vsaving(ia,:)*ytrans(iy,:)');           
       end
    end
    
    Vdiff = max(max(abs(V-Vchoice)));
    V = Vchoice;
    if Display >=1
        disp(['Iteration no. ' int2str(iter), ' max val fn diff is ' num2str(Vdiff)]);
    end
end    



%% PLOT
% consumption policy function
subplot(2,2,1);
plot(agrid,con(:,1),'b-',agrid,con(:,ny),'r-','LineWidth',1);
grid;
xlim([0 amax]);
ylim([0 4]);
title('Consumption Policy Function');
legend({'Lowest income state','Highest income state'},'Location','north');

% consumption policy function: zoomed in
% subplot(2,2,2);
% plot(agrid,con(:,1),'b-o',agrid,con(:,ny),'r-o','LineWidth',2);
% grid;
% xlim([0 2]);
% title('Consumption: Zoomed');

% savings policy function
subplot(2,2,2);
plot(agrid,sav(:,1)-agrid,'b-',agrid,sav(:,ny)-agrid,'r-','LineWidth',1);
hold on;
plot(agrid,zeros(na,1),'k','LineWidth',0.5);
hold off;
grid;
xlim([0 amax]);
title('Savings Policy Function (a''-a)');
legend({'Lowest income state','Highest income state'},'Location','north');

% savings policy function: zoomed in
% subplot(2,2,4);
% plot(agrid,sav(:,1)-agrid,'b-o',agrid,sav(:,ny)-agrid,'r-o','LineWidth',2);
% hold on;
% plot(agrid,zeros(na,1),'k','LineWidth',0.5);
% hold off;
% grid;
% xlim([0 2]);
% title('Savings: Zoomed (a''-a)');

% value function
subplot(2,2,3:4);
plot(agrid,V(:,1),'b-',agrid,V(:,ny),'r-','LineWidth',1);
hold on;
plot(agrid,zeros(na,1),'k','LineWidth',0.5);
hold off;
grid;
xlim([0 amax]);
title('Value Function');
legend({'Lowest income state','Highest income state'},'Location','south');


    
%% SIMULATE
if DoSimulate==1

    yindsim = zeros(Nsim,Tsim); % income y simulation index: # of people * # of time periods 
    asim = zeros(Nsim,Tsim); % to store assets simulated 
    
    %create interpolating function
    for iy = 1:ny
        savinterp{iy} = griddedInterpolant(agrid,sav(:,iy),'linear');
        % given (ai,yj) use saving function sav(ai,yj) to get optimal asset
        % saved to nxt period 
    end
    % initialize permanent income 
    it = 1; 
    yindsim(yrand(:,it)<= ycumdist(1),it) = 1;
    for iy = 2:ny
        yindsim(yrand(:,it)> ycumdist(iy-1) & yrand(:,it)<=ycumdist(iy),it) = iy;
    end

    %loop over time periods
    for it = 1:Tsim
        if Display >=1 && mod(it,100) ==0
            disp([' Simulating, time period ' int2str(it)]);
        end
        
        %permanent income realization: note we vectorize simulations at once because
        %of matlab, in other languages we would loop over individuals
        if it > 1
            yindsim(yrand(:,it)<= ycumtrans( yindsim(:,it-1),1),it) = 1;
            for iy = 2:ny
                yindsim(yrand(:,it)> ycumtrans(yindsim(:,it-1),iy-1) & yrand(:,it)<=ycumtrans(yindsim(:,it-1),iy),it) = iy;
            end
        end
        logysim(:,it) = logygrid(yindsim(:,it));
        
        % asset choice
        if it<Tsim
            for iy = 1:ny
                asim(yindsim(:,it)==iy,it+1) = savinterp{iy}(asim(yindsim(:,it)==iy,it));
            end
        end
    end
    
    %assign actual income values;
    ysim = ygrid(yindsim);

end

%% PLOT2
if MakePlots ==1 
    figure(2);

    % asset distribution
    subplot(2,2,1:2);
    hist(asim(:,Tsim),100);
    h = findobj(gca,'Type','patch');
    set(h,'FaceColor',[.7 .7 .7],'EdgeColor','black','LineStyle','-');
    ylabel('')
    title('Asset distribution');

    % asset distribution statistics
    aysim = asim(:,Tsim);
    disp(['Mean assets (relative to mean income) : ' num2str(mean(aysim))]);
    disp(['Fraction borrowing constrained: ' num2str(sum(aysim==borrow_lim)./Nsim * 100) '%']);
    disp(['10th Percentile: ' num2str(quantile(aysim,.1))]);
    disp(['50th Percentile: ' num2str(quantile(aysim,.5))]);
    disp(['90th Percentile: ' num2str(quantile(aysim,.9))]);
    disp(['99th Percentile: ' num2str(quantile(aysim,.99))]);

    % income distribution
    subplot(2,2,3);
    hist(ysim(:,Tsim),ygrid);
    h = findobj(gca,'Type','patch');
    set(h,'FaceColor',[0 0.5 0.5],'EdgeColor','blue','LineStyle','-');
    ylabel('')
    title('Income distribution');

    % convergence check
    subplot(2,2,4);
    plot([1:Tsim]',mean(asim,1),'k-','LineWidth',1.5);
    ylabel('Time Period');
    title('Mean Asset Convergence');

end 


%% WEALTH INEQUALITY SUMMARY STATISTICS
disp(['coefficient of variation: ' num2str(mean(aysim)/std(aysim))]);
disp(['gini coefficient: ' num2str(ginicoeff(aysim))]);
disp(['99-50 ratio: ' num2str(quantile(aysim,.99)/quantile(aysim,.5))]);
disp(['90-50 ratio: ' num2str(quantile(aysim,.90)/quantile(aysim,.5))]);
disp(['wealth share of top 10%: ' num2str(sum(aysim(aysim>=quantile(aysim,.90)))/sum(aysim))]);
disp(['wealth share of top 1%: ' num2str(sum(aysim(aysim>=quantile(aysim,.99)))/sum(aysim))]);
