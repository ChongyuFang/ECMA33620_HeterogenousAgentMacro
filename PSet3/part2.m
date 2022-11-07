% The University of Chicago
% Spring 2022
% ECMA 33620 Intro to Heterogeneous Agent Macroeconomics
% Instructor: Prof. Greg Kaplan

% Problem Set 3
% Part 2
% Chongyu Fang

clear;
close all;

% preferences
risk_aver   = 2;
beta        = 0.97;
alpha       = 1./3; 
delta       = 0.2; 

%returns
% r           = -0.02;
% R = 1+ r;

% income risk: AR(1)
mu_y    = 0;
sd_y    = 0.1;
rho     = 0.97;
ny      = 5;
tau     = 0;
%  tau   = 1;
T       = 0;
%  T     = 1;

% asset gridss
na          = 40;
amax        = 50; % amax = 100 
borrow_lim  = 0;
agrid_par   = 0.5; %1 for linear, 0 for L-shaped

% computation
max_iter    = 1000;
tol_iter    = 1.0e-6;
Nsim        = 50000;
Tsim        = 1000;

Display     = 1;
DoSimulate  = 1;
MakePlots   = 1;


rng(2017);
yrand = rand(Nsim,Tsim);
%yrand = readtable('yrand.csv');
%yrand = yrand{:,2:1001};


% assets
agrid = linspace(0,1,na)';
agrid = agrid.^(1./agrid_par);
agrid = borrow_lim + (amax-borrow_lim).*agrid;

% income: rowenhurst
[logygrid, ytrans, ydist] = rouwenhorst(ny, -0.5*sd_y^2, sd_y, rho);
ygrid = exp(logygrid)/(exp(logygrid)'*ydist);
ycumdist = cumsum(ydist);
ycumtrans = cumsum(ytrans,2);

%%
ygrid = [0.4039; 0.6094; 0.9194; 1.3873; 2.0932]';
ytrans = [0.9413366 0.0573403 0.0013098 0.0000133 0.0000001;
          0.0143351 0.9419915 0.0430152 0.0006550 0.0000033;
          0.0002183 0.0286768 0.9422098 0.0286768 0.0002183;
          0.0000033 0.0006550 0.0430152 0.9419915 0.0143351;
          0.0000001 0.0000133 0.0013098 0.0573403 0.9413366];
ydist = [0.2101672; 0.1929506; 0.1937644; 0.1929506; 0.2101672]';
ycumdist = cumsum(ydist);
ycumtrans = cumsum(ytrans,2); 
%%

borrow_lim  = -0.25;
u = @(c)(c.^(1-risk_aver)-1)./(1-risk_aver);
u1 = @(c) c.^(-risk_aver);
u1inv = @(u) u.^(-1./risk_aver);

% iterate through interest rates 
r_v = linspace (-0.02,0.03,10); 
%agg_a = zeros(1,10); 
agg_a = zeros(1,10); 
for ir = 1:10
    r = r_v(ir); 
    R = 1+r; 
    conguess = zeros(na,ny);
    for iy = 1:ny
        conguess(:,iy) = r.*agrid+ygrid(iy);
    end

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

        % loop over current period ssets
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
    
    % simulation 
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
    agg_asim = sum(asim);
    agg_a(ir) = agg_asim(:,Tsim);
end 

%% Plot interest rates vs aggregate assets 
if MakePlots ==1 
    figure(1);

    plot(agg_a,r_v,'r-',agg_b,r_v,'k-','LineWidth',1.5);
    ylabel('Interest Rate');
    xlabel('Aggregate Wealth'); 
    xlim([-500000 3000000]); 
    ylim([-0.02,0.035])
    title('Aggregate Wealth as Interest Rate Varie');
    legend({'b= -0.25','b=0'},'Location','South');

end 

%% (b) kn_assets function 
alpha = 1./3;
delta = 0.2; 
kappa = [2.4;2.2;2.0;1.9;1.85;1.8;1.78;1.75;1.73;1.71;1.68;1.65;1.62;1.6]';
% kappa = [2.0;1.9;1.8;1.7;1.6;1.5;1.4;1.3]';
r_vector = alpha.*kappa.^(alpha-1)- delta; 
w_vector =(1-alpha).*kappa.^alpha; 
%% 

mean_asset = zeros(1,14);
for count = 1:14
    mean_asset(count) = kn_assets_N(kappa(count)); 
end 

% Need to rewrite kn_assets_n using ge_aiyagari.m 

% mean_asset(1) = kn_assets_N(kappa(1)); % need to divide by y; 
% mean_asset(2) = kn_assets_N(kappa(2)); 
% mean_asset(3) = kn_assets_N(kappa(3)); 
% mean_asset(4) = kn_assets_N(kappa(4)); 
% mean_asset(5) = kn_assets_N(kappa(5)); 
% mean_asset(6) = kn_assets_N(kappa(6)); 
% mean_asset(7) = kn_assets_N(kappa(7)); 
% mean_asset(8) = kn_assets_N(kappa(8)); 

%% plot 
MakePlots =1; 
if MakePlots ==1 
    figure(2);

    plot(mean_asset,r_vector,'r-',kappa,r_vector,'b-','LineWidth',1.5);
    ylabel('Interest Rate'); 
%   ylim([-0.04,0.05]);
    title('Mean savings, Capital-labor ratio and Interest rate');
    legend({'Mean savings/labor input','Capital-labor ratio(K/N)'},'Location','west');

end
