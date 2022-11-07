% The University of Chicago
% Spring 2022
% ECMA 33620 Intro to Heterogeneous Agent Macroeconomics
% Instructor: Prof. Greg Kaplan

% Problem Set 1
% Part 3
% Chongyu Fang

clear;
close all;

%% PARAMETERS

% horizon
T = 50;

% preferences
risk_aver   = 1.5;
beta        = 0.98;
Phi         = 0.5;

% returns
r           = 0.02;
%r           = 0.01;
R = 1+ r;

% wage
wage = zeros(T,1);
wage(1:25) = (1:1:25);
wage(26:50) = (25:-1:1);
wage = wage./10;
%wage = wage.*(1-0.4)./10;

% unemployment benefits
benefit = 0.5;
%benefit = 0.1;

% asset grids
na          = 1000;
% Set this with caution, if too low, wealth is bounded, consumption would be weird
amax        = 20; % upper limit of asset on asset grid
borrow_lim  = 0;
%borrow_lim  = -0.10;
agrid_par   = 1; %1 for linear, 0 for L-shaped


%% OPTIONS
Display     = 1;
DoSimulate  = 1;
MakePlots   = 1;

%% SET UP GRIDS

% assets
agrid = linspace(0,1,na)';
agrid = agrid.^(1./agrid_par);
agrid = borrow_lim + (amax-borrow_lim).*agrid;

% put explicit point at a=0
[aclose,idx] = min(abs(agrid-0)); % set the smallest grid to zero
agrid(idx) = 0;

%% UTILITY FUNCTION

if risk_aver==1
    u = @(c)log(c);
else    
    u = @(c)(c.^(1-risk_aver)-1)./(1-risk_aver);
end    

u1 = @(c) c.^(-risk_aver);

%% INITIALIZE ARRAYS
V = zeros(na,T); % Value function
con = zeros(na,T); % Consumption Policy Function c(a)
sav = zeros(na,T); % Saving Policy Function a'(a)
savind = zeros(na,T); % The index of a' on grid
h = zeros(na,T); % Optimal working decision h

%% DECISIONS AT t=T
savind(:,T) = find(agrid==0);
sav(:,T) = 0; % At t=T trivially do not save
h(:,T) = 0; % At t=T trivially do not work
con(:,T) = R.*agrid + wage(T).*h(:,T) + benefit.*(1-h(:,T)) - sav(:,T);
V(:,T) = u( max(con(:,T),1.0e-10));


%% SOlVE VALUE FUNCTION BACKWARD

for it = T-1:-1:1
    if Display >=1 
        disp(['Solving at age: ' int2str(it)]);
    end
    
    
    % loop over assets
    for ia = 1:na
        
        % optimal working decision
        cash_work = R.*agrid(ia) + wage(it);
        Vchoice_work = u(max(cash_work-agrid,1.0e-10)) - Phi + beta.*V(:,it+1);
        cash_notwork = R.*agrid(ia) + benefit;
        Vchoice_notwork = u(max(cash_notwork-agrid,1.0e-10)) + beta.*V(:,it+1);

        if max(Vchoice_work) >= max(Vchoice_notwork) % compare value between work/not work
            [V(ia,it),savind(ia,it)] = max(Vchoice_work);
            h(ia,it) = 1;
            cash = cash_work;

        else
            [V(ia,it),savind(ia,it)] = max(Vchoice_notwork);
            h(ia,it) = 0;
            cash = cash_notwork;
        end

        sav(ia,it) = agrid(savind(ia,it));
        con(ia,it) = cash - sav(ia,it);
    end
    
end


%% SIMULATE
% Since we derived c(a), a'(a), h(a), we can sumulate consumption, wealth,
% work decidon path now
if DoSimulate ==1
    aindsim = zeros(T+1,1); % asset index simulation
    
    % initial assets: uniform on [borrow_lim, amax]    
    ainitial = 0; % endowment asset a0 = 0 given
    
    % get the asset index on the grid with the asset being
    % the nearest to 0 on the grid;
    aindsim(1) = interp1(agrid,[1:na]',ainitial,'nearest');
        
    % simulate forward
    for it = 1:T
        disp([' Simulating, time period ' int2str(it)]);

        % asset choice
        aindsim(it+1) = savind(aindsim(it),it);
    end
    
    % assign actual asset, income and work values;
    asim = agrid(aindsim);
    hsim = zeros(T,1);
    y = zeros(T,1);
    for ih = 1:T
        hsim(ih) = h(aindsim(ih),ih);
        y(ih)= wage(ih).*hsim(ih) + benefit.*(1-hsim(ih));
    end
    csim = R.*asim(1:T) + y - asim(2:T+1);
end


%% MAKE PLOTS
if MakePlots ==1 
    figure(1);
    
    % consumption and income path
    subplot(1,2,1);
    plot([1:50]',y,'k-',[1:50]',csim,'r--','LineWidth',2);
    grid;
    title('Income and Consumption');
    legend('Income','Consumpion');
    ylim([0,2.5]);

    % wealth path function
    subplot(1,2,2);
    plot([0:50]',asim,'b-','LineWidth',2);
    hold on;
    plot(agrid,zeros(na,1),'k','LineWidth',0.5);
    hold off;
    grid;
    title('Wealth');

    figure(2);

    % optimal working decision path
    plot([1:50]',hsim,'-m*', 'MarkerSize',10);
    hold on; 
    plot(agrid,zeros(na,1),'m','LineWidth',0.5);
    hold off;
    grid;
    title('Working Decision');
        

end
