function [harmpowpks] = harmpowtry(powratioresample)

% Calculate deharm results, get 2-row period-amplitude-phase datasets with top-2 amplitude values
% This script applies 3-iternary searching for the dominated 2 periods (and phase?
%    with maxima amplitude and the second maximum amplitude
%       
% Calls for
%   harmpow.m
% OUTPUT
%   harmpowpks
%% Iternary #1: searches for maximum and 2nd maximum amplitude peak
% estimate amplitude for period from 40 kyr to 4000 kyr with a step of 10 kyr
step = 10;  % The first step for harmpow
            % The second step for harmpow will be step/100 (Line 31)
            % The third setp for harmpow will be step/1000 (Line 36)
[harmpow1] = harmpow(powratioresample,40,4000,step);

% get peaks for the 1st round estimation of peaks
[harmpowpks] = getpks(harmpow1,2);   % get ALL peaks of amplitude curves
    
    harmamppks = harmpowpks(:,2);   % sort amplitude in ascending order
    harmamppkssort = sort(harmamppks);
    ampmax = harmamppkssort(length(harmamppks));    % maximum amplitude
    ampsec = harmamppkssort(length(harmamppks)-1);    % 2nd maximum amplitude
    location_ampmax = find (harmamppks == ampmax);  % find location for ampmax
    location_ampsec = find (harmamppks == ampsec);  % find location for ampsec
    period_ampmax = harmpowpks(location_ampmax,1);  % Read period with maximum amplitude
    period_ampsec = harmpowpks(location_ampsec,1);  % Read period with 2nd maximum amplitude
    
%% Iternary #2: searches for maximum amplitude peak
[harmpow2] = harmpow(powratioresample,period_ampmax-step,period_ampmax+step,step/100);
clear harmpowpks
[harmpowpks] = getpks(harmpow2,2);   % get peaks of refined amplitude curves

%% Iternary #3: searches for maximum amplitude peak
[harmpow3] = harmpow(powratioresample,harmpowpks-step/100,harmpowpks+step/100,step/1000);
clear harmpowpks
[harmpowpks(1,:)] = getpks(harmpow3,2); % get peaks of refined amplitude curves

%%
figure; 
subplot(3,2,1),plot(harmpow1(:,1),harmpow1(:,2))
hold on; scatter(harmpowpks(1,1), harmpowpks(1,2),'r')
       xlabel('Period ( kyr )')
       ylabel('Amplitude')
       title('Maximum amplitude')
subplot(3,2,3),plot(harmpow2(:,1),harmpow2(:,2))
hold on; scatter(harmpowpks(1,1), harmpowpks(1,2),'r')
       xlabel('Period ( kyr )')
       ylabel('Amplitude')
subplot(3,2,5),plot(harmpow3(:,1),harmpow3(:,2)); 
hold on; scatter(harmpowpks(1,1), harmpowpks(1,2),'r')
       xlabel('Period ( kyr )')
       ylabel('Amplitude')       

%% Iternary #2: searches for 2nd maximum amplitude peak
%calculate deharm results
[harmpow5] = harmpow(powratioresample,period_ampsec-step,period_ampsec+step,step/100);
[harmpowpks2] = getpks(harmpow5,2);   % get peaks of amplitude curves

%% Iternary #3: searches for 2nd maximum amplitude peak
[harmpow6] = harmpow(powratioresample,harmpowpks2(1,1)-step/100,harmpowpks2(1,1)+step/100,step/1000);
clear harmpowpks2
[harmpowpks(2,:)] = getpks(harmpow6,2);
%%
subplot(3,2,2),plot(harmpow1(:,1),harmpow1(:,2))
hold on; scatter(harmpowpks(2,1), harmpowpks(2,2),'g')
       xlabel('Period ( kyr )')
       ylabel('Amplitude')
       title('Second maximum amplitude')
subplot(3,2,4),plot(harmpow5(:,1),harmpow5(:,2))
hold on; scatter(harmpowpks(2,1), harmpowpks(2,2),'g')
       xlabel('Period ( kyr )')
       ylabel('Amplitude')
subplot(3,2,6),plot(harmpow6(:,1),harmpow6(:,2)); 
hold on; scatter(harmpowpks(2,1), harmpowpks(2,2),'g')
       xlabel('Period ( kyr )')
       ylabel('Amplitude')