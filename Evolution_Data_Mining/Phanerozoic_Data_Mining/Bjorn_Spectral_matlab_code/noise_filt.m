
%%% noise_filt.m is an example Matlab/Octave routine for comparing the
%%% effects of using a finite impulse response filter (Savitzky-Golay)
%%% with different parameter settings.

close all;clc;clear

x=rand(100,1);
fx_2 = sgolayfilt(x,2,11);    % filter the data using a 2nd order polynomial
fx_3 = sgolayfilt(x,4,11);    % filter the data using a 3rd order polynomial
fx_4 = sgolayfilt(x,6,11);    % filter the data using a 4th order polynomial
fx_5 = sgolayfilt(x,8,11);    % filter the data using a 5th order polynomial

subplot(2,2,1); plot(x,'k',fx_2,'r','LineWidth',4); title('2nd order polynomial, window size: 11')
subplot(2,2,2); plot(x,'k',fx_3,'y','LineWidth',4); title('4th order polynomial, window size: 11')
subplot(2,2,3); plot(x,'k',fx_4,'g','LineWidth',4); title('6th order polynomial, window size: 11')
subplot(2,2,4); plot(x,'k',fx_5,'b','LineWidth',4); title('8th order polynomial, window size: 11')

figure

gx_2 = sgolayfilt(x,4,5);     % filter the data using a 2nd order polynomial
gx_3 = sgolayfilt(x,4,11);    % filter the data using a 3rd order polynomial
gx_4 = sgolayfilt(x,4,17);    % filter the data using a 4th order polynomial
gx_5 = sgolayfilt(x,4,23);    % filter the data using a 5th order polynomial

subplot(2,2,1); plot(x,'k',gx_2,'r','LineWidth',4); title('4th order polynomial, window size: 5')
subplot(2,2,2); plot(x,'k',gx_3,'y','LineWidth',4); title('4th order polynomial, window size: 11')
subplot(2,2,3); plot(x,'k',gx_4,'g','LineWidth',4); title('4th order polynomial, window size: 17')
subplot(2,2,4); plot(x,'k',gx_5,'b','LineWidth',4); title('4th order polynomial, window size: 23')
