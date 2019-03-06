
%%% dftps.m is a Matlab/Octave routine for computing the discrete-time
%%% Fouerier power spectrum for a uniformly sampled time series of
%%% diversity data. 4 plots are generated to show the impact of
%%% detrending (by a cubic polynomial) on a time series. You
%%% can easily modify this code to work on any data series.

close all;clc;clear

fname='fossil_diversity.dat'; % set the file name
data=load(fname);
t=data(:,1);
x=data(:,2);

%%% Fourier transformation without detrending
sd_x=std(x);
xn=x/sd_x;             % normalize to variance of 1
lps=2^(16);            % length (num. of elements) for power spectrum
xn_fft=fft(xn,lps);    % Fourier transform interpolated to length lps
xn_ps=xn_fft .* conj(xn_fft)/(lps);  % power spectrum is computed by 
                       % piecewise squaring of complex conjugate 
                       % divided by lps

%%% Fourier transformation with detrending
[p s]=polyfit(t,x,3);  % compute coefficients for 3rd order polynomial
x_p=polyval(p,t);      % compute values of polynomial
r=x-x_p;               % compute residual values
sd_r=std(r);           % (the rest is the same as above)
rn=r/sd_r;
rn_fft=fft(rn,lps);
rn_ps=rn_fft .* conj(rn_fft)/(lps);

%%% Generate corresponding frequency data
fmax=0.05;             % highest frequency of interest (freq=1/time)
f=[0:1:round(fmax*lps)]/lps;  % for horizontal axis compute frequencies
                       % from 0 to fmax in increments proprotional
                       % to lps. If interested in higher frequencies
                       % then increase fmax

%%% Plot data
subplot(2,2,1); plot(t,x,'k','LineWidth',4); axis([0 545 0 4.5]); text(200,3.5,'NON-DETRENDED'); xlabel('Time (millions of years ago)'); ylabel('Diversity (thousdands of genera)')
subplot(2,2,2); plot(f,xn_ps(1:round(fmax*lps)+1),'k','LineWidth',4); axis([0 0.05 0 1]); xlabel('Frequency (cycles/million years)'); ylabel('Spectral Power (normalized)'); line([(1/62) (1/62)], [0.8 1], 'Color', 'm', 'LineWidth',2); line([(1/140) (1/140)], [0.8 1], 'Color', 'm', 'LineWidth',2); text((1/135),0.9,'140Myr'); text((1/60),0.9,'62Myr')
subplot(2,2,3); plot(t,r,'g','LineWidth',4); axis([0 545 -0.9 0.9]); text(200,0.6,'DETRENDED'); xlabel('Time (millions of years ago)'); ylabel('Diversity Residuals (hundreds of genera)')
subplot(2,2,4); plot(f,rn_ps(1:round(fmax*lps)+1),'g','LineWidth',4); axis([0 0.05 0 1]); xlabel('Frequency (cycles/million years)'); ylabel('Spectral Power (normalized)'); line([(1/62) (1/62)], [0.8 1], 'Color', 'm', 'LineWidth',2); line([(1/140) (1/140)], [0.8 1], 'Color', 'm', 'LineWidth',2); text((1/135),0.9,'140Myr'); text((1/60),0.9,'62Myr')
