% Blackman-Tukey spectral analysis
% code to calculate frequencies using the Blackman-Tukey method
% first set up data with three periods (100, 41, and 23) to demostrate the % method
t = [1:600];
y =  sin(2*pi*t/100) + sin(2*pi*t/41) + sin(2*pi*t/23);

% choose lag
lag = 1/3;
lagno = round(lag*length(y));

% calculate the autocorrelation function
if lag == 1;
  c = xcorr(y);
else;
  c = xcorr(y, y, lagno);
end;

% shift the function to place the zero offset point in first location of
% the vector
center = ceil(length(c)/2);
c1 = c(center:length(c))

% do the FFT and take the real part
p = real(fft(c1));
% normalize to mean of 1
p = p/std(p);

% plot the results
fNyquist = 0.5/(t(2)-t(1))
f=linspace(0, fNyquist, floor(lagno/2+1))
h = figure;
plot(f, p(1:length(f)));

% set the maximum frequency on the plot to 0.06
ax = axis;
axis([ax(1) 0.06 ax(3) ax(4)]) ;
ylabel('spectral power');
xlabel('frequency (cycles/kyr)');
print(h,'structure.pdf','-dpdf');