% [datapks] = getpks(data); get series of amplitude peaks series from amplitude series
% INPUT 
%   data, a nrow x ncol matrix
%   valuecol, select peaks of column valuecol

function [datapks] = getpks(data,valuecol)
[nrow,ncol] = size (data);  % size of data
m=1;
datapks = zeros(1,ncol);
for i=2:nrow-1
    if data(i,valuecol)>= data(i-1,valuecol)
      if  data(i,valuecol)>= data(i+1,valuecol)
          for j = 1: ncol
              datapks(m,j)=data(i,j);
          end
        m=m+1;
      end
    end
end