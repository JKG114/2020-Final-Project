%Binary Level Tract Census Test. Null Hypothesis: NonWhites killed
%proportional to census level demographic data
%C Demographics
%E Counts of race of deceased in county
%If you call EDA_White_BINARY_CENSUS_LEVEL
    %White is the first column, non-white is second
%If you call Black_Binary_Tract_RacesOfCounties.csv    
    %NonBlack Is column1 and Black is second
%If you call Hispanic_Binary_Tract_RacesOfCounties.csv    
    %NonHispanic Is column1 and Hispanic is second    
C=csvread("Binary_Tract_RacesOfCounties.csv",1);
E=csvread('Binary_Tract_RacesOfVictims.csv',1);

NumSamples=1000;
[NumTracts,NTypes]=size(C);

% Clean C, meaning force total in each row to be 1

CSums=sum(C,2);
for col=1:NTypes
    C(:,col)=C(:,col)./CSums;
end

% Get number of attacks in each row, and then normalize rows of E

ESums=sum(E,2);
for col=1:NTypes
    E(:,col)=E(:,col)./ESums;
end

S=sum(E(:,2)-C(:,2)); % The observed value of the statistic

% Make NumSamples surrogate E's and compute, for each, a
% surrogate S ('SS')

SS=zeros(NumSamples,1);

for samp=1:NumSamples
    
    ES=zeros(NumTracts,NTypes);  % ES will hold the current surrogate E
    
    for row=1:NumTracts
        r = mnrnd(ESums(row),C(row,:)); % Multinomial selection of 
                    % number of victims, but from the distribution in C
        
        % load the victim numbers into the surrogate, ES
        for col=1:NTypes
            ES(row,col)=r(col); 
        end
    end
    
    % Normalize the rows of ES (make them probability distributions
    ESSums=sum(ES,2); 
    for col=1:NTypes
        ES(:,col)=ES(:,col)./ESSums;
    end
    
    % Compute the L1 distance between C and the surrogate ES
    SS(samp)=sum(ES(:,2)-C(:,2));

end
    
% Display Results

fig=5;
figure(fig)
close(fig)
figure(fig)
hist(SS);
hold on
scatter(S,0,100,'filled','r')
hold off
pvalue=(sum(SS>=S)+1)/(NumSamples+1);
disp(['p-value: ',num2str(pvalue)]) 
title(['County Level Binary Hypthesis Test p-value:',num2str(pvalue)])
    
    
    
    