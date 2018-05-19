%C = Demographics
%E = Race Of deceased
%Start with The NorthE_MidW
C=csvread("csvs/NonAG_RacesOfNorthE_MidW.csv",1);
E=csvread("csvs/NonAG_NorthE_MidW_RacesOfVictims.csv",1);
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

S=sum(sum(abs(E-C))); % The observed value of the statistic

% Make surrogate NumSamples surrogate E's and compute, for each, a
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
    SS(samp)=sum(sum(abs(ES-C)));

end
    
% Display Results

figure(2)
close(2)
figure(2)
subplot(2,2,1)
hist(SS);
hold on
scatter(S,0,100,'filled','r')
hold off
pvalue=(sum(SS>=S)+1)/(NumSamples+1);
title(['Non Agg NorthE MidW H-Test: p-value<=',num2str(pvalue)])

%NEW REGION
%C = Demographics
%E = Race Of deceased
%Now the South
C=csvread('csvs/NonAG_RacesOfSouth.csv',1);
E=csvread('csvs/NonAG_South_RacesOfVictims.csv',1);
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

S=sum(sum(abs(E-C))); % The observed value of the statistic

% Make surrogate NumSamples surrogate E's and compute, for each, a
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
    SS(samp)=sum(sum(abs(ES-C)));

end
    
% Display Results

subplot(2,2,2)
hist(SS);
hold on
scatter(S,0,100,'filled','r')
hold off
pvalue=(sum(SS>=S)+1)/(NumSamples+1);
title(['Non Agg South H-Test: p-value<=',num2str(pvalue)])


%NEW REGION
%Now the West
C=csvread('csvs/NonAG_RacesOfWest.csv',1);
E=csvread('csvs/NonAG_West_RacesOfVictims.csv',1);
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

S=sum(sum(abs(E-C))); % The observed value of the statistic

% Make surrogate NumSamples surrogate E's and compute, for each, a
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
    SS(samp)=sum(sum(abs(ES-C)));

end
    
% Display Results

subplot(2,2,3)
hist(SS);
hold on
scatter(S,0,100,'filled','r')
hold off
pvalue=(sum(SS>=S)+1)/(NumSamples+1);
title(['Non Agg West H-Test: p-value<=',num2str(pvalue)])


