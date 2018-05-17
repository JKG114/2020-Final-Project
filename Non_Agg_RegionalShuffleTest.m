%Start with The NorthEast
C=csvread('RacesOfNortheast.csv',1);
E=csvread('NortheEast_RacesOfVictims.csv',1);
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

figure(1)
close(1)
figure(1)
subplot(2,2,1)
hist(SS);
hold on
scatter(S,0,100,'filled','r')
hold off
pvalue=(sum(SS>=S)+1)/(NumSamples+1);
disp(['p-value: ',num2str(pvalue)]) 
title('Northeast Hypthesis Test')
text(.35,50,['p-value <= ',num2str(pvalue)])

%NEW REGION
%Now the South
C=csvread('RacesOfSouth.csv',1);
E=csvread('South_RacesOfVictims.csv',1);
E = E';
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
disp(['p-value: ',num2str(pvalue)]) 
title('South Hypthesis Test')
text(.2,50,['p-value <= ',num2str(pvalue)])

%NEW REGION
%Now the Midwest
C=csvread('RacesOfMidwest.csv',1);
E=csvread('Midwest_RacesOfVictims.csv',1);
E = E';
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
disp(['p-value: ',num2str(pvalue)]) 
title('Midwest Hypthesis Test')
text(.25,50,['p-value <= ',num2str(pvalue)])

%NEW REGION
%Now the Mountain
C=csvread('RacesOfWest.csv',1);
E=csvread('West_RacesOfVictims.csv',1);
E = E';
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

subplot(2,2,4)
hist(SS);
hold on
scatter(S,0,100,'filled','r')
hold off
pvalue=(sum(SS>=S)+1)/(NumSamples+1);
disp(['p-value: ',num2str(pvalue)]) 
title('West Hypthesis Test')
text(.2,80,['p-value <= ',num2str(pvalue)])

